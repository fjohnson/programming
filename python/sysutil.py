import os
import sys
import re
from subprocess import call, Popen, PIPE
from fcntl import flock, LOCK_EX, LOCK_UN

'''Module defining various functions of convenience'''

def bcall(callstring):
    '''This lets me make the equivalent call(['ls', '-l']) like
    bcall('ls -l'). Better call. Returns errorcode.'''
    return ecall(*(callstring.split()))

def ecall(*args, **kwargs):
    '''Open a process, redirecting its std streams to this processes
    This is just like subprocess.call but I dont have to pass the
    arguments like this call(['ls', '-l']) but rather ecall('ls','-l'). Returns errcode.
    Easy call.'''
    kwargs['stdout'] = sys.stdout
    kwargs['stderr'] = sys.stderr
    return eproc(*args, **kwargs)[2]

def bproc(callstring):
    '''Convenience method for calling eproc where we want to return
    the stdout,stderr. Invocation looks like bproc('ls -l') instead of
    the longer eproc('ls', '-l'). Better proc.'''
    return eproc(*(callstring.split()))

def eproc(*args,**kwargs):
    '''Open a process, returning in a tuple the stdout,stderr,retcode.
    Easy proc.'''
    assert len(args) > 0

    try: stdout = kwargs['stdout']
    except KeyError: stdout = PIPE
    try: stderr = kwargs['stderr']
    except KeyError: stderr = PIPE

    try: proc = Popen(list(args),
                      stdout=stdout,
                      stderr=stderr)
    except OSError as e:
        import errno
        
        #Only handle No such file or directory error
        if e.errno != errno.ENOENT:
            raise e
                
        if not 'PATH' in os.environ:
            msg = 'Could not find executable %s. PATH is not set.' % args[0]
            sys.stderr.write(msg + os.linesep)
            raise e
        msg = 'Could not find executable %s.' % args[0]
        msg2 = os.linesep + 'PATH is %s' % os.environ['PATH']
        sys.stderr.write(msg + msg2 + os.linesep)
        raise e


    #ret will be a tuple (None,None) if stdout!=PIPE and stderr!=PIPE
    ret = proc.communicate()
    return ret + (proc.wait(),)
    
def curlcall(url, method='GET', stdout=PIPE, stderr=PIPE):
    '''Make a call to curl for "url"'''
    return eproc("curl", '-k', '-u', 'X:X',
                 '-X',method, url, stdout=stdout,stderr=stderr)

def getItemURL(url,item):
    '''Find all instances of "item" at "url". 
    Valid items are subjects/experiments/reconstructions'''

    itemdict = {'subjects': '"/data/subjects/(.+?)"',
                'experiments': '<xnat:experiment ID="(.+?)"',
                'reconstructions':'<xnat:reconstructedImage ID="(.+?)">'}
    
    stdout,stderr,retcode = curlcall(url)
    #print stdout
    return re.findall(itemdict[item], stdout)

def urljoin(base,rest,delimiter=None):
    '''Join two url segments together optionally separated by delimiter.
    So if base was http://google.com and rest was news then the result
    would be http://google.com/news. This function returns a 'urljoined'
    object that allows chaining so you can say something like:

    urljoin("http://google.com","news").
    urljoin("forensics").
    urljoin("crypt","necessary")
    -> http://google.com/news/forensics/necessary/crypt
    '''

    #class to allow chaining of results. see deleteTargetedRecon function.
    class urljoined:
        def __init__(self,string): 
            self.string=string
            self.urljoin = lambda rest,delimiter=None: urljoin(string, rest,delimiter)

        def __add__(self,other): return self.string+other
        def __radd__(self,other): return other + self.string
        def __str__(self): return self.string
        def __repr__(self): return self.string

    if not delimiter: return urljoined(base + '/' + rest)
    return urljoined(base + '/%s/'%delimiter + rest)

def deleteTargetedRecon(base,project,subject,experiment,recon):
    '''Delete a specific reconstruction. 
    "base" is the url of the XNAT instance.'''

    target = (urljoin(base,'data')
            .urljoin('archive')
            .urljoin(project,'projects')
            .urljoin(subject,'subjects')
            .urljoin(experiment,'experiments')
            .urljoin(recon,'reconstructions'))
    print 'url: ' + target
    so,er,ret = curlcall(`target`,"DELETE")
    print 'stdout:'
    print so
    print 'stderr:'
    print er
    print 'retcode: ' + `ret`
    
def deleteAllReconsSubject(surl):
    '''Delete all reconstructions for all subjects'''
    subject_urls=map(lambda subject: urljoin(surl,subject), 
                     getItemURL(surl, 'subjects'))

    exp_urls=[]
    for surl in subject_urls:
        exps = getItemURL(surl+'?format=xml','experiments')
        for exp in exps: 
            exp_urls.append(urljoin(surl,exp,'experiments'))
        
    for eurl in exp_urls:
        recons = getItemURL(eurl+'?format=xml','reconstructions')
        for recon in recons:
            print recon
            target = urljoin(eurl,recon,'reconstructions')
            stdout,stderr,ret = curlcall(target,"DELETE")
            #print target,'\t...deleting'
            if ret:
                print stderr
                assert False, 'xnat error.'

def getConvertLock():
    '''Create a lock file so conversions are done one at a time.

    The version of dcm2nii I am currently using attempts to read 
    the provided configuration file by first placing an exclusive
    lock on it. Unfortunately it also specifies the LOCK_NB flag
    when attempting to grab a lock, so if a lock already exists
    (a very likely scenario when 30 copies of dcm2nii are running
    simulatenously) it will receive an error and quit. 

    I was thinking of placing a lock on the configuration file first
    before calling dcm2nii and allowing dcm2nii to inheirit the lock
    but it looks like this is a bit flaky. One option would be to fork
    a new process and get that process to open an exclusive lock on
    the configuration file with flock(), it would then do an execv()
    to turn itself into dcm2nii. But this is a bit flaky because
    flock() is sometimes emulated by fcntl() and I only have
    documentation that flock() locks are carried over in an execve()
    call. The docs for fork() explicitly say "File locks set by the
    parent process are not inherited by the child process."

    The easiest way is just to create a lock file that all copies
    of dicom2analyze.py will share to ensure conversions are only
    done one at a time. I.e a _blocking_ exclusive lock on a new file.
    '''

    CONVERSION_LOCK_PATH=os.path.expanduser("~/.dcm2nii/con_lock")
    lock = open(CONVERSION_LOCK_PATH, 'w+')
    lock.write(str(os.getpid()))
    flock(lock,LOCK_EX)
    return lock #returns locked file

def releaseConvertLock(lock):
    '''Release a file "lock" locked with getConvertLock()'''
    flock(lock,LOCK_UN)

    

#surl='https://ninja:8181/nrg/data/archive/projects/InitialTVB/subjects'
#deleteAllReconsSubject(surl)
#deleteTargetedRecon('https://ninja:8181/nrg','InitialTVB',
#                     'NRG_S00006','NRG_E00007',
#                     'fibertracking_analysis_DTI-4_T1-8_PDT2-7')
