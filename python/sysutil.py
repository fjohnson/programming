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

def getFileExclusiveLock(fileobj):
    '''Place an exclusive lock on the passed in file. Block if an exclusive
    lock already exists'''
    flock(fileobj,LOCK_EX)

def releaseFileLock(lock):
    '''Release a file "lock" locked with getFileExclusiveLock()'''
    flock(lock,LOCK_UN)





    


