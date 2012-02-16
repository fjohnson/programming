import os
import sys
import re
import xml.dom.minidom
import xml.parsers.expat
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

def getXmlRoot(xmlfile):
    '''Open `xmlfile' and return a DOM. False if parsing fails.'''
    try:
        param_xml = xml.dom.minidom.parse(xmlfile)
    except xml.parsers.expat.ExpatError as e:
        sys.stderr.write("ExpatError while parsing line: %d col: %d%s" 
                          % (e.lineno, e.offset, os.linesep))
        sys.stderr.write(xml.parsers.expat.ErrorString(e.code) + os.linesep)
        sys.stderr.write("The %s file is not valid XML.%n" + (xmlfile,os.linesep))
        return False
    except: 
        #The documentation available does not actually say that xml.dom.minidom.parse
        #will use the Expat parser. Thus it is possible that another parser may be used
        #in the future and so I am accounting for that here. I got an Expat error when
        #attempting to parse an empty xml file.
        sys.stderr.write("Unknown exception. Error while parsing %s" %xmlfile)
        sys.stderr.write(os.linesep)
        raise #reraise caught exception. parsing failed.
    
    #If there is no document element (root element) the parser should
    #have thrown an error. I was getting an ExpatError when I was
    #trying to parse a blank document with xml.dom.minidom.parse
    #For other parsers... I don't know.
    
    childNodes = param_xml.documentElement.childNodes
    
    #The root element of the parameters xml file did not contain
    #any elements. Strange.
    if not childNodes:
        return False 
    return param_xml

class xmlTextNodeParseException(Exception): pass
class noChildNodesException(xmlTextNodeParseException): pass

def getXmlNodeText(node):
    '''Extract the text out of a node element.
    A node element looks something like this: 
    <tvb:bgthresh>100</tvb:bgthresh>
    It is assumed it will not contain any xml mark up and therefore should 
    only contain a single xml.dom.Node.TEXT_NODE object.
    
    Input: node -> An ELEMENT_NODE
    Output: the .nodeValue of node's single TEXT_NODE stripped of white space.
    '''
    
    #Node must be an element type node.
    if node.nodeType != node.ELEMENT_NODE: 
        msg = "Input node type does not match node.ELEMENT_NODE"
        raise xmlTextNodeParseException(msg)
    
    #No child nodes means its an empty element. Bad.
    if not node.childNodes: 
        msg = "Input node contains no child nodes. Expecting a TEXT_NODE."
        raise noChildNodesException(msg)
    
    #balk if there is more than one text node. see assumption above in doc string.
    textnodes = filter(lambda n : n.nodeType == n.TEXT_NODE, node.childNodes)
    
    if len(textnodes) > 1:
        
        #Dumb error message but... 
        msg = ("While I was parsing <%s> I encountered two " % node.tagName + 
        " or more separate sections of text. An xml element " + 
        "is used to separate text. " + os.linesep + 
        "Did you insert one into the parameters by mistake?" + 
        os.linesep)
        raise xmlTextNodeParseException(msg) 
    return textnodes[0].nodeValue.strip()

    


