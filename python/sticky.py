import re
import sys
import os

'''This module presents an interface for a CLI stickypad.
Useful if you want to add some arbitrary text for later retrieval.
Consider it a visual clipboard if that makes you happy :)
Note: Entries with # are not supported. 
'''
usage = "usage: sticky [-c [index] | [[-a] text] " 
 
def fatal(exception):
    '''Fatal error. Exception!'''
    print 'Exception: ' + exception
    sys.exit(1)
    
def err(ecode):
    '''Crash due to bad user input at the CLI'''
    errs = {'E01':'Missing text to add',
            'E02':'Bad switch',
            'E03':'Too many args',
            }
    print usage
    print "error: %s" % errs[ecode]
    sys.exit(1)

def getHistoryFile(mode):
    '''Return an open file object for the sticky history file'''
    try:
        return open(os.path.expanduser("~/.sticky"),mode)
    except IOError as e:
        fatal(str(e))
    
def add(text):
    '''Add a sticky entry'''
    stickytab = getHistoryFile('a')
    stickytab.write("#%s#\n"%text)
    try:stickytab.close()
    except IOError as e: fatal(str(e))

def getStickies():
    '''Retreive the stickies in the sticky file. Return an iterator.'''
    if not os.path.exists(os.path.expanduser("~/.sticky")):
        return None
    
    stickytab = getHistoryFile("r")
    matches = re.finditer("#(.*?)#", stickytab.read(), re.DOTALL)
    try:stickytab.close()
    except IOError as e: fatal(str(e))
    return matches

def list(target=None):
    '''List all entries or only entries containing text `target\''''
    matches = getStickies()
    if not matches: return
    
    if target:
        i = 0 
        for m in matches:
            if target in m.group(1):
                print "[%d] %s" % (i,m.group(1))
                i += 1
    else:
        for i,e in enumerate(matches):
            print "[%s] %s" % (i,e.group(1))

def clear(index=None):
    '''Erase all sticky entries or a specific one @ index'''
    stickyfile = os.path.expanduser("~/.sticky")
    
    if index == None:
        try:
            if os.path.exists(stickyfile):
                os.unlink(stickyfile)
                return
        except Exception as e:
            fatal(str(e))
    
    stickies = getStickies()
    clear()
    stickyFile = getHistoryFile("w")
    
    try:
        for i,e in enumerate(stickies):
            if i == index: continue
            stickyFile.write("#%s#\n"%e.group(1))
        stickyFile.close()
    except Exception as e:
        fatal(str(e))
    
def main():
    index = None #Initialize clear index variable.
    
    if len(sys.argv) == 3:
        if sys.argv[1] == "-a":
            mode = "add"
            text = sys.argv[2]
        elif sys.argv[1] == "-c":
            mode = "clear"
            try: index = int(sys.argv[2]) #clear input index number.
            except ValueError as v: fatal(str(v))
        else: err('E02')
        
    elif len(sys.argv) == 2:
        if sys.argv[1] == "-c": mode = "clear"
        elif sys.argv[1] == '-h':
            print usage
            sys.exit(1)
        else:
            mode = "search"
            text = sys.argv[1]
    elif len(sys.argv) == 1:
        mode = "list"
    else:
        err('E03')
    
    if mode == "add": add(text)
    elif mode == "list": list()
    elif mode == "search": list(text)
    elif mode == "clear": clear(index)
    else: assert False, "Programming error"   

if __name__ == '__main__': main() 