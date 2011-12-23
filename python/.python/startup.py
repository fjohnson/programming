verbose = True #Verbose import reporting

if verbose: print 'Importing sys'
import sys

imodules = ['os','subprocess','re','glob',
            'readline']

for e in imodules:
    if verbose: print 'Importing %s' % e
    setattr(sys.modules[__name__],e, __import__(e))

#Import all the scripts I have written in ../
#This is just a temp solution until i create a proper package.
this_script_parent_dir = os.path.dirname(os.path.expanduser(__file__))
parent_of_parentdir = os.path.dirname(this_script_parent_dir)
oldpath = os.path.abspath(os.curdir)

os.chdir(parent_of_parentdir)
#for pfile in glob.iglob(os.path.join(parent_of_parentdir, '*.py')):
for pfile in glob.iglob('*.py'):
    modname = pfile[:-len('.py')]
    if verbose: print 'Importing %s' % modname
    setattr(sys.modules[__name__],modname, __import__(modname))
os.chdir(oldpath)

#clean up variable declarations. these shouldn't exist upon python startup.
#by extension if modules were loaded with these names they are unfortunately
#cleared from the namespace. 
del modname 
del pfile
del this_script_parent_dir
del parent_of_parentdir
del oldpath

try:
    import readline
except ImportError:
    print "Module readline not available."
else:
    import rlcompleter
    readline.parse_and_bind("tab: complete")
