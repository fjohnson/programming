'''A script to backup the valuable mkl work. Backups up the mkl files as
mkl.backup.tgz, mkl.backup.1.tgz, ... , mkl.backup.n.tgz
March 1st, 2011, 9:04pm
'''

import os
import os.path
import subprocess
import re

homedir = os.path.expanduser("~") + os.sep
version = 1
version_pattern = "mkl\.backup\.(?P<version>[0-9]+)\.tgz"
prog = re.compile(version_pattern)
output_name = 'mkl.backup'
file_backup_list = [homedir + 'java/jni/mklfuncs',
                    homedir + 'java/plsnpairs/pls/test/MklMatrixTest.java',
                    homedir + 'java/plsnpairs/pls/test/MklDoubleMatrix.java',
                    homedir + 'java/plsnpairs/makefile']
                    
def version_sort(ver1, ver2):
    '''Sort by backup version'''

    #Guaranteed to match since we are using this on a list of files
    #that has already been determined to match the regular expression.

    ver1_v = int(prog.match(ver1).group('version'))
    ver2_v = int(prog.match(ver2).group('version'))
    
    if ver1_v < ver2_v: return -1
    if ver1_v > ver2_v: return 1
    return 0


backups = filter(prog.match, os.listdir(homedir))
if backups:
    backups.sort(cmp=version_sort, reverse=True)
    latest_backup = backups[0]
    version = int(latest_backup.split('.')[2])
    version += 1
    version = '.'+str(version)
    
elif os.path.exists(homedir + "mkl.backup.tgz"): 
    version = '.1'
else:
    version = ''

print "Creating backup %s" % (output_name + version + '.tgz')
subprocess.Popen(['/bin/tar','czvf',
                  homedir + output_name + version + '.tgz'] + file_backup_list
                      ).wait()
