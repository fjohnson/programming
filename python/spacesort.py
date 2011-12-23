#!/usr/bin/python2
import os
import sys
import re

'''
  author: Fletcher Johnson
  email: flt.johnson@gmail.com
  date: Dec 21, 2010
  du does not order the output of files according to their size.
  This script does this. E.g du -h . | ./spacesort.py > out 
'''

def ncmp(x,y):
    ''' Given two tuples of (size,filename) first compare
    the size element of the tuples recursively and if the
    sizes match compare the filenames, ordering by length
    and then lexigraphic order. '''

    if type(x) == type(()):
        # x = ('1.85G','file')
        size_x,fname_x = x
        size_y,fname_y = y
        ret = ncmp(size_x,size_y)

        if ret == 0: #sizes are the same
            ret = cmp(len(fname_y),len(fname_x)) #compare name lengths
            if ret == 0: #if equal, compare lexographic ordering
                ret = cmp(fname_y,fname_x)
        return ret

    #sort has recursed down to matching the sizes
    xmatch = re.match('^\d+\.?\d*\D\D?$',x)
    ymatch = re.match('^\d+\.?\d*\D\D?$',y)

    if not xmatch and not ymatch:
        return 0 #both 0
    elif xmatch and not ymatch: #y is 0
        return 1
    elif not xmatch and ymatch: #x is 0
        return -1
    
    #both x and y should look like something like 1.45G 
    sizes = ['kB','K','MB','M','G','T','P','E','Z','Y']
  
    ender = lambda x : x.endswith('kB') and 'K'\
        or x.endswith('MB') and 'M'\
        or x[-1]

    ret = cmp(sizes.index(ender(x)),sizes.index(ender(y)))
    if ret != 0: return ret 
    
    parse = lambda v : ( (v.endswith('kB') or v.endswith('MB') )\
                         and float(v[:-2])) or float(v[:-1])
    return cmp(parse(x),parse(y))

def so_rec_helper(sizes,map,file_tuple):
    file,index_into_sizes = file_tuple
    size_of_file,name_of_file = file
    sizes[index_into_sizes] = None

    try:
        sys.stdout.write(size_of_file + '\t' + name_of_file)
    except IOError: #could happen if writing output to a pipe and it closes
        sys.exit(0) #this happens when piping output to 'head' for instance

    if name_of_file in map:
        for children_of_this_file in map[name_of_file]:
            so_rec_helper(sizes,map,children_of_this_file)

def showorder(map,sizes):
    for i in xrange(len(sizes)):
        file = sizes[i]
        if file == None: 
            continue
        #print out this file and all files underneath it.
        so_rec_helper(sizes,map,(file,i))
        
        
def map_dirname_to_children(sizes):
    '''This function takes in a sorted list of files according to size and lex 
    ordering and then reorganizes this list so that these files are listed as
    per the directory tree they originated in. This ultimately leads to a clearer
    view of where the largest files are on disk. Instead of simply listing the
    largest files on disk, this allows us to first list the directory where the
    largest files reside and then recursively list the files in that directory 
    from greatest to least.'''

    map = {}
    index_into_size = 0

    for file in sizes:

        #May 14th 2011, file[1] is the filename rather than the size?
        base = os.path.dirname(file[1]) + '\n' #all files end in newlines
        
        if not base in map:
            map[base] = [(file,index_into_size)]
        else:
            map[base].append((file,index_into_size)) #next smallest added
        index_into_size = index_into_size + 1
    return map
        
def readin():
    '''Read in input from du'''

    sizes = []
    line = sys.stdin.readline()

    while( line ):
        k,v=line.split(None,1) #Split at most once using whitespace as delim.
        sizes.append((k,v))
        line = sys.stdin.readline()
        
    sizes.sort(ncmp,reverse=True)

    #output the root node to prevent an infinite loop.
    #it is the only file where its dirname is the same as itself.
    k,v = sizes[0]
    if v == '/\n':
        sys.stdout.write(k + '\t' + v)
        sizes = sizes[1:]
    return sizes

def printout():
    '''Print out files sorted by size (greatest to least)'''
    sizes = readin() #Order input by size then lex order
    map = map_dirname_to_children(sizes)
    showorder(map,sizes)

def test():
    s = [('10G','/h/f\n'),
     ('10G','/h/f/wunderkind\n'),
     ('0','/h/alice')]

    # s = [
    #     ('5G','/home/fletcher/sup\n'),
    #     ('10G','/home/fletcher/swivel\n'),
    #     ('2G','/home/fletcher/sup/ab\n'),
    #     ('3G','/home/fletcher/sup/cd\n'),
    #     ('4G','/home/fletcher/swivel/next\n'),
    #     ('6G','/home/fletcher/swivel/one\n'),
    #     ('15G','/home/fletcher/\n'),
    #     ('20G','/home/fl\n'),
    #     ('150G','/\n')]

    s.sort(ncmp,reverse=True)

    print s
    map = map_dirname_to_children(s)
    print map
    showorder(map,s)

if __name__ == '__main__':
    printout()    
#test()

