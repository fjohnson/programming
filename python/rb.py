#!/usr/bin/python
'''Ratcliff/Obershelp algorithm.
http://drdobbs.com/database/184407970
http://xlinux.nist.gov/dads/HTML/ratcliffObershelp.html
'''

import sys

def rb(word1, word2):

    w1l = len(word1)
    w2l = len(word2)

    maxc = 0
    
    i1 = 0
    while i1 < w1l - maxc :
        i2 = 0
        while i2 < w2l - maxc:
        
            c1,c2 = i1,i2
            common_chars = 0

            while word1[c1] == word2[c2]:
                c1 += 1
                c2 += 1
                common_chars += 1
                if c1 == w1l or c2 == w2l: break

            if common_chars > maxc: 
                s1,s2 = i1,i2
                maxc = common_chars

            i2 += 1
        i1 += 1

    if not maxc: return maxc
    return (maxc 
            + rb(word1[0:s1], word2[0:s2]) 
            + rb(word1[s1+maxc:w1l], word2[s2+maxc:w2l]))

def rb_help(word1, word2):
    print 'inspecting words %s %s' % (word1,word2)
    match = (rb(word1, word2) * 2.0) / (len(word1) + len(word2))
    print '%f match' % (match * 100)

if __name__ == '__main__':
    rb_help(sys.argv[1], sys.argv[2])
            
        
