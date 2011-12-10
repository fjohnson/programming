# coding=utf-8
import re
import sys
import os 
import codecs

DICT='/usr/share/dict/words'

#Don't split punctuation.
APOSTROPHES = u"’'"

'''
This pattern will separate contiguous sequences of alphanumeric
characters. Sequences are split not only on spaces but also on special
characters such as "{}'();. Practically what this means is that
punctuation is stripped off and the words are successfully extracted.

There are two exceptions.

The ' character is special because it can be both a quotation mark and
a quote character. We want to strip off quotation characters so we
don't parse out words with quote symbols attached, but at the same
time words can end on ' such as "sus' cabbages" or "didn't".  This
gives us certain ambiguities e.g 'the mad hatter's name was sus' sus'
was an mad man'.  Without knowledge of the english language difficult
to determine where quotes end and apostrophes begin.

This pattern will allow me to not split on whatever is defined as
apostrophes if the word has alphanumeric characters trailing after the
apostrophe. It allows me to extract words that end in an apostrophe
by removing the trailing apostrophe and for words such as "didn't"
I can keep the apostrophe.

Hyphenated words are also captured. A hyphen does not split words up.
This is so phrases such as "co-dependent", "anti-crime", or line split
words such as "amer-
ican" can be processed. 
'''

WORD_PAT = re.compile(
u"""
\w+-\w+(?:-\w+)*(?:[%s]\w+)?| #capture hypenated words. May include apostrophe.
\w+[%s]?\w+| #capture words of form word then apostrophe then word
\w #capture a single letter
""" % (APOSTROPHES,APOSTROPHES),
re.UNICODE | re.VERBOSE)

def processHyphenatedToken(ht,firstIdx,wordlist):
  '''Process hyphenated tokens. Return true if the token is recognized.

  Hyphenated tokens come in four cases:

  1) When there are two or more tokens separated by hypens then 
  the hyphenated token represents a conglomeration of words or letters.
  An example would be a-b-c's or anti-flaming-axe-league.

  When there is only one hyphen...
  2) It may be a word followed by a suffix or a prefixed word.
  An example would be co-dependent. 
  3) It may be that the hypen is used as a line break for page formatting.
  An example would be he-dge where 'he-' were the last three characters on
  a line, followed by 'dge' on the next line.
  4) It may be that it is two words or characters separated by a hypen.
  This is similar to case 1). An example: Pro-Keyboardist.

  The hyphenated token is recognized if it each hyphen separated token 
  can be found in a dictionary, the hyphenated token can be found in a 
  dictionary, or on removal of a hyphen in a single hyphenated token the
  result yields a word found in a dictionary.
  '''

  hidx = ht.find('-',firstIdx+1) #check for more than one token.
  
  #Case 1)	
  if hidx != -1:
    for token in ht.split('-'):
      if not word_in_dictionary(token,wordlist): return False
    return True
   
  if word_in_dictionary(ht,wordlist): return True #Case 2)  
  if word_in_dictionary(ht.replace('-',''),wordlist): return True #Case 3)
  for token in ht.split('-'): #Case 4)
      if not word_in_dictionary(token,wordlist): return False
  return True   

def regex_word_search(text):
    result_set = WORD_PAT.findall(text)
    
    #Normalize text here.
    #p.s it might be useful to take a look at the unicodedata module,
    #specifically the normalize() function.
    

    def normalize_text(word):
        '''turn words to lowercase, convert apostrophes'''
        word = word.lower()
        pat = "[%s]" % APOSTROPHES
        word = re.sub(pat,"'", word)
        return word
        
    return map(normalize_text, result_set)

        
def load_words(dict):
    '''Load a dictionary at location "dict.
    Return the result as a frozen set of words.'''
    dictfobj = codecs.open(dict, encoding="iso8859")

    #When unicode strings are compared with regular strings
    #the regular strings are converted into unicode automatically
    #(ascii is the assumed encoding)
    def tolower_and_strip(string):
        return string.lower().strip()

    words = map(tolower_and_strip, dictfobj.readlines())
    dictfobj.close()
    return frozenset(words)
    
def word_in_dictionary(word, dict): 
    try:
        if word in dict: return True
        return False
    except UnicodeWarning:
        print "WARNING: " + word

 
if __name__ == '__main__':
    if len(sys.argv) < 2:
        print 'usage: uc.py textfile'
        sys.exit(1)

    textf = codecs.open(sys.argv[1], encoding='utf-8')
    text = textf.read()
    textf.close()

    dict = load_words(DICT)
    words = regex_word_search(text)
    for word in words:
        if word.isdigit(): continue #skip digits

        hidx = word.find('-')
        if hidx == -1 :
            if not word_in_dictionary(word,dict):
                print word
        elif not processHyphenatedToken(word,hidx,dict):
            print word
      
    sys.exit(0)

