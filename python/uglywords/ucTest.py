# coding=utf-8
import unittest
import uc
import os 

class UCTest(unittest.TestCase):

    def testBasicDictionaryCreation(self):
        wordset = uc.load_words('/usr/share/dict/words')
        self.assert_('word' in wordset)
        self.assert_(not 'WORD' in wordset)
        self.assert_('mobile\'s' in wordset)
        self.assert_('mobile' in wordset)
        self.assert_('city' in wordset)
        self.assert_(not 'seahawks' in wordset)
        self.assert_('there' in wordset)
    
    def testBasicWordPartitioning(self):
        sentence = u"once upon a time there lived a buddah in a grassy cavern"
        got = set(uc.regex_word_search(sentence))
        expected = set(sentence.split())
        self.checkEqual(got,expected)


    def testBasic_NandW_Partitioning(self):
        sentence = "here laYeth 4.51"
        result = uc.regex_word_search(sentence)
        got = set(result)
        expected = set(['here', 'layeth', '4', '51'])
        self.checkEqual(got,expected)


    def testUnicode_Partitioning(self):
        unicodestr=u'チムブレ 中国话不用彁字 4.57'
        result = uc.regex_word_search(unicodestr)
        got = result
        expected = [u'チムブレ', u'中国话不用彁字', u'4', u'57']
        self.checkEqual(got,expected)

    
 #[Exceptions to the (rule) { not } with"
 #            " standing? 100%
    def testPunctuated_sentence(self):
        s = ("So that's the straight dope: She hit him, he ran; a \"clear\""
             + " case of hit and run!")
        expected = ['so', "that's", "the", "straight", "dope",
                    "she", "hit", "him", "he", "ran", "a", 'clear',
                    "case", "of", "hit", "and", "run"]
        got = uc.regex_word_search(s)
        self.checkEqual(got,expected)

    def testExtra_Punctuation(self):
        s = "[Ever] wonder 100% & gather $100 ~ {filthy} [greens?] <hobby>"
        expected = ['ever', 'wonder', '100', 'gather', '100',
                    'filthy', 'greens', 'hobby']
        got = uc.regex_word_search(s)
        self.checkEqual(got,expected)

    def testUnicode_Punctuation(self):
        '''Test Unicode punctuation characters are stripped.
        This isn't a very good test. I just quickly threw it together
        to check that a few of the unicode punctuation characters did not
        interfere with parsing out words. I thought that maybe unicode
        punctuation characters might have stayed within the word...

        Unicode code point 02b9 is one such punctuation character that
        is considered a valid character in a word.
        '''

        s = u"‘thats’‚ ‛too “funny”′wasteʹ ‴man‴"
        expected = [u'thats',u'too',u'funny', u'wasteʹ', u'man']
        got = uc.regex_word_search(s)
        self.checkEqual(got,expected)

    def testWords_In_Dict(self):
        s = """The city as we imagine it, the soft city of illusion, myth,
             aspiration, and nightmare, is as real, maybe more real, than
             the hard city one can locate on maps, in statistics, in
             monographs on urban sociology and demography and architecture."""
        words = map(str.lower,uc.regex_word_search(s))
        wordset = uc.load_words('/usr/share/dict/words')
        for word in words:
            self.assertTrue(uc.word_in_dictionary(word,wordset),
                            str(word) + " not in dictionary!")

    def testCliche(self):
        '''Test word is not split on special unicode char'''
        got = uc.regex_word_search(u"clichés")
        expected = [u'clichés']
        self.checkEqual(got,expected)

    def testUnicode_Punctuation2(self):
        got = uc.regex_word_search(u"hamstrung’s")
        expected = ["hamstrung's"]
        self.checkEqual(got,expected)

    def testQuotes(self):
        '''Test stripping quotes from a word'''
        got = uc.regex_word_search("'whatever'")
        expected = ['whatever']
        self.checkEqual(got,expected)

    def testHyphenWords(self):
        '''Test the effective capture of hyphenated words'''
        got = uc.regex_word_search("a-b-c;ab-c's;anti-flag;strong-bad-fun")
        expected = ['a-b-c', "ab-c's", 'anti-flag', 'strong-bad-fun']
        self.checkEqual(got,expected)

    def testProcessingHyphenatedToken(self):
        '''Test the identification of words in hyphenated tokens'''
        s = "a-b-c's a-b-c anti-death co-dependent amer-ican"
        wordset = ('a','b','c',"c's",'anti','death','co-dependent','american')
        words = uc.regex_word_search(s)
        for w in words:
            idx = w.find('-')
            if not uc.processHyphenatedToken(w,idx,wordset):
                self.assertTrue(False,"Token %s not in wordset" % w) 

    def testGroupAdjacentCapitalizedWordsAsSingleName(self):
        '''Test dictionary lookup of words composed of two 
        or more adjacent capitalized words. For example, New York should be
        looked up as "New York" and not the two separate words "New" and "York"'''
        s = "Once in Hong Kong and New   York, Sable rode."
        got = uc.regex_word_search(s)
        expected = ['once','in','hong kong','and','new   york', 'sable','rode']
        self.checkEqual(got,expected)

    def checkEqual(self,got,expected):
        self.assertEqual(got, expected, 
                         os.linesep + str(got) + os.linesep + str(expected))
if __name__ == '__main__':
  unittest.main()
