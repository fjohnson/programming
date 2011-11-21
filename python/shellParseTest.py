import unittest
import shellparse
import random

class TestPipelineCommands(unittest.TestCase):
    def test_pplni_Simple(self):
        result = shellparse.parsePipeline("ls")
        self.assertEqual(result,[(['ls'], [])])

    def test_pplni_CommandArgs(self):
        result = shellparse.parsePipeline("ls one two three")
        self.assertEqual(result,[(['ls','one','two','three'], [])])

    def test_pplni_CommandBg(self):
        result = shellparse.parsePipeline("ls&")
        self.assertEqual(result,[(['ls'], ['&'])])

    def test_pplni_CommandArgsBg(self):
        result = shellparse.parsePipeline("ls one two&")
        self.assertEqual(result,[(['ls','one','two'], ['&'])])

    def test_pplni_CommandRedir(self):
        
        result = shellparse.parsePipeline("cat file > out")
        self.assertEqual(result,[(['cat','file','out'], ['>'])])
        
        result = shellparse.parsePipeline("dmesg > out")
        self.assertEqual(result,[(['dmesg','out'], ['>'])])
    
    def test_pplni_CommandRedirIn(self):
        result = shellparse.parsePipeline("echo < file")
        self.assertEqual(result,[(['echo','file'], ['<'])])

    def test_pplni_CommandRedir_StdErr(self):
    
        result = shellparse.parsePipeline("cat file 2&> out")
        self.assertEqual(result,[(['cat','file','out'], ['2&>'])])
        
        result = shellparse.parsePipeline("dmesg 2&> out")
        self.assertEqual(result,[(['dmesg','out'], ['2&>'])])

    def test_pplni_CommandRedirBad(self):
        self.assertRaises(shellparse.commandError,
                          shellparse.parsePipeline,
                          'cat file > out here')

    def test_pplni_CommandRedirInBad(self):
        self.assertRaises(shellparse.commandError,
                          shellparse.parsePipeline,
                          'cat file < out here')

    def test_pplni_CommandRedirBG(self):
        result = shellparse.parsePipeline("cat file > out&")
        self.assertEqual(result,[(['cat','file','out'], ['>','&'])])

        result = shellparse.parsePipeline("cat file 2&> out&")
        self.assertEqual(result,[(['cat','file','out'], ['2&>','&'])])

    def test_pplni_CommandPipeBasic(self):
        result = shellparse.parsePipeline("cat file | less")
        self.assertEqual(result,[
                (['cat','file'], []),
                (['less'], [])
                ])

    def test_pplni_CommandPipeBasic2(self):
        result = shellparse.parsePipeline("cat file | grep -i item | less")
        self.assertEqual(result,[
                (['cat','file'], []),
                (['grep','-i','item'], []),
                (['less'], [])
                ])

    def test_pplni_CommandPipe_Fout(self):
        result = shellparse.parsePipeline("cat file | echo > out")
        self.assertEqual(result,[
                (['cat','file'], []),
                (['echo','out'], ['>'])
                ])
    def test_pplni_CommandPipe_FoutBG(self):
        result = shellparse.parsePipeline("cat file | echo > out&")
        self.assertEqual(result,[
                (['cat','file'], []),
                (['echo','out'], ['>','&'])
                ])

    def test_pplni_CommandPipe_Fout(self):
        result = shellparse.parsePipeline("cat file | echo 2&> out")
        self.assertEqual(result,[
                (['cat','file'], []),
                (['echo','out'], ['2&>'])
                ])

    def test_pplni_CommandPipe_FoutBG(self):
        result = shellparse.parsePipeline("cat file | echo 2&> out&")
        self.assertEqual(result,[
                (['cat','file'], []),
                (['echo','out'], ['2&>','&'])
                ])

    def test_pplni_CommandPipe_BadBG(self):
        bad_ex = ['cat file& | echo',
                  'cat file& | echo > file',
                  'cat file& | echo 2&> file',
                  'cat& file | echo 2&> file']
        def check(bad_c):
            self.assertRaises(shellparse.commandError,
                              shellparse.parsePipeline,
                              bad_c)
        map(check,bad_ex)

    def test_CommandTokenizer(self):
        command = ("ls","ls /home","cut -d' '",
                   "\\'", "'pop'", "'\pop'", "\ ",
                   "\  ", "\\''apples'", "'a'b'c'\\d")
        expected = (["ls"],["ls","/home"], ["cut", "-d "],
                    ["'"], ["pop"], ["\pop"], [" "],
                    [" "], ["'apples"], ["abcd"])
        for c,e in zip(command,expected):
            result = shellparse.tokenizeCommand(c)
        self.assertEqual(result,e)

        
if __name__ == '__main__':
  def suite():
    tests = ['test_pplni_CommandRedirBG']
    return unittest.TestSuite(map(TestPipelineCommands, tests))

  #unittest.TextTestRunner(verbosity=2).run(suite())
  unittest.main()
