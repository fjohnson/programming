import unittest
import shellparse
import random

class TestPipelineCommands(unittest.TestCase):
    def test_pplni_Simple(self):
        result = shellparse.parsePipelineNoIn("ls")
        self.assertEqual(result,[(['ls'], [])])

    def test_pplni_CommandArgs(self):
        result = shellparse.parsePipelineNoIn("ls one two three")
        self.assertEqual(result,[(['ls','one','two','three'], [])])

    def test_pplni_CommandBg(self):
        result = shellparse.parsePipelineNoIn("ls&")
        self.assertEqual(result,[(['ls'], ['&'])])

    def test_pplni_CommandArgsBg(self):
        result = shellparse.parsePipelineNoIn("ls one two&")
        self.assertEqual(result,[(['ls','one','two'], ['&'])])

    def test_pplni_CommandRedir(self):
        
        result = shellparse.parsePipelineNoIn("cat file > out")
        self.assertEqual(result,[(['cat','file','out'], ['>'])])
        
        result = shellparse.parsePipelineNoIn("dmesg > out")
        self.assertEqual(result,[(['dmesg','out'], ['>'])])
    
    def test_pplni_CommandRedirIn(self):
        result = shellparse.parsePipelineNoIn("echo < file")
        self.assertEqual(result,[(['echo','file'], ['<'])])

    def test_pplni_CommandRedir_StdErr(self):
    
        result = shellparse.parsePipelineNoIn("cat file 2&> out")
        self.assertEqual(result,[(['cat','file','out'], ['2&>'])])
        
        result = shellparse.parsePipelineNoIn("dmesg 2&> out")
        self.assertEqual(result,[(['dmesg','out'], ['2&>'])])

    def test_pplni_CommandRedirBad(self):
        self.assertRaises(shellparse.commandError,
                          shellparse.parsePipelineNoIn,
                          'cat file > out here')

    def test_pplni_CommandRedirInBad(self):
        self.assertRaises(shellparse.commandError,
                          shellparse.parsePipelineNoIn,
                          'cat file < out here')

    def test_pplni_CommandRedirBG(self):
        result = shellparse.parsePipelineNoIn("cat file > out&")
        self.assertEqual(result,[(['cat','file','out'], ['>','&'])])

        result = shellparse.parsePipelineNoIn("cat file 2&> out&")
        self.assertEqual(result,[(['cat','file','out'], ['2&>','&'])])

    def test_pplni_CommandPipeBasic(self):
        result = shellparse.parsePipelineNoIn("cat file | less")
        self.assertEqual(result,[
                (['cat','file'], []),
                (['less'], [])
                ])

    def test_pplni_CommandPipeBasic2(self):
        result = shellparse.parsePipelineNoIn("cat file | grep -i item | less")
        self.assertEqual(result,[
                (['cat','file'], []),
                (['grep','-i','item'], []),
                (['less'], [])
                ])

    def test_pplni_CommandPipe_Fout(self):
        result = shellparse.parsePipelineNoIn("cat file | echo > out")
        self.assertEqual(result,[
                (['cat','file'], []),
                (['echo','out'], ['>'])
                ])
    def test_pplni_CommandPipe_FoutBG(self):
        result = shellparse.parsePipelineNoIn("cat file | echo > out&")
        self.assertEqual(result,[
                (['cat','file'], []),
                (['echo','out'], ['>','&'])
                ])

    def test_pplni_CommandPipe_Fout(self):
        result = shellparse.parsePipelineNoIn("cat file | echo 2&> out")
        self.assertEqual(result,[
                (['cat','file'], []),
                (['echo','out'], ['2&>'])
                ])

    def test_pplni_CommandPipe_FoutBG(self):
        result = shellparse.parsePipelineNoIn("cat file | echo 2&> out&")
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
                              shellparse.parsePipelineNoIn,
                              bad_c)
        map(check,bad_ex)

if __name__ == '__main__':
  def suite():
    tests = ['test_pplni_CommandRedirBG']
    return unittest.TestSuite(map(TestPipelineCommands, tests))

  #unittest.TextTestRunner(verbosity=2).run(suite())
  unittest.main()
