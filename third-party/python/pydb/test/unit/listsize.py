#!/usr/bin/python -t
"Unit test for Extended Python debugger listsize "
import unittest, sys, os

top_srcdir = "../.."
if top_srcdir[-1] != os.path.sep:
    top_srcdir += os.path.sep
sys.path.insert(0, os.path.join(top_srcdir, 'pydb'))

import pydb                

class PdbTest(pydb.Pdb):
    def __init__(self):
        pydb.Pdb.__init__(self)
        self.errLines = []
        self.msgLines = []
        self.msg_last_nocr = False

    def errmsg(self, msg):
        self.errLines.append(msg)

    def msg(self, msg):
        if self.msg_last_nocr:
            self.msgLines[-1] += msg
        else:
            self.msgLines.append(msg)
        self.msg_last_nocr = False

    def msg_nocr(self, msg):
        if self.msg_last_nocr:
            self.msgLines[-1] += msg
        else:
            self.msgLines.append(msg)
        self.msg_last_nocr = True

class PdbListsize(unittest.TestCase):
    def test_listsize(self):
        """Test getting list command"""
        pydb = PdbTest()
        pydb.noninteractive = True

        # Run show listsize
        pydb.msgLines=[]; pydb.errLines=[]; 
        pydb.do_show("listsize")
        self.assertTrue(len(pydb.errLines)==0 and len(pydb.msgLines)!=0,
                        "'show listsize' command")

        # An invalid listsize command
        pydb.msgLines=[]; pydb.errLines=[]; 
        pydb.do_set("listsize foo")
        self.assertTrue(len(pydb.errLines)!=0 and len(pydb.msgLines)==0,
                        "Noninteger arg in 'set listsize'")

        pydb.msgLines=[]; pydb.errLines=[]; 
        pydb.do_set("listsize 20")
        self.assertEqual(pydb.listsize, 20)
        self.assertTrue(len(pydb.errLines)==0,
                        "No error in 'set listsize'")
        pydb.msgLines=[]; pydb.errLines=[]; 

if __name__ == "__main__":
    unittest.main()
