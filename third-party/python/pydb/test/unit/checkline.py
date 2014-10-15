#!/usr/bin/python -t
# -*- Python -*-
# $Id: checkline.py.in,v 1.2 2008/12/21 10:54:23 rockyb Exp $
"Unit test for checkline"
import inspect, os, sys, unittest

top_builddir = "../.."
if top_builddir[-1] != os.path.sep:
    top_builddir += os.path.sep
sys.path.insert(0, os.path.join(top_builddir, 'pydb'))
top_srcdir = "../.."
if top_srcdir[-1] != os.path.sep:
    top_srcdir += os.path.sep
sys.path.insert(0, os.path.join(top_srcdir, 'pydb'))

import fns

class CheckLine(unittest.TestCase):
    def errmsg(self, msg):
        self.errors.append(msg)
        return

    def test_basic(self):
        global top_srcdir
        self.curframe = inspect.currentframe()
        check_script = os.path.join(top_srcdir, 'test', 'data',
                                    'checkline.py')
        for t in ( ( 1, True), 
                   ( 2, False),
                   ( 3, False),  # FIXME add 4
                   ( 5, False),
                   ( 6, True),
                   ( 7, False),
                   ( 8, False),
                   ( 9, True),
                   (10, False),
                 ):
            self.errors=[]
            result = fns.checkline(self, check_script, t[0])
            if t[1]:
                self.assertEqual(t[0], result, "Should have worked")
                self.assertEqual(0, len(self.errors), "With no errors")
            else:
                self.assertNotEqual(t[0], result, "Should not have worked")
                self.assertEqual(1, len(self.errors), "and given an error")
        return

if __name__ == '__main__':
    unittest.main()
