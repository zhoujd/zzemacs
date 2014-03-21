"""Python Debugger with a gdb-like command interface."""
# -*- coding: UTF-8 -*-
app_name = 'Python Extended Debugger'
version = '1.26'
URL = 'http://bashdb.sourceforge.net/pydb/'
HELP_URL = URL + 'pydb/index.html'

import sys
assert sys.version_info>= map(int, '2.4.0'.split('.')), \
	 _("Python %s or newer required") % '2.4.0'
from pydb import *

