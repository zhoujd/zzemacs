#### This is py tools setup script for zhoujd

import os
import sys
import glob
from distutils.core import setup

setup(
    name             = "pycomplete",
    version          = "0.1",
    description      = "python tools for zachary zhou",
    long_description = "self using script on python",
    author           = "zachary zhou",
    author_email     = "zjd-405@163.com",
    url              = "https://github.com/zhoujd",
    license          = "LGPL",
    py_modules       = ['pycomplete'],
	requires         = ['pymacs'],
    scripts          = [],
    )

