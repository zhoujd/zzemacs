# website: https://github.com/zhoujd/zzokapi
# author: Zachary Zhou <zchrzhou@gmail.com>

# common

import os
import platform
import okapi.framework as framework

def path2unix(path):
    sysstr = platform.system()
    if sysstr == "Windows":
        path = path.replace("\\", "/")
    return path

def getworkdir():
    return path2unix(os.getcwd())
    
def getfiledir(filepath):
    strfilepath = os.path.realpath(filepath)
    return path2unix(os.path.dirname(strfilepath))

def setcoredir(dir):
    framework.config.appdirname = dir
