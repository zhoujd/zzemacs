## command

import os
import platform

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
