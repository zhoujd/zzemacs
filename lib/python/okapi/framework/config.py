# website: https://github.com/zhoujd/zzokapi
# author: Zachary Zhou <zchrzhou@gmail.com>

# framework config file

appdirname = "core"
appignorefiles = ["__init__.py"]
appnames = {
    ".py": "python",
    ".pl": "perl",
    ".sh": "bash",
    ".go": "go run",    #Line 1: //usr/bin/env go run "$0" "$@"; exit "$?"
}
verbose = False
maxmatch = 5
supportnoext = True
limitdept = True
