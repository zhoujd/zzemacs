repo
====

## Install repo

```
$ sudo curl https://storage.googleapis.com/git-repo-downloads/repo > /usr/local/bin/repo
$ sudo chmod a+x /usr/local/bin/repo
$ export PATH=${PATH}:/usr/local/bin
```

## Downloading Repo source

```
## Downloading Repo source from https://gerrit.googlesource.com/git-repo
$ export REPO_URL=https://mirrors.tuna.tsinghua.edu.cn/git/git-repo/
$ repo init -u <repo.git> -b dev
$ repo sync
```
