repo
====

## Install repo

```
$ curl https://storage.googleapis.com/git-repo-downloads/repo > repo
$ chmod +x repo
$ sudo mv repo /usr/local/bin/
```

## Downloading repo source

```
## Downloading repo source from https://gerrit.googlesource.com/git-repo
$ export REPO_URL=https://mirrors.tuna.tsinghua.edu.cn/git/git-repo/
$ repo init -u <repo.git> -b dev
$ repo sync
```
