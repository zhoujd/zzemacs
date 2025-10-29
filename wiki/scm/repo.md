repo
====

## URLs

```
## https://github.com/GerritCodeReview/git-repo
```

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

## Avoid 'Enable color display in this user account (y/N)?'

```
$ git config --global color.ui "true"
```
