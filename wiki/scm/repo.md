repo
====

## URLs

```
## https://github.com/GerritCodeReview/git-repo
## https://gerrit.googlesource.com/git-repo
## https://mirrors.tuna.tsinghua.edu.cn/git/git-repo
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

## Get repo from your own computer (without internet)

```
## ... A new version of repo (2.61) is available.
## ... New version is available at: <dir>/.repo/repo/repo
## ... The launcher is run from: /usr/local/bin/repo
## !!! The launcher is not writable.  Please talk to your sysadmin or distro
## !!! to get an update installed.
$ cd workspace
$ git clone https://mirrors.tuna.tsinghua.edu.cn/git/git-repo
$ mkdir repo_init_no_internet && cd repo_init_no_internet
$ repo init --repo-url=/home/<user>/workspace/git-repo
```
