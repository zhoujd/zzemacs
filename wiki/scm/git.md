Git
===

## Pushing to Git returning Error Code 403 fatal: HTTP request failed

    ## Option 1
    # Edit .git/config file under your repo directory
    # Find url= entry under section [remote "origin"]
    # Change it from url=https://github.com/zhoujd/zzemacs.git to https://USERNAME@github.com/zhoujd/zzemacs.git
    # where USERNAME is your github user name

    ## Option 2
    $ git remote set-url origin https://yourusername@github.com/user/repo.git

## Failed connect to github.com:443; No error

    $ git config --global http.proxy <proxy-server-address:port>

## Delete remote branch

    $ git br -dr origin/test
    $ git push origin :<branch-name>

## Delete remote tag

    $ git push origin :refs/tags/<tag-name>

## Unable to negotiate with <ip_address>: no matching host key type found. Their offer: ssh-dss

    [Linux] /etc/ssh/ssh_config
    [Windows] The OpenSSH config file should be located at C:\Program Files\Git\etc\ssh\ssh_config.
    Add the following (you will need to do this with administrator privileges):

    HostkeyAlgorithms +ssh-dss

## Rollback from 'git reset --hard'

    If you didn't already commit your local changes (or at least stage them via `git add`, they're gone.
    `git reset --hard` is a destructive operation for uncommitted changes.
    If you did happen to stage them, but didn't commit them,
    try `git fsck --lost-found` and then search through the contents of .git/lost-found -
    it will contain all of the objects that aren't referenced by a known commit,
    and may include versions of files that were staged.
    You can recover anything you git added, with git fsck --lost-found and poke around in .git/lost-found.
    find .git/objects -type f | xargs ls -lt | sed 60q will give you the last 60 things to get added to the repo,
    that'll help.
    Anything you didn't git add is gone as surely as if you'd deleted it yourself.

    $ git reflog

    $ find .git/objects/ -type f | xargs ls -lt | sed 10q
    $ find .git/objects/ -type f | xargs ls -lt | head -n 5

    166 Apr 28 18:23 .git/objects/31/15844befa835f26603625d1cf09ba59011e2b8

    $ git cat-file p <ID> > a.md
    $ git cat-file p 3115844befa835f26603625d1cf09ba59011e2b8 ## remove /

## Delete submodule

    $ git submodule deinit {MOD_NAME}
    $ git rm --cached {MOD_NAME}
    $ git commit -am "Remove a submodule"

## Update submodule URL

    ### Note: git version greater than 2.7.4, the lower version may need chang .git/config manually
    ## Step 1: Modify URL in .gitmodules
    ## Step 2: Update URL to .git/config
    $ git submodule sync
    ## Step 3: Save via commit
    $ git commit -am "Update submodule url"

## Rename branch

    $ git branch -m new-name
    $ git branch -m old-name new-name
    $ git push origin :old-name new-name
    $ git push origin -u new-name

## No submodule mapping found in .gitmodules for path 'xxxxxx'

    $ git rm --cached  xxxxxx

## Git submodule tracking latest
    Git 1.8.2 added the possibility to track branches.

    # add submodule to track master branch
    $ git submodule add -b master [URL to Git repo];
    $ git submodule add -b . [URL to Git repo]
    $ git config -f .gitmodules submodule.<path>.branch <branch>

    # update your submodule
    $ git submodule update --remote

## gnutls_handshake() failed: Error in the pull function

    ## rebuid git from source with openssl
    $ sudo apt-get update
    $ sudo apt-get install build-essential fakeroot dpkg-dev
    $ sudo apt-get build-dep git
    $ sudo apt-get install libcurl4-openssl-dev
    $
    $ mkdir ~/git-openssl
    $ cd ~/git-openssl
    $ apt-get source git
    $
    $ cd git-2.17.1/
    $ sed -i 's/libcurl4-gnutls-dev/libcurl4-openssl-dev/g' debian/control
    $ sed -i 's/TEST =test//g' debian/rules
    $ sudo dpkg-buildpackage -rfakeroot -b
    $ cd ..
    $ sudo dpkg -i git_2.17.1-1ubuntu0.3_amd64.deb

    or

    ## Install git package with openssl support
    $ wget https://www.dreamoftime0.com/wp-content/uploads/ftp/tools/git_openssl_2.17.1/git_2.17.1-1ubuntu0.3_amd64.deb
    $ sudo dpkg -i git_2.17.1-1ubuntu0.3_amd64.deb

## Git clone verbose output

    $ GIT_TRACE=2 git clone --progress --verbose http://github.com/zhoujd/zzemacs ~/zzemacs

## How to Use Multiple Git Configs on One Computer

    $ cat ~/.gitconfig
    [includeIf "gitdir:~/personal/"]
    path = ~/.gitconfig-personal
    [includeIf "gitdir:~/work/"]
    path = ~/.gitconfig-work

## Gitk dark mode

    ## https://github.com/dracula/gitk
    $ mkdir -p ~/.config/git
    $ git clone https://github.com/dracula/gitk
    $ cp gitk/gitk ~/.config/git/

## Git GUI

    $ sudo apt install git-gui
    $ cd /usr/share/git-gui/lib

## How to use git bisect?

    $ git help bisect
    $ git log --pretty=oneline
    $ git bisect start
    $ git bisect bad                 # Current version is bad
    $ git bisect good v2.6.13-rc2    # v2.6.13-rc2 is known to be good
    or
    $ git bisect start [end_commit] [begin_commit]
    $ git bisect start HEAD 4d83cf
    $ git log  ## run test
    $ git bisect good
    $ git log  ## run test
    $ git bisect bad
    $ git bisect reset

## gitea

    ## https://gitea.io/en-us/
    ## https://docs.gitea.io/en-us/install-with-docker/
    ## https://docs.gitea.io/en-us/install-from-binary/

## How to cherry-pick multiple commits

    ## https://stackoverflow.com/questions/1670970/how-to-cherry-pick-multiple-commits
    ## From commit A to commit B
    $ git cherry-pick A^..B

    ## Ignore A itself
    $ git cherry-pick A..B

## Deleting Git Branches with Magit

    ## Open the Magit status buffer (C-x g)
    ## Press y to get a listing of all branches and tags in the git repo
    ## Navigate to the branch you want to delete and press k

## Git Submodule ans misc commands

    ## http://gitbook.liuhui998.com/index.html
    $ git submodule add url path/to/name
    $ git submodule init
    $ git submodule foreach git pull
    $ git submodule update
    $ git cherry-pick xxx-commit-id
    $ git pull --squash another
    $ git update-index --assume-unchanged /path/to/file
    $ git update-index --no-assume-unchanged /path/to/file
    $ find -type d -empty -exec touch {}/.gitignore \;

## Build from source

    $ sudo apt install libssl-dev libcurl4-openssl-dev libexpat1-dev
    $ sudo yum install openssl-devel libcurl-devel expat-devel perl-devel
    $ wget https://github.com/git/git/archive/v2.16.6.zip
    $ unzip v2.16.6.zip
    $ cd git-2.16.6
    $ make prefix=/opt/git all
    $ make prefix=/opt/git install
    $ echo /opt/git/bin >> /etc/environment # or others

## Get work root

    $ git rev-parse --show-toplevel

## GitHub Token

    $ GITHUB_TOKEN="xxxxxxxxxxxxxxx"
    $ git config --global url."https://${GITHUB_TOKEN}@github.com/zhoujd".insteadOf "https://github.com/zhoujd"
    $ git config --file=~/.gitconfig-url url."https://${GITHUB_TOKEN}@github.com/zhoujd".insteadOf "https://github.com/zhoujd"
    $ cat ~/.gitconfig-url
      [url "https://${GITHUB_TOKEN}@github.com/zhoujd"]
        insteadOf = https://github.com/zhoujd


## Git reset author

    $ git commit --amend --author="Author Name <email@address.com>"
    $ git commit --amend --reset-author --no-edit

## Git Corrupt loose object

    ## https://stackoverflow.com/questions/4254389/git-corrupt-loose-object
    $ rm -fr .git
    $ git init
    $ git remote add origin [your-git-remote-url]
    $ git fetch
    $ git reset --mixed origin/main
    $ git branch --set-upstream-to=origin/main main

    ## https://accio.github.io/programming/2021/06/16/fix-loose-objects-in-git.html
    $ find .git/objects/ -size 0 -exec rm -f {} \;
    $ git fetch origin

## Git diff to pipeline

```
$ git diff | delta --paging always --side-by-side
$ git log -1 -p | delta --paging always --side-by-side
$ git show HEAD | delta --paging always --side-by-side

$ git diff | riff
$ git log -1 -p | riff
$ git show HEAD | riff

$ git diff | tig
$ git log -1 -p | tig
$ git show HEAD | tig
```
