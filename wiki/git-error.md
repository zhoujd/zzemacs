Git error and solutions
=======================

1. Pushing to Git returning Error Code 403 fatal: HTTP request failed

   Edit .git/config file under your repo directory
   Find url= entry under section [remote "origin"]
   Change it from url=https://github.com/zhoujd/zzemacs.git to https://USERNAME@github.com/zhoujd/zzemacs.git
   where USERNAME is your github user name

   or
   
        $ git remote set-url origin https://yourusername@github.com/user/repo.git

   
2. Failed connect to github.com:443; No error

        $ git config --global http.proxy <proxy-server-address:port>

3. Delete remote branch

        $ git br -dr origin/test 
        $ git push origin :<branch-name>


4. Delete remote tag

        $ git push origin :refs/tags/<tag-name>

5. Unable to negotiate with <ip_address>: no matching host key type found. Their offer: ssh-dss

   [Linux] /etc/ssh/ssh_config
   [Windows] The OpenSSH config file should be located at C:\Program Files\Git\etc\ssh\ssh_config. 
   Add the following (you will need to do this with administrator privileges):
  
        HostkeyAlgorithms +ssh-dss

6. Rollback from 'git reset --hard' 

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
