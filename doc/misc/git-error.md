Git error and solutions
========

1. Pushing to Git returning Error Code 403 fatal: HTTP request failed

   Edit .git/config file under your repo directory
   Find url= entry under section [remote "origin"]
   Change it from url=https://github.com/rootux/ms-Dropdown.git to https://USERNAME@github.com/rootux/ms-Dropdown.git
   where USERNAME is your github user name

   or
   
   git remote set-url origin https://yourusername@github.com/user/repo.git

   
2. Failed connect to github.com:443; No error

   git config --global http.proxy <proxy-server-address:port>

