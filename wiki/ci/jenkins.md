Jenkins
=======

## Jenkins + Artifactory Integration

    ## https://developers.sinch.com/docs/sms/sdks/maven-jenkins-artifactory-integration/
    ## https://jfrog.com/open-source/#artifactory

## JFrog CLI

    ## https://jfrog.com/getcli/

## Jenkins Plugins Adding

    ## Copy Artifacts
    ## Audit log
    ## Audit tail
    ## Github pull request builder
    ## Label Linked Jobs Plugin
    ## Job Import Plugin
    ## Authorize Project Plugin

## Jenkins build github pull request with GitHub Pull Request Builder Plugin

    ## https://github.com/janinko/ghprb
    ## https://mreigen.medium.com/integrate-jenkins-builds-into-github-pull-requests-33bc053d6210
    ## Step 1: Go to Manage Jenkins > Configure System -> GitHub Pull Request Builder
    ## GitHub Server API URL: https://api.github.com
    ## Credentials: Click Add
    ## Step 2: Configure your build project to receive web-hook from GitHub
    ## Click on the project name, click Configure. Enter:
    ## Under Advanced, set Name to origin
    ## If you just want to build PRs, set refspec to +refs/pull/${ghprbPullId}/*:refs/remotes/origin/pr/${ghprbPullId}/*
    ## If you want to build PRs and branches, set refspec to +refs/heads/*:refs/remotes/origin/* +refs/pull/*:refs/remotes/origin/pr/*
    ## (see note below about parameterized builds - https://github.com/janinko/ghprb#parameterized-builds)
    ## In Branch Specifier, enter ${sha1} instead of the default */master.
    ## If you want to use the actual commit in the pull request, use ${ghprbActualCommit} instead of ${sha1}

## PR build with GitHub Branch Source Plugin

    ## Click "New item" and select "Multibranch Pipeline" as type
    ## In Job Configuration select "GitHub" as branch source, select credentials and enter your GitHub HTTPS URL
    ## Kick off a PR

## How to Reset Jenkins Admin users Password

    ## https://stackoverflow.com/questions/6988849/how-to-reset-jenkins-security-settings-from-the-command-line
    ## Change true to false in /var/lib/jenkins/config.xml file => <useSecurity>false</useSecurity>
    $ cat /var/lib/jenkins/config.xml
    $ sed -i 's/<useSecurity>true<\/useSecurity>/<useSecurity>false<\/useSecurity>/g' /var/lib/jenkins/config.xml
    $ sudo service jenkins restart
    
## Jenkins with GitHub via webhook

    ## https://zhuanlan.zhihu.com/p/519172176
    ## 1. Install GitHub Plugin
    ## 2. Config Github Plugin (token with 'admin:repo_hook', 'repo' and 'repo:status')
    ## 3. Create a freestyle Job (checked 'Build when a change is pushed to GitHub')
    ## 4. Config Webhook on Github (Settings >> Webhooos & services -> Just the push event -> Add webhook)

