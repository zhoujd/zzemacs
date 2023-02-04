Hugo
====

## Use Hugo to Generate a Static Website

    ## https://thenewstack.io/tutorial-use-hugo-to-generate-a-static-website/
    ## Installing Hugo
    $ sudo apt install hugo git -y

    ## Create a Static Site with Hugo
    ## Step 1: Generate the Site
    $ hugo new site newstack
    $ cd newstack

    ## Step 2: Download a Theme (https://themes.gohugo.io/)
    $ git init
    $ git submodule add https://github.com/luizdepra/hugo-coder.git themes/hugo-coder
    $ cd themes/hugo-coder
    $ cp -rf * ../../
    $ cd ../../
    $ cp exampleSite/config.toml .

    ## Step 3: Launch the Site Locally
    $ hugo server -D
    $ nano config.toml
    [params]
    info = "Hello, New Stack"

    ## How to Create New Content
    $ cp -rf exampleSite/content/* ./content
    $ hugo new content/posts/blog-post-1.md
    $ nano content/posts/blog-post-1.md

    ## Hugo will automatically detect the change of the newly added

## Carefull erases docsy-example to start new

    $ hugo version
    $ rm -rf docsy-example
    $ git clone --recurse-submodules --depth 1 https://github.com/google/docsy-example.git
    $ cd docsy-example
    $ git checkout -b main
    $ mv config.toml config.toml-orig
    $ sed 's/# github_branch/github_branch/g' config.toml-orig > config.toml
    $ diff config.toml-orig config.toml
    $ hugo serve
