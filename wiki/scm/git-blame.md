Git Blame
=========

1. Basic Usage

        $ git blame myfile
        $ git log -p abcdefgh

        ## Displaying Author Email
        $ git blame -e myfile

        ## Displaying Long Commit Hash
        $ git blame -l myfile
        $ git blame -el gulpfile.babel.js

        ## Displaying Raw Timestamp
        $ git blame -t gulpfile.babel.js

        ## Listing Specific Range of Lines
        $ git blame -L startLineNumber,endLineNumber filePath
        $ git blame -L 10,20 gulpfile.babel.js

        $ git blame -L X,+N
        $ git blame -L 15,+5 gulpfile.babel.js

        ## Getting Help
        $ man git-blame
        $ git help blame

2. Emacs use git blame

    First, use the vc-annotate command, which is bound to the key sequence C-x v g.

