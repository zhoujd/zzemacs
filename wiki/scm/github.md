Github
======

1. Download tarball base on commit id

        ## unarchive tar.gz with --strip-components=1
        ## https://github.com/<username>/<repository>/tarball/<version>
        $ wget https://github.com/OpenVisualCloud/VCAC-SW-Analytics/tarball/1ce455db80e20b544b9d9e67176636e13f3f42f0
        $ wget https://codeload.github.com/OpenVisualCloud/VCAC-SW-Analytics/tar.gz/1ce455db80e20b544b9d9e67176636e13f3f42f0
        $ wget -O - https://codeload.github.com/OpenVisualCloud/VCAC-SW-Analytics/legacy.tar.gz/1ce455db80e20b544b9d9e67176636e13f3f42f0 | tar xz
        $ curl -L  https://codeload.github.com/OpenVisualCloud/VCAC-SW-Analytics/tar.gz/1ce455db80e20b544b9d9e67176636e13f3f42f0 | tar xz --strip-components=1

        $ curl -u username:password https://github.com/<org>/<repo>/tarball/<sha>
        $ curl -L "https://api.github.com/repos/<org>/<repo>/tarball/$commit_sha?access_token=$github_token" | tar -xz -C "$extract_dir/"
