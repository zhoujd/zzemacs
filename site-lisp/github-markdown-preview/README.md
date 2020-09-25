# github-markdown-preview.el

Emacs lisp to preview markdowned text with browser using Github markdown render API.

## Requirements

* Emacs 23 or higher.
* Browser (Tested on Firefox and Chrome for Linux).

## Installation

### Manually

``` shell
$ git clone https://github.com/alpha22jp/github-markdown-preview.el.git
```

Then, add the directory you put this package to `load-path` of your Emacs like below.

``` emacs-lisp
(add-to-list 'load-path
             (expand-file-name "~/foo/bar/github-markdown-preview.el"))
```

Then add the following line to your Emacs setup file.

``` emacs-lisp
(require 'github-markdown-preview)
```

## Usage

`M-x github-markdown-preview` during editing markdwoned text file.

## Known issues

* For Firefox, need to reload window when the preview file is already opened on the browser.

## History

version 0.1.0

* Initial release
