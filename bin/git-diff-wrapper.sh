#!/bin/sh

### parameter desc
## $1 => $LOCAL
## $2 => $REMOTE

## difftool selects
## http://www.scootersoftware.com/support.php?c=kb_vcs.php
## http://www.perforce.com/perforce/products/merge.html
## http://meldmerge.org/

## use emacs as diff tool
EMACS_FLAG="y"

diff_extern()
{
    if [ "$OS" = "Windows_NT" ] ; then
        DIFF_TOOL_0="$ZZNIX_HOME/home/zhoujd/zztools/bcompare/bcompare $*"
        DIFF_TOOL_1="$ZZNIX_HOME/home/zhoujd/zztools/perforce/p4merge $*"

        DIFF_SELECT=$DIFF_TOOL_0
    else
        DIFF_TOOL_0="$HOME/zztools/bcompare/bin/bcompare $*"
        DIFF_TOOL_1="$HOME/zztools/meld/bin/meld $*"
        DIFF_TOOL_2="$HOME/zztools/p4v/bin/p4merge $*"

        DIFF_SELECT=$DIFF_TOOL_1
    fi

    $DIFF_SELECT
}

diff_emacs()
{
    if [ "$OS" = "Windows_NT" ] ; then
		ZZEMACS_PATH="$ZZNIX_HOME/home/zhoujd/zzemacs"
	else
		ZZEMACS_PATH="$HOME/zhoujd/zzemacs"
	fi

    emacs --no-site-file -q \
          --eval "(load-file \"$ZZEMACS_PATH/elisp/ediff-sample.el\")" \
          --eval "(ediff-sample-diff \"$1\" \"$2\")" \
          --eval "(message \"emacs diff finished.\")"
}

main()
{
    case "$EMACS_FLAG" in
        "Y" | "y" )
            diff_emacs $* 
            ;;
        * )
            diff_extern $*
            ;;
    esac
}

## run diff tools
main $1 $2
