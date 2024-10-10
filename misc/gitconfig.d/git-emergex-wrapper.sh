#!/bin/sh

### parameter desc
## $1 => $BASE
## $2 => $LOCAL
## $3 => $REMOTE
## $4 => $MERGED

if [ $# != 4 ]; then
    echo "Usage $0 \$1 \$2 \$3 \$4
\$1 => \$BASE
\$2 => \$LOCAL
\$3 => \$REMOTE
\$4 => \$MERGED
This is merge tools for emacs"
    exit 1
fi

main() {
    ELISP_PATH=${ELISP_PATH:-"~/zzemacs/elisp"}
    emacs --quick \
          --eval "(load-file \"$ELISP_PATH/ediff-sample.el\")" \
          --eval "(ediff-merge-files \"$2\" \"$3\" \"$1\" \"$4\")" \
          --eval "(message \"emacs merge finished.\")"
}

## run merge tools
main $1 $2 $3 $4
