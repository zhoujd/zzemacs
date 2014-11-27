# shell directory bookmark setting
SHELLBM_FILE=~/.sdirs

alias m0='alias g0="cd `pwd`"'
alias m1='alias g1="cd `pwd`"'
alias m2='alias g2="cd `pwd`"'
alias m3='alias g3="cd `pwd`"'
alias m4='alias g4="cd `pwd`"'
alias m5='alias g5="cd `pwd`"'
alias m6='alias g6="cd `pwd`"'
alias m7='alias g7="cd `pwd`"'
alias m8='alias g8="cd `pwd`"'
alias m9='alias g9="cd `pwd`"'

alias lma='alias | grep -e "alias g[0-9]" | grep -v "alias m" | sed "s/alias //"'
alias mdump="alias | grep -e 'alias g[0-9]' | grep -v 'alias m' > $SHELLBM_FILE"
alias mcls="rm -f $SHELLBM_FILE && touch $SHELLBM_FILE"

touch $SHELLBM_FILE
source $SHELLBM_FILE
