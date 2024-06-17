#!/bin/bash

search_up() {
    local look=${PWD%/}

    while [[ -n $look ]]; do
        [[ -e $look/$1 ]] && {
            printf '%s\n' "$look"
            return
        }

        look=${look%/*}
    done

    [[ -e /$1 ]] && echo /
}

search_up "$1"
