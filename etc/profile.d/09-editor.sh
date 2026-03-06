### editor

if [ -n "$INSIDE_EMACS" ]; then
    export EDITOR=ec
    export VISUAL=ec
else
    for e in me vi; do
        if command -v $e >/dev/null 2>&1; then
            export EDITOR=$e
            export VISUAL=$e
            break
        fi
    done
fi
