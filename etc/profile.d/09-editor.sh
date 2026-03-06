### editor

for e in me ec vi; do
    if command -v $e >/dev/null 2>&1; then
        export EDITOR=$e
        export VISUAL=$e
        break
    fi
done
