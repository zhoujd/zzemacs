markdown
========

## Pandoc convert docx to markdown with embedded images

    $ sudo apt install -y markdown pandoc
    $ pandoc --extract-media ./myMediaFolder input.docx -o output.md
    $ pandoc -s example30.docx --no-wrap --reference-links -t markdown -o example35.md
