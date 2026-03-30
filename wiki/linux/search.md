search
======

## ag search "--"

```
## The first -- tells ag that subsequent arguments are patterns/files, not options.
$ ag -- "--foo" /path/to/search

## -Q ensure the parttern is treated a literal string not a regular expression
ag -Q -- "--foo"
```
