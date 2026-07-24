codex
=====

## URLs

```
## https://github.com/openai/codex
```

## Dependence

```
$ sudo apt install bubblewrap
```

## Download

```
$ VER=v0.145.0     ## v0.136.0
$ wget https://github.com/openai/codex/releases/download/rust-${VER}/codex-x86_64-unknown-linux-musl.tar.gz
$ tar xfv codex-x86_64-unknown-linux-musl.tar.gz
$ sudo cp -vf codex-x86_64-unknown-linux-musl /usr/local/bin/codex
$ codex -V
```
