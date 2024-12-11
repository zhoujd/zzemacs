Module
======

## URLs

https://www.intel.com/content/www/us/en/docs/oneapi/programming-guide/2024-0/use-environment-modulefiles-with-linux.html
https://modules.readthedocs.io/en/latest/
http://modules.sourceforge.net/

## Use Environment Modulefiles with Linux*

```
## Set your environment
$ sudo apt update
$ sudo apt install tcl
$ sudo apt install environment-modules

## Confirm that the local copy of tclsh is new enough
$ echo 'puts [info patchlevel] ; exit 0' | tclsh

## Test the module installation by initializing the module alias
$ source /usr/share/modules/init/sh
$ module
$ echo $MODULEPATH

## Bash auto complete
$ source /usr/local/Modules/init/bash_completion
```

## Use Example

```
$ cat > compile-verison <<EOF
#%Module -*- tcl -*-

conflict        riscv_toolchain/freedomstudio-version

set             tool_home       /opt/riscv/toolchain
setenv          RISCV           $tool_home

append-path     PATH            $tool_home/bin
append-path     LIBRARY_PATH    $tool_home/lib
append-path     LD_LIBRARY_PATH $tool_home/lib
append-path     LD_INCLUDE_PATH $tool_home/include
append-path     MANPATH         $tool_home/share/man
EOF

$ module purge
$ module load riscv_toolchain/compile-version
$ module unload riscv_toolchain/compile-version
```
