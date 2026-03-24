bash
====

## Trap ctrl-c and call ctrl_c()

```
trap ctrl_c INT

ctrl_c()
{
    echo "** Trapped CTRL-C"
}
```
