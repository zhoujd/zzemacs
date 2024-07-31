# prf-tramp

This repository holds several packages:

 - `prf-tramp`: Various TRAMP util functions
 - `prf-tramp-method`: Helper functions for manipulating tramp methods
 - `prf-tramp-friendly-parsing`: Helper functions to allow a more human-friendly construction of TRAMP paths

Examples can be found in [examples.md](examples.md).


## Installation

Not yet on [Melpa](https://melpa.org/).

For now, the recommended way to install is with [use-package](https://github.com/jwiegley/use-package), [quelpa](https://github.com/quelpa/quelpa) and [quelpa-use-package](https://github.com/quelpa/quelpa-use-package).

To require everything:

```el
(use-package prf-tramp
  :quelpa (prf-tramp :fetcher github :repo "p3r7/prf-tramp"
  :after tramp))

(use-package prf-tramp-method
  :quelpa (prf-tramp-method :fetcher github :repo "p3r7/prf-tramp")
  :after tramp)

(use-package prf-tramp-friendly-parsing
  :quelpa (prf-tramp-friendly-parsing :fetcher github :repo "p3r7/prf-tramp"))
```


## Functions

### TRAMP VEC Utils

Helper functions to manipulate TRAMP vec objects.

* prf-tramp | [prf/tramp/vec/undissect](#prftrampvecundissect-vec) `(vec)`
* prf-tramp | [prf/tramp/vec/vec-p](#prftrampvecvec-p-vec) `(vec)`


## TRAMP VEC Utils

#### prf/tramp/vec/undissect `(vec)`

Converts VEC back into TRAMP path string.


#### prf/tramp/vec/vec-p `(vec)`

Returns `'t` when VEC is a valid TRAMP vec.


## Usage

For a concrete example, read [this article about overriding some TRAMP methods](https://www.eigenbahn.com/2020/01/15/tramp-autologin-insanity).
