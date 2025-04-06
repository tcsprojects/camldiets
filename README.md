Camldiets
==================

Version 0.3, Copyright (c) 2013-2025

A highly efficient OCaml set implementation for fat sets, i.e. densely populated sets over a discrete linear order.

It is developed and maintained by:
- (c) Oliver Friedmann, University of Munich (http://oliverfriedmann.com)
- (c) Martin Lange, University of Kassel (http://carrick.fmv.informatik.uni-kassel.de/~mlange/)

## OPAM

You can install this package via OPAM under the name `Camldiets`.


## Credits
The code for handling the AVL trees is borrowed from the Objective Caml Standard Library Set module.
(c) Xavier Leroy, projet Cristal, INRIA Rocquencourt


## Commands


### Build

```
dune build
```

### Release

1. Change version in `dune-project`.
2. Update `CHANGES.md`.
3. Run `dune build`.
4. Commit
```
  git status
  git add -A
  git commit -m "message"
  git tag v0.x [--force]
  git push origin master --tags [--force]
```
5. Release
```
  dune-release tag
  dune-release distrib
  dune-release publish
  dune-release opam pkg
  dune-release opam submit
```  