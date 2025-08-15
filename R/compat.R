## We have had orderly out as 'orderly2' for long enough that there
## are quite a few existing source archives that use things like
## `orderly2::fn()` and workflows that include `orderly2`.  So we want
## to have the ability to smooth over this as best as possible.  There
## are several aspects to this:
##
## 1. we should offer to rewrite people's source code
##
## 2. we should create a stub package on the fly that contains all the
## new orderly bits, with an infrequent warning that this has happened
##
## 3. we should create a real, installable, package that wrap over
## orderly (inst/orderly2, we'll pop this onto the universe).


load_orderly2_support <- function() {
  
}
