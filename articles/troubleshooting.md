# Troubleshooting

This page describes some (well, one currently) problems that you might
hit. It’s designed to be linked to from error messages or bug reports.

``` r
library(orderly)
```

## Outpack files accidentally committed to git

As discussed in [the `orderly`
introduction](https://mrc-ide.github.io/orderly/articles/introduction.html#interaction-with-version-control),
you do not want to commit any files from `.outpack/`, `drafts/` or
`archive/` (if used) to git as this will create all sorts of problems
down the line.

If you were directed here, it is probably because you have ended up with
these files in git and want to undo this situation. The least painful
way depends on your situation.

We have now put in guard rails to try and prevent this happening, but it
could still happen to you if you modify the `.gitignore` file or
force-add files for example.

Once you are in this situation, `orderly` will shout at you:

``` r
orderly_run("data")
## Error in `orderly_run()`:
## ! Detected 6 outpack files committed to git
## ✖ Detected files were found in '.outpack/' and 'simple/'
## ℹ For tips on resolving this, please see
##   <https://mrc-ide.github.io/orderly/articles/troubleshooting.html>
## ✖ Found: .outpack/config.json, .outpack/index/outpack.rds,
##   .outpack/location/local/20260121-100004-d58b21e2,
##   .outpack/metadata/20260121-100004-d58b21e2,
##   simple/data/20260121-100004-d58b21e2/data.R, and
##   simple/data/20260121-100004-d58b21e2/data.rds
## ℹ To turn this into a warning and continue anyway set the option
##   'orderly.git_error_is_warning' to TRUE by running
##   `options(orderly.git_error_is_warning = TRUE)`
## ℹ To disable this check, set the option 'orderly.git_error_ignore' to TRUE by
##   running `options(orderly.git_error_ignore = TRUE)`
```

which may have directed you to this very page. If you just want to
continue working anyway, then run the suggested command:

``` r
options(orderly.git_error_is_warning = TRUE)
```

after which things will work with a warning the first time that session:

``` r
orderly_run("data")
## Warning in orderly_run("data"): Detected 6 outpack files committed to git
## ✖ Detected files were found in '.outpack/' and 'simple/'
## ℹ For tips on resolving this, please see
##   <https://mrc-ide.github.io/orderly/articles/troubleshooting.html>
## ✖ Found: .outpack/config.json, .outpack/index/outpack.rds,
##   .outpack/location/local/20260121-100004-d58b21e2,
##   .outpack/metadata/20260121-100004-d58b21e2,
##   simple/data/20260121-100004-d58b21e2/data.R, and
##   simple/data/20260121-100004-d58b21e2/data.rds
## This warning is displayed once per session.
## ✔ Wrote '.gitignore'
## ℹ Starting packet 'data' `20260121-100005-441147ff` at 2026-01-21 10:00:05.270219
## > orderly_description(
## +   display = "A demo data set")
## > x <- jitter(1:30)
## > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
## > d <- data.frame(x, y)
## > orderly_artefact("data.rds", description = "A synthetic dataset")
## > saveRDS(d, "data.rds")
## ✔ Finished running data.R
## ℹ Finished 20260121-100005-441147ff at 2026-01-21 10:00:05.308936 (0.03871632 secs)
## [1] "20260121-100005-441147ff"
```

subsequent calls will not display the warning:

``` r
orderly_run("data")
## ℹ Starting packet 'data' `20260121-100005-69b95327` at 2026-01-21 10:00:05.417369
## > orderly_description(
## +   display = "A demo data set")
## > x <- jitter(1:30)
## > y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
## > d <- data.frame(x, y)
## > orderly_artefact("data.rds", description = "A synthetic dataset")
## > saveRDS(d, "data.rds")
## ✔ Finished running data.R
## ℹ Finished 20260121-100005-69b95327 at 2026-01-21 10:00:05.440641 (0.02327228 secs)
## [1] "20260121-100005-69b95327"
```

The rest of this section discusses how you might permanently fix the
issue. The solution you might pick depends on what you care about.

### I don’t care about my git history at all

This is the case if you have just started a project, and are not yet
collaborating on it with anyone else (or if that person is willing to
re-clone their sources). The simplest thing to do is:

1.  Delete the `.git` directory entirely
2.  Run `orderly_gitignore_update("(root)")` to set up a reasonable
    `.gitignore` that will prevent this situation happening again
3.  Run `git add .` to add everything back in (review this with
    `git status` to make sure you’re happy)
4.  Run `git commit -m "Initial commit"` to create a single commit that
    contains all the files in currently in your repo *with no history*,
    and also with no `.outpack` files

If you have previously pushed this repo to GitHub or similar then you
will need to set that up again

5.  `git remote add origin https://github.com/user/repo` (replacing
    `user/repo` with your path, or using `git@github.com:user/repo` if
    you use ssh to talk with GitHub)
6.  `git branch -M main` assuming you are using `main` for your default
    branch, which is now most common
7.  `git push --force -u origin main`

Note that this is **destructive** and will require coordination with any
collaborators as you have changed history.

### I just want this to go away and nothing I have committed is very large

If you do care about your history, but you also have only committed a
few files (e.g., you have committed files from `.outpack/` which are
small but not a large 100MB file in `archive/` that is preventing you
[pushing to
GitHub](https://docs.github.com/en/repositories/working-with-files/managing-large-files/about-large-files-on-github))
then you could just delete the offending files from git without updating
the history, or affecting your local copies.

1.  Run `git rm --cached .outpack` (repeating with `draft` and `archive`
    as needed)
2.  Run `orderly_gitignore_update("(root)")` to set up a reasonable
    `.gitignore` that will prevent this situation happening again
3.  Run `git add .gitignore` to also stage this
4.  Run `git commit -m "Delete unwanted outpack files"`

You can then push this without any issues.

### I care about my history but want this stuff gone

If you are working on a branch, and the unwanted files were committed on
that branch, the simplest thing to do is to copy the changes you have
made somewhere safe, create a new branch against the current `main` and
copy those changes over there. You could do this somewhat automatically
by generating and applying a patch:

    git diff -- src > changes.patch
    git checkout main
    git checkout -b changes-attempt2
    git apply changes.patch
    git push -u origin changes-attempt2

If the unwanted files have been committed onto your default branch, then
you will have to do some potentially gory history rewriting. See [this
StackOverflow
question](https://stackoverflow.com/questions/10067848/remove-folder-and-its-contents-from-git-githubs-history),
the [git docs](https://git-scm.com/docs/git-filter-branch#_warning) and
[the currently recommended tool for doing
this](https://github.com/newren/git-filter-repo/). Good luck!
