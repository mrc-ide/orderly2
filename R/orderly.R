orderly_run <- function(name, parameters = NULL, envir = NULL,
                        root = NULL, locate = TRUE) {
  root <- orderly_root(root, locate)
  src <- file.path(root$path, "src", name)

  envir <- envir %||% .GlobalEnv
  assert_is(envir, "environment")

  dat <- orderly_read(src)

  ## we should provide nice validation here for the things that we can
  ## reason about, we'll need to do this *again* later though, but
  ## this way we can fail early.
  orderly_validate(dat, src)

  id <- outpack::outpack_id()
  path <- file.path(root$path, "draft", name, id)
  fs::dir_create(path)

  ## For now, let's just copy *everything* over. Later we'll get more
  ## clever about this and avoid copying over artefacts and
  ## dependencies.
  all_files <- fs::dir_ls(src, all = TRUE)
  if (length(dat) == 0) {
    fs::file_copy(all_files, path)
  } else {
    ## This will require some care in the case where we declare
    ## directory artefacts, not totally sure what we'll want to do
    ## there, but it probably means that we need to write a dir walker
    ## - let's ignore that detail for now and come up with some
    ## adverserial cases later.
    exclude <- unlist(lapply(dat$artefacts, "[[", "files"), TRUE, FALSE)
    to_copy <- union(dat$resources,
                     setdiff(fs::dir_ls(src, all = TRUE), exclude))
    fs::file_copy(to_copy, path)
  }

  ## It will be nice here to come up with static inputs and static
  ## outputs as we can mark these as immutable and required.
  schema <- custom_metadata_schema()

  browser()

  withCallingHandlers({
    p <- outpack::outpack_packet_start(path, name, parameters = parameters,
                                       id = id, root = root$outpack)
    p$orderly3 <- list()
    if (!is.null(parameters)) {
      list2env(parameters, envir)
    }

    if (length(dat$resources) > 0) { # outpack should cope with this...
      outpack::outpack_packet_file_mark(dat$resources, "immutable", packet = p)
    }
    outpack::outpack_packet_run("orderly.R", envir, packet = p)

    ## It's not that hard here to detect changed files, but this is
    ## likely somewhat platform dependent and on windows the accuracy
    ## of the clock for this stuff is not perfect.
    ##
    ## files_end <- fs::dir_ls(src, all = TRUE, recurse = TRUE, type = "file")
    ## files_info <- fs::file_info(files_end)
    ## files_is_new <-
    ##   !(files_end %in% files_start) |
    ##   files_info$modification_time > t0 |
    ##   files_info$birth_time > t0 |
    ##   files_info$change_time > t0
    if (!is.null(p$orderly3$artefacts)) {

    }

    ## At this point we want to know what was really an output; in the
    ## case where no static analysis was possible, we could look at
    ## timestamps I think.

    ## outpack::outpack_packet_add_custom("orderly", to_json(custom_metadata),
    ##                                    schema)

    outpack::outpack_packet_end()
    unlink(path, recursive = TRUE)
  }, error = function(e) {
    ## Eventually fail nicely here with mrc-3379
    outpack::outpack_packet_cancel()
  })

  id
}


get_active_packet <- function() {
  NULL
}
