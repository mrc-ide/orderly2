## Some constants:
local <- "local"
orphan <- "orphan"
location_reserved_name <- c(local, orphan)
location_types <- c(local, orphan, "path", "http", "packit", "custom")
re_id <- "^([0-9]{8}-[0-9]{6}-[[:xdigit:]]{8})$"


outpack_id <- function() {
  time <- Sys.time()
  sprintf("%s-%s%s",
          iso_time_str(time),
          val_to_bytes(as.numeric(time), 2),
          paste(as.character(openssl::rand_bytes(2)), collapse = ""))
}


validate_outpack_id <- function(id, call = NULL) {
  assert_scalar_character(id, call = call)
  if (!grepl(re_id, id)) {
    cli::cli_abort("Malformed id '{id}'", call = call)
  }
}


not_found_error <- function(message, data) {
  structure(list(message = message, data = data),
            class = c("not_found_error", "error", "condition"))
}


## In orderly we also add the result of git status --porcelain here,
## but we've never really used this and it's quite slow for big
## repos. More problematic, the format looks hard to replicate with
## gert (which provides a really nice data frame of status
## information) and I imagine we'd have similar issues with a python
## client. Most of the time when these things are run on a controlled
## server we only need the hash really.
##
## Also note that there might be 0, 1, or more urls depending on the
## way that the repo is configured; this feels ok really.
git_info <- function(path) {
  repo <- git_open(path)
  if (is.null(repo)) {
    return(NULL)
  }

  branch <- gert::git_branch(repo = repo)
  if (identical(branch, "HEAD")) {
    # HEAD isn't a valid branch name, and instead is what gets returned when a
    # detached head was checked out.
    branch <- NULL
  }

  list(sha = ignore_errors(gert::git_commit_id(repo = repo)),
       branch = branch,
       url = gert::git_remote_list(repo = repo)$url)
}


## Almost certainly this will get expanded, but this should work fine
## for our initial needs.
find_all_dependencies <- function(id, metadata) {
  ret <- unique(id)
  while (length(id) > 0) {
    id_new <- unlist(lapply(metadata[id], function(x) x$depends$packet),
                     FALSE, FALSE)
    id <- setdiff(id_new, ret)
    ret <- c(id, ret)
  }
  sort(ret)
}


validate_parameters <- function(parameters, call) {
  if (is.null(parameters) || length(parameters) == 0) {
    return()
  }
  assert_is(parameters, "list", call = call)
  assert_named(parameters, unique = TRUE, call = call)
  check_parameter_values(parameters, FALSE, call)
}


validate_file_from_to <- function(x, envir,
                                  name = deparse(substitute(x)),
                                  call = NULL) {
  ## Later, we can expand this to support a data.frame too perhaps?
  if (is.list(x)) {
    if (!all(vlapply(x, is_string))) {
      cli::cli_abort("All elements of '{name}' must be strings", call = call)
    }
    x <- list_to_character(x)
  }

  if (!is.character(x)) {
    cli::cli_abort(
      c(sprintf("Unexpected object type for '%s'", name),
        x = sprintf("Given object of class %s", collapseq(class(x))),
        i = "Expected a (named) character vector"),
      call = call)
  }

  to <- names(fill_missing_names(x))
  from <- unname(x)
  to_value <- string_interpolate_simple(to, envir, call)

  if (any(duplicated(to_value))) {
    dups <- unique(to_value[duplicated(to_value)])
    cli::cli_abort(
      c(sprintf("Every destination filename (in '%s') must be unique", name),
        i = sprintf("Duplicate names: %s", collapseq(dups))),
      call = call)
  }

  data_frame(here = to_value, there = from)
}
