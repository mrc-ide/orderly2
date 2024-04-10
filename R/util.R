`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


is_directory <- function(x) {
  fs::is_dir(x)
}


is_call <- function(x, name) {
  is.recursive(x) && is.name(x[[1]]) && as.character(x[[1]]) == name
}


is_assignment <- function(x) {
  if (!(is.recursive(x) && is.name(x[[1]]))) {
    return(FALSE)
  }
  as.character(x[[1]]) %in% c("<-", "=", "<<-")
}


is_orderly_ns_call <- function(x) {
  is.recursive(x) && is_call(x[[1]], "::") &&
    as.character(x[[1]][[2]]) == "orderly2"
}


vlapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, logical(1), ...)
}


vnapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, numeric(1), ...)
}


set_names <- function(x, nms) {
  if (length(nms) == 1 && length(x) != 1) {
    if (is.null(x)) {
      return(NULL)
    }
    nms <- rep_len(nms, length(x))
  }
  names(x) <- nms
  x
}


set_class <- function(x, cls) {
  class(x) <- cls
  x
}


dir_ls_local <- function(path, ...) {
  withr::with_dir(path, fs::dir_ls(path = ".", ...))
}


scalar <- function(x) {
  jsonlite::unbox(x)
}


rep_along <- function(x, v) {
  rep_len(x, length(v))
}


data_frame <- function(...) {
  ret <- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
  rownames(ret) <- NULL
  ret
}


squote <- function(x) {
  sprintf("'%s'", x)
}


dquote <- function(x) {
  sprintf('"%s"', x)
}


drop_null <- function(x, empty) {
  i <- vlapply(x, is.null)
  if (all(i)) empty else x[!i]
}


replace_ragged <- function(x, i, values) {
  ret <- as.list(x)
  ret[i] <- values
  unlist(ret, FALSE, FALSE)
}


## Not R environments, but system environment variables
resolve_envvar <- function(x, variables, used_in, error = TRUE) {
  if (!is.null(variables)) {
    withr::local_envvar(variables)
  }

  make_name <- function(x, parent) {
    if (is.null(names(x))) {
      sprintf("%s[[%d]]", parent, seq_along(x))
    } else {
      sprintf("%s$%s", parent, names(x))
    }
  }

  re <- "^\\$([0-9A-Z_]+)$"
  resolve <- function(x, name) {
    if (is.recursive(x)) {
      x[] <- Map(resolve, x, make_name(x, name))
    } else if (is.character(x) && length(x) == 1 && grepl(re, x)) {
      sys_getenv(sub(re, "\\1", x), name, error, default = NA_character_)
    } else {
      x
    }
  }

  Map(resolve, x, make_name(x, used_in))
}


vcapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, character(1), ...)
}


## TODO: also replace copy_shared with this
expand_dirs <- function(paths, workdir) {
  if (length(paths) == 0) {
    return(character())
  }
  withr::local_dir(workdir)
  i <- is_directory(paths)
  if (any(i)) {
    contents <- lapply(paths[i], function(p) {
      as.character(fs::dir_ls(p, all = TRUE, type = "file", recurse = TRUE))
    })
    paths <- replace_ragged(paths, i, contents)
  }
  paths
}


copy_files <- function(src, dst, files, overwrite = FALSE) {
  fs::dir_create(unique(file.path(dst, dirname(files))))
  fs::file_copy(file.path(src, files),
                file.path(dst, files),
                overwrite = overwrite)
}


ignore_errors <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}


current_orderly_version <- function() {
  utils::packageVersion("orderly2")
}


yaml_read <- function(filename) {
  catch_yaml <- function(e) {
    stop(sprintf("while reading '%s'\n%s", filename, e$message),
         call. = FALSE)
  }
  tryCatch(yaml_load(read_lines(filename, warn = FALSE)),
           error = catch_yaml)
}


yaml_load <- function(string) {
  ## More restrictive true/false handling.  Only accept if it maps to
  ## full (true|yes) / (false|no):
  handlers <- list(
    "bool#yes" = function(x) if (tolower(x) %in% c("true", "yes")) TRUE else x,
    "bool#no" = function(x) if (tolower(x) %in% c("false", "no")) FALSE else x)
  yaml::yaml.load(string, handlers = handlers)
}


file_exists <- function(..., workdir = NULL) {
  files <- c(...)
  if (!is.null(workdir)) {
    assert_scalar_character(workdir)
    owd <- setwd(workdir) # nolint
    on.exit(setwd(owd)) # nolint
  }
  fs::file_exists(files)
}


check_fields <- function(x, name, required, optional) {
  msg <- setdiff(required, names(x))
  if (length(msg) > 0L) {
    stop(sprintf("Fields missing from %s: %s",
                 name, paste(msg, collapse = ", ")))
  }
  extra <- setdiff(names(x), c(required, optional))
  if (length(extra) > 0L) {
    stop(sprintf("Unknown fields in %s: %s",
                 name, paste(extra, collapse = ", ")))
  }
}


list_to_character <- function(x, named = TRUE) {
  vcapply(x, identity, USE.NAMES = named)
}


list_to_numeric <- function(x, named = TRUE) {
  vnapply(x, identity, USE.NAMES = named)
}


sys_getenv <- function(x, used_in, error = TRUE, default = NULL) {
  v <- Sys.getenv(x, NA_character_)
  if (is.na(v) || !nzchar(v)) {
    if (error) {
      reason <- if (!nzchar(v)) "empty" else "not set"
      stop(sprintf("Environment variable '%s' is %s\n\t(used in %s)",
                   x, reason, used_in), call. = FALSE)
    } else {
      v <- default
    }
  }
  v
}


read_lines <- function(...) {
  paste(readLines(...), collapse = "\n")
}


system_file <- function(...) {
  system.file(..., mustWork = TRUE)
}


## Designed for use reading json files as a single string and dropping
## and trailing whitespace. The warn = FALSE arg prevents an annoying
## warning about a lack of a trailing newline.
read_string <- function(path) {
  trimws(paste(readLines(path, warn = FALSE), collapse = "\n"))
}


orderly2_file <- function(path) {
  system_file(path, package = "orderly2")
}


## We could rewrite this non-recurively, this just comes from orderly
find_file_descend <- function(target, start = ".", limit = "/") {
  root <- normalise_path(limit)
  start <- normalise_path(start)

  f <- function(path) {
    if (file.exists(file.path(path, target))) {
      return(path)
    }
    if (normalise_path(path) == root) {
      return(NULL)
    }
    parent <- normalise_path(file.path(path, ".."))
    if (parent == path) {
      return(NULL)
    }
    Recall(parent)
  }
  ret <- f(start)
  if (!(is.null(ret))) {
    ret <- normalise_path(ret)
  }
  ret
}


val_to_bytes <- function(x, nbytes) {
  n <- round((x %% 1) * 256 ^ nbytes)
  paste(packBits(intToBits(n))[nbytes:1], collapse = "")
}


iso_time_str <- function(time = Sys.time()) {
  strftime(time, "%Y%m%d-%H%M%S", tz = "UTC")
}


time_to_num <- function(time = Sys.time()) {
  as.numeric(time)
}


num_to_time <- function(num) {
  as.POSIXct(num, origin = "1970-01-01", tz = "UTC")
}


empty_time <- function() {
  num_to_time(numeric(0))
}


to_json <- function(x, schema = NULL, auto_unbox = FALSE, ...) {
  json <- jsonlite::toJSON(x, auto_unbox = auto_unbox,
                           json_verbatim = TRUE, na = "null", null = "null",
                           ...)
  if (should_validate_schema(schema)) {
    load_schema(schema)$validate(json, error = TRUE)
  }
  json
}


from_json <- function(x, ...) {
  jsonlite::fromJSON(x, simplifyVector = FALSE, ...)
}


as_json <- function(str) {
  assert_scalar_character(str)
  structure(str, class = "json")
}


last <- function(x) {
  x[[length(x)]]
}


collector <- function() {
  envir <- new.env(parent = emptyenv())
  envir$data <- list()
  list(
    add = function(x) {
      envir$data <- c(envir$data, list(x))
    },
    get = function() {
      envir$data
    }
  )
}


string_starts_with <- function(sub, str) {
  substr(str, 1, nchar(sub)) == sub
}


string_drop_prefix <- function(sub, str) {
  substr(str, nchar(sub) + 1, nchar(str))
}


## We might want to return information about why these failed, later,
## so that better error messages can be created.
near_match <- function(x, possibilities, threshold = 2, max_matches = 5) {
  if (length(possibilities) == 0) {
    return(character())
  }
  i <- tolower(x) == tolower(possibilities)
  if (any(i)) {
    possibilities[i]
  } else {
    d <- set_names(drop(utils::adist(x, possibilities, ignore.case = TRUE)),
                   possibilities)
    utils::head(names(sort(d[d <= threshold])), max_matches)
  }
}


near_matches <- function(x, ...) {
  set_names(lapply(x, near_match, ...), x)
}


error_near_match <- function(title, x, hint, join, possibilities) {
  err <- sprintf("%s: %s", title, paste(squote(x), collapse = ", "))
  near <- near_matches(x, possibilities)
  i <- lengths(near) > 0
  if (any(i)) {
    near_str <- vcapply(which(lengths(near) > 0), function(i) {
      sprintf("'%s': %s %s",
              names(near)[[i]],
              join,
              paste(squote(near[[i]]), collapse = ", "))
    })
    err <- c(err,
             i = hint,
             set_names(near_str, rep("*", length(near_str))))
  }
  err
}


check_symbol_from_str <- function(str, name) {
  assert_scalar_character(str, name)
  dat <- strsplit(str, "(?<=[^:])::(?=[^:])", perl = TRUE)[[1]]
  if (length(dat) != 2) {
    stop(sprintf("Expected fully qualified name for '%s'", name))
  }
  list(namespace = dat[[1]], symbol = dat[[2]])
}


collapseq <- function(x, last = NULL) {
  collapse(squote(x), last)
}


collapse <- function(x, last = NULL) {
  if (!is.null(last) && length(x) > 1) {
    paste0(x, rep(c(", ", last, ""), c(length(x) - 2, 1, 1)), collapse = "")
  } else {
    paste(x, collapse = ", ")
  }
}


## There are so many ways of doing this, none of them are amazing; I
## don't really want to use glue for this because it implies we can do
## all the things that glue does within the string substitution, which
## we can't do. Instead, we want something much much simpler that can
## be used for constructing paths but is fairly intuitive.
##
## I've gone with a shell-expansion like ${var} syntax here. If this
## is not suitable, users can always do their own substitutions.
string_interpolate_simple <- function(x, envir, call = NULL) {
  if (inherits(x, "AsIs") || !any(grepl("${", x, fixed = TRUE))) {
    return(x)
  }
  vcapply(x, string_interpolate_simple1, envir, call, USE.NAMES = FALSE)
}


string_interpolate_simple1 <- function(x, envir, call) {
  re <- "\\$\\{\\s*(.*?)\\s*\\}"

  m <- gregexec(re, x)[[1L]]
  if (length(m) == 1 && m < 0) {
    return(x)
  }

  m_end <- m + attr(m, "match.length") - 1L
  start <- m[1, ]
  end <- m_end[1, ]
  from <- substr(rep(x, ncol(m)), m[1, ], m_end[1, ])
  to <- substr(rep(x, ncol(m)), m[2, ], m_end[2, ])

  to_value <- lapply(to, function(el) {
    value <- tryCatch(
      get(el, envir),
      error = function(e) {
        cli::cli_abort(
          c(sprintf("Failed to find value for '%s'", el),
            i = sprintf("Was interpolating string '%s'", quote_braces(x))),
          call = call)
      })
    tryCatch(
      as.character(value),
      error = function(e) {
        cli::cli_abort(
          c(sprintf("Failed to convert '%s' to character", el),
            x = quote_braces(e$message),
            i = sprintf("Was interpolating string '%s'", quote_braces(x))),
          call = call)
      })
  })

  if (any(err <- lengths(to_value) != 1)) {
    msg <- sprintf("Failed when retrieving '%s' which has length %d",
                   quote_braces(to[err]), lengths(to_value)[err])
    cli::cli_abort(
      c("Failed to convert string interpolation variable to string",
        set_names(msg, rep("x", length(msg))),
        i = sprintf("Was interpolating string '%s'", quote_braces(x)),
        i = "All values in ${{...} must refer to strings"),
      call = call)
  }

  to_value <- list_to_character(to_value)
  if (any(err <- grepl(re, to_value))) {
    msg <- sprintf("Tried to substitute '%s' to '%s'",
                   quote_braces(from[err]),
                   quote_braces(to_value[err]))
    cli::cli_abort(
      c("Can't perform recursive string interpolation",
        set_names(msg, rep("x", length(msg))),
        i = sprintf("Was interpolating string '%s'", quote_braces(x)),
        i = "Don't use '${{...}' within the values you are substituting to"),
      call = call)
  }

  for (i in seq_along(start)) {
    x <- sub(from[[i]], to_value[[i]], x, fixed = TRUE)
  }

  x
}


quote_braces <- function(x) {
  gsub("{", "{{", x, fixed = TRUE)
}


git_open <- function(path) {
  tryCatch(gert::git_open(path), error = function(e) NULL)
}


path_is_git_ignored <- function(path, root) {
  repo <- git_open(root)
  if (is.null(repo)) {
    rep_len(NA, length(path))
  } else {
    gert::git_ignore_path_is_ignored(path, repo)
  }
}


row_any <- function(x) {
  if (is.null(dim(x))) x else apply(x, 1, any)
}


delete_empty_directories <- function(path) {
  withr::local_dir(path)
  paths <- fs::dir_ls(".", type = "directory", recurse = TRUE)
  paths <- setdiff(paths[order(nchar(paths), decreasing = TRUE)], path)
  for (p in paths) {
    if (length(fs::dir_ls(p, all = TRUE)) == 0) {
      fs::dir_delete(p)
    }
  }
  invisible(p)
}


with_trailing_slash <- function(x) {
  sub("(?<![/])$", "/", x, perl = TRUE)
}


is_simple_scalar_atomic <- function(x) {
  length(x) == 1 && is_simple_atomic(x)
}


is_simple_atomic <- function(x) {
  (is.character(x) || is.numeric(x) || is.logical(x)) && !anyNA(x)
}


is_string <- function(x) {
  is.character(x) && length(x) == 1L
}


normalise_path <- function(x) {
  normalizePath(x, winslash = "/", mustWork = TRUE)
}


pairs <- function(a) {
  i <- which(upper.tri(diag(length(a))), TRUE)
  Map(c, a[i[, 1]], a[i[, 2]], USE.NAMES = FALSE)

}


pretty_bytes <- function(n) {
  if (n < 1e3) {
    unit <- "B"
  } else if (n < 1e6) {
    unit <- "kB"
    n <- n / 1e3
  } else {
    unit <- "MB"
    n <- n / 1e6
  }
  paste(prettyNum(round(n, 1), big.mark = ","), unit)
}


file_canonical_case <- function(path, workdir) {
  if (length(path) != 1) {
    return(vcapply(path, file_canonical_case, workdir, USE.NAMES = FALSE))
  }
  stopifnot(!fs::is_absolute_path(path))
  path_split <- tolower(fs::path_split(path)[[1]])
  base <- workdir
  ret <- character(length(path_split))
  for (i in seq_along(path_split)) {
    pos <- dir(base)
    j <- which(path_split[[i]] == tolower(pos))
    if (length(j) != 1) {
      return(NA_character_)
    }
    ret[[i]] <- pos[[j]]
    base <- file.path(base, pos[[j]])
  }
  paste(ret, collapse = "/")
}


saverds_atomic <- function(data, path, allow_fail = FALSE) {
  tmp <- tempfile(pattern = sub("\\.rds", "", basename(path)),
                  tmpdir = dirname(path),
                  fileext = ".rds")
  saveRDS(data, tmp)
  if (allow_fail) {
    tryCatch(
      fs::file_move(tmp, path),
      error = function(e) unlink(tmp))
  } else {
    tryCatch(
      fs::file_move(tmp, path),
      finally = unlink(tmp))
  }
}


paths_are_identical <- function(x, y) {
  fs::path_norm(x) == fs::path_norm(y)
}


is_testing <- function() {
  # Copied from testthat, to avoid having the package as a run-time dependency.
  # https://github.com/r-lib/testthat/blob/fe50a22/R/test-env.R#L20
  identical(Sys.getenv("TESTTHAT"), "true")
}

#' Given a character vector or list, missing names are filled using the value.
fill_missing_names <- function(x) {
  if (!is.null(x)) {
    if (is.null(names(x))) {
      names(x) <- list_to_character(x)
    } else {
      missing <- is.na(names(x)) | names(x) == ""
      names(x)[missing] <- x[missing]
    }
  }
  x
}
