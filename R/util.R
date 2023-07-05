`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


is_directory <- function(x) {
  fs::is_dir(x)
}


is_call <- function(x, name) {
  is.recursive(x) && is.name(x[[1]]) && as.character(x[[1]]) == name
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
  names(x) <- nms
  x
}


dir_ls_local <- function(path, ...) {
  withr::with_dir(path, fs::dir_ls(path = ".", ...))
}


scalar <- function(x) {
  jsonlite::unbox(x)
}


to_json <- function(obj, pretty = FALSE) {
  jsonlite::toJSON(obj, pretty = pretty, auto_unbox = FALSE, na = "null",
                   null = "null", json_verbatim = TRUE, digits = NA)
}


rep_along <- function(x, v) {
  rep_len(x, length(v))
}


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}


squote <- function(x) {
  sprintf("'%s'", x)
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


resolve_env <- function(x, env, used_in, error = TRUE) {
  if (!is.null(env)) {
    withr::local_envvar(env)
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


## TODO: also replace copy_global with this
expand_dirs <- function(paths, workdir) {
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
    "bool#yes" = function(x)
      if (tolower(x) %in% c("true", "yes")) TRUE else x,
    "bool#no" = function(x)
      if (tolower(x) %in% c("false", "no")) FALSE else x)
  yaml::yaml.load(string, handlers = handlers)
}


file_exists <- function(..., check_case = FALSE, workdir = NULL,
                        force_case_check = FALSE) {
  files <- c(...)
  if (!is.null(workdir)) {
    assert_scalar_character(workdir)
    owd <- setwd(workdir) # nolint
    on.exit(setwd(owd)) # nolint
  }
  exists <- file.exists(files)

  if (check_case) {
    incorrect_case <- logical(length(files))
    if (!is_linux() || force_case_check) {
      incorrect_case[exists] <-
        !vlapply(files[exists], file_has_canonical_case)
      if (any(incorrect_case)) {
        correct <- vcapply(files[incorrect_case], file_canonical_case)
        names(correct) <- files[incorrect_case]
        attr(exists, "incorrect_case") <- incorrect_case
        attr(exists, "correct_case") <- correct
        exists[incorrect_case] <- FALSE
      }
    }
  }

  exists
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
