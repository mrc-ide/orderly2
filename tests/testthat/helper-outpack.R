create_random_packet <- function(root, name = "data", parameters = NULL,
                                 id = NULL, n_files = 1) {
  src <- fs::dir_create(tempfile())
  on.exit(fs::dir_delete(src))
  for (n in seq_len(n_files)) {
    file_name <- sprintf("data%s.rds", if (n > 1) n else "")
    saveRDS(runif(10), file.path(src, file_name))
  }
  p <- outpack_packet_start_quietly(
    src, name, parameters = parameters, id = id, root = root)
  outpack_packet_end_quietly(p)
  p$id
}


create_deterministic_packet <- function(root, name = "data",
                                        parameters = NULL) {
  src <- fs::dir_create(tempfile())
  on.exit(fs::dir_delete(src))
  saveRDS(1:10, file.path(src, "data.rds"))
  p <- outpack_packet_start_quietly(
    src, name, parameters = parameters, root = root)
  outpack_packet_end_quietly(p)
  p$id
}


mock_metadata_depends <- function(id, depends = character(0)) {
  ret <- list(list(id = id, depends = data_frame(packet = depends)))
  names(ret) <- id
  ret
}


## Creates a simple chain of packets a, b, c, ... that depend on each
## other.
create_random_packet_chain <- function(root, length, base = NULL) {
  src <- fs::dir_create(tempfile())
  on.exit(fs::dir_delete(src), add = TRUE)

  id <- character()
  suppressMessages({
    for (i in seq_len(length)) {
      nm <- letters[[i]]
      p <- file.path(src, nm)
      fs::dir_create(p)
      packet <- outpack_packet_start(p, nm, root = root)
      id[[nm]] <- packet$id

      if (i == 1 && is.null(base)) {
        saveRDS(runif(10), file.path(p, "data.rds"))
      } else {
        code <- sprintf("saveRDS(readRDS('input.rds') * %d, 'data.rds')", i)
        writeLines(code, file.path(p, "script.R"))
        id_use <- if (i > 1) id[[letters[i - 1]]] else base
        outpack_packet_use_dependency(packet, id_use,
                                      c("input.rds" = "data.rds"))
        outpack_packet_run(packet, "script.R")
      }
      outpack_packet_end(packet)
    }
  })

  id
}


create_random_dependent_packet <- function(root, name, dependency_ids) {
  src <- fs::dir_create(tempfile())
  on.exit(fs::dir_delete(src), add = TRUE)

  p <- outpack_packet_start_quietly(src, name, root = root)

  len <- length(dependency_ids)
  if (len == 0) {
    saveRDS(runif(10), file.path(p, "data.rds"))
  } else {
    inputs <- paste0(sprintf("readRDS('input%s.rds')", seq_len(len)),
                     collapse = " * ")
    code <- sprintf("saveRDS(%s , 'data.rds')", inputs)
    writeLines(code, file.path(src, "script.R"))
    for (num in seq_len(len)) {
      input_name <- sprintf("input%s.rds", num)
      outpack_packet_use_dependency(p, dependency_ids[[num]],
                                    stats::setNames("data.rds", input_name))
    }
    outpack_packet_run(p, "script.R")
  }
  outpack_packet_end_quietly(p)

  p$id
}


create_temporary_root <- function(...) {
  path <- tempfile()
  withr::defer_parent(fs::dir_delete(path))
  suppressMessages(orderly_init(path, ...))
  root_open(path, require_orderly = FALSE)
}


## A really simple example that we use in a few places
create_temporary_simple_src <- function() {
  path <- tempfile()
  withr::defer_parent(fs::dir_delete(path))
  fs::dir_create(path)

  path <- tempfile()
  fs::dir_create(path)
  writeLines(c(
    "d <- read.csv('data.csv')",
    "png('zzz.png')",
    "plot(d)",
    "dev.off()"),
    file.path(path, "script.R"))
  write.csv(data.frame(x = 1:10, y = runif(10)),
            file.path(path, "data.csv"),
            row.names = FALSE)

  path
}


temp_file <- function() {
  path <- tempfile()
  withr::defer_parent({
    if (fs::file_exists(path)) {
      fs::file_delete(path)
    }
  })
  path
}


helper_add_git <- function(path) {
  gert::git_init(path)
  if (file.exists(file.path(path, ".outpack"))) {
    suppressMessages(orderly_gitignore_update("(root)", path))
  }
  gert::git_add(".", repo = path)
  user <- "author <author@example.com>"
  sha <- gert::git_commit("initial", author = user, committer = user,
                          repo = path)
  branch <- gert::git_branch(repo = path)
  url <- "https://example.com/git"
  gert::git_remote_add(url, repo = path)
  list(user = user, branch = branch, sha = sha, url = url)
}


## This matches the old semantics of outpack_root, and is used to
## create a root that does not have the orderly bits.
outpack_init_no_orderly <- function(...) {
  path <- orderly_init_quietly(...)
  fs::file_delete(file.path(path, "orderly_config.yml"))
  outpack_root$new(path)
}


outpack_packet_run <- function(packet, script, envir = NULL) {
  if (is.null(envir)) {
    envir <- new.env(parent = .GlobalEnv)
  }
  packet <- check_current_packet(packet)
  withr::with_dir(packet$path,
                  source_echo(script, envir = envir, echo = FALSE))
}


outpack_packet_start_quietly <- function(...) {
  suppressMessages(outpack_packet_start(...))
}


outpack_packet_end_quietly <- function(...) {
  suppressMessages(outpack_packet_end(...))
}

forcibly_truncate_file <- function(path) {
  permissions <- fs::file_info(path)$permissions
  fs::file_delete(path)
  fs::file_create(path)
  fs::file_chmod(path, permissions)
}

#' Scrub packets from an output.
#'
#' This returns a transformation suitable to be passed to `expect_snapshot`.
#' The specified packet IDs are removed from the output, and replaced with
#' stable strings of the same length.
#'
#' @param ... the list of packet IDs to remove from the output.
#' @noRd
scrub_packets <- function(...) {
  ids <- c(...)
  replacements <- sprintf("19700101-000000-%08x", seq_along(ids))
  names(replacements) <- ids
  function(x) stringr::str_replace_all(x, replacements)
}
