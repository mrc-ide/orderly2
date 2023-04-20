##' Declare orderly parameters. You should only have one call to this
##' within your file, though this is not enforced! Typically you'd put
##' it very close to the top, though the order does not really matter.
##' Parameters are scalar atomic values (e.g. a string, number or
##' boolean) and defaults must be present literally (i.e., they may
##' not come from a variable itself). Provide `NULL` if you do not
##' have a default, in which case this parameter will be required.
##'
##' @title Declare orderly parameters
##'
##' @param ... Any number of parameters
##'
##' @return Undefined
##'
##' @export
orderly_parameters <- function(...) {
  p <- get_active_packet()
  if (is.null(p)) {
    pars <- static_orderly_parameters(list(...))
    env <- parent.frame()
    check_parameters_interactive(env, pars)
  }

  invisible()
}


static_orderly_parameters <- function(args) {
  if (length(args) == 0L) {
    return(NULL)
  }
  assert_named(args, unique = TRUE, name = "Arguments to 'orderly_parameters'")
  check_parameter_values(args, TRUE)

  args
}


current_orderly_parameters <- function(src, env) {
  dat <- orderly_read(src)
  pars <- static_orderly_parameters(dat$parameters)
  check_parameters_interactive(env, pars)
}


##' Declare that a file, or group of files, are an orderly
##' resource. By explicitly declaring files as resources orderly will
##' mark the files as immutable inputs and validate that your analysis
##' does not modify them when run with [orderly_run()]
##'
##' @title Declare orderly resources
##'
##' @param files Any number of names of files
##'
##' @return Undefined
##'
##' @export
orderly_resource <- function(files) {
  ## TODO: an error here needs to throw a condition that we can easily
  ## handle and or defer; that's not too hard to do though - convert
  ## the error into something with a special class, perhaps make it a
  ## warning in normal R and then register a handler for it in the
  ## main run.
  assert_character(files)
  assert_file_exists(files)

  p <- get_active_packet()
  if (!is.null(p) && length(files) > 0L) {
    outpack::outpack_packet_file_mark(files, "immutable", packet = p)
    p$orderly3$resources <- c(p$orderly3$resources, files)
  }

  invisible()
}


static_orderly_resource <- function(args) {
  list(files = static_character_vector(args$files))
}


##' Declare an artefact. By doing this you turn on a number of orderly
##' features; see Details below. You can have multiple calls to this
##' function within your orderly script.
##'
##' (1) files matching this will *not* be copied over from the src
##' directory to the draft directory unless they are also listed as a
##' resource with [orderly_resource()]. This feature is only enabled
##' if you call this function from the top level of the orderly script
##' and if it contains only string literals (no variables).
##'
##' (2) if your script fails to produce these files, then
##' [orderly_run()] will fail, guaranteeing that your task does really
##' produce the things you need it to.
##'
##' (3) within the final metadata, your artefacts will have additional
##' metadata; the description that you provide and a grouping
##'
##' @title Declare orderly artefacts
##'
##' @param description The name of the artefact
##'
##' @param files The files within this artefact
##'
##' @return Undefined
##'
##' @export
orderly_artefact <- function(description, files) {
  assert_scalar_character(description)
  assert_character(files) # also check length >0 ?

  p <- get_active_packet()
  if (!is.null(p)) {
    artefact <- list(description = description, files = files)
    p$orderly3$artefacts <- c(p$orderly3$artefacts, list(artefact))
  }

  invisible()
}


static_orderly_artefact <- function(args) {
  list(description = static_character_vector(args$description),
       files = static_character_vector(args$files))
}


##' Declare a dependency on another packet
##'
##' @title Declare a dependency
##'
##' @param name The name of the packet to depend on
##'
##' @param query The query to search for; often this will simply be
##'   the string `latest`, indicating the most recent version. You may
##'   want a more complex query here though.
##'
##' @param use A named character vector of filenames to copy from the
##'   upstream packet. The name corresponds to the destination name,
##'   so c(here.csv = "there.csv") will take the upstream file
##'   `there.csv` and copy it over as `here.csv`.
##'
##' @param as (Optional) A name to refer to the results of running
##'   this query. The name can be used in subsequent calls to
##'   `orderly_depends` as a subquery.
##'
##' @return Undefined
##' @export
orderly_dependency <- function(name, query, use, as = NULL) {
  assert_scalar_character(name)
  assert_scalar_character(query)
  if (!is.null(as)) {
    assert_scalar_character(as)
  }

  assert_character(use)
  assert_named(use, unique = TRUE)

  ctx <- orderly_context()
  id <- outpack::outpack_query(query, ctx$parameters, name = name,
                               require_unpacked = TRUE, root = ctx$root)
  if (ctx$is_active) {
    outpack::outpack_packet_use_dependency(id, use, ctx$packet)
  } else {
    outpack::outpack_copy_files(id, use, ctx$path, ctx$root)
  }

  if (!is.null(as)) {
    if (as %in% ls(env)) {
      stop(paste0("Trying to save query result as '", as,
                  "' but a variable with that name already exists."))
    }

    id_terms <- lapply(id, function(x) bquote(id == .(x)))
    subquery <- id_terms[[1]]
    for (term in id_terms[-1]) {
      subquery <- call("||", subquery, term)
    }
    env[[as]] <- subquery
  }

  invisible()
}


run_outpack_query <- function(query, parameters, env, name, root) {
  subquery <- as.list(env)
  if (length(subquery) == 0) {
    subquery <- NULL
  }
  ids <- outpack::outpack_query(query, parameters, name = name,
                                require_unpacked = TRUE,
                                subquery = subquery,
                                root = root)
  if (is.na(ids) || length(ids) == 0) {
    ## TODO: Include query in the error message
    stop("Found no packets")
  }
  ids
}


static_orderly_dependency <- function(args) {
  name <- args$name
  query <- args$query
  use <- args$use

  name <- static_string(name)
  use <- static_character_vector(use)
  ## TODO: allow passing expressions directly in, that will be much
  ## nicer, but possibly needs some care as we do want a consistent
  ## approach to NSE here
  query <- static_string(query)
  if (is.null(name) || is.null(use) || is.null(query)) {
    return(NULL)
  }
  list(name = name, query = query, use = use)
}


##' Copy global resources into a packet directory. You can use this to
##' share common resources (data or code) between multiple packets.
##' Additional metadata will be added to keep track of where the files
##' came from.  Using this function requires that the orderly
##' repository has global resources enabled, with a
##' `global_resources:` section in the `orderly_config.yml`; an error
##' will be raised if this is not configured.
##'
##' @title Copy global resources into a packet directory
##'
##' @param ... Named arguments corresponding to global resources to
##'   copy. The name will be the destination filename, while the value
##'   is the filename within the global resource directory.
##'
##' @return Undefined
##' @export
orderly_global_resource <- function(...) {
  files <- validate_global_resource(list(...))
  ctx <- orderly_context()

  files <- copy_global(ctx$root, ctx$path, ctx$config, files)
  if (ctx$is_active) {
    outpack::outpack_packet_file_mark(files$here, "immutable",
                                      packet = ctx$packet)
    ctx$packet$orderly3$global_resources <-
      rbind(ctx$packet$orderly3$global_resources, files)
  }

  invisible()
}


validate_global_resource <- function(args) {
  if (length(args) == 0) {
    stop("orderly_global_resource requires at least one argument")
  }
  assert_named(args, unique = TRUE)
  is_invalid <- !vlapply(args, function(x) is.character(x) && length(x) == 1)
  if (any(is_invalid)) {
    stop(sprintf("Invalid global resource %s: entries must be strings",
                 paste(squote(names(args)[is_invalid]), collapse = ", ")))
  }
  list_to_character(args)
}


copy_global <- function(path_root, path_dest, config, files) {
  if (is.null(config$global_resources)) {
    stop(paste("'global_resources' is not supported;",
               "please edit orderly_config.yml to enable"),
         call. = FALSE)
  }

  here <- names(files)
  there <- unname(files)

  global_path <- file.path(path_root, config$global_resources)
  assert_file_exists(
    there, check_case = TRUE, workdir = global_path,
    name = sprintf("Global resources in '%s'", global_path))
  src <- file.path(global_path, there)
  dst <- file.path(path_dest, here)

  is_dir <- is_directory(file.path(global_path, there))
  fs::dir_create(file.path(path_dest, dirname(here)))
  if (any(is_dir)) {
    fs::dir_copy(src[is_dir], dst[is_dir])
    ## Update the names that will be used in the metadata:
    files <- lapply(src[is_dir], dir)
    here <- replace_ragged(here, is_dir, Map(file.path, here[is_dir], files))
    there <- replace_ragged(there, is_dir, Map(file.path, there[is_dir], files))
  }
  if (any(!is_dir)) {
    fs::file_copy(src[!is_dir], dst[!is_dir])
  }

  data_frame(here = here, there = there)
}


static_orderly_global_resource <- function(args) {
  list(files = static_character_vector(args))
}


static_string <- function(x) {
  if (is.character(x)) {
    x
  } else if (is_call(x, "c") && length(x) == 2 && is.character(x[[2]])) {
    x[[2]]
  } else {
    NULL
  }
}


static_character_vector <- function(x) {
  if (is.character(x)) {
    x
  } else if (is_call(x, "c")) {
    x <- lapply(x[-1], static_character_vector)
    x <- if (all(vlapply(x, is.character))) unlist(x, FALSE, FALSE) else NULL
  } else {
    x <- NULL
  }
  x
}



static_eval <- function(fn, call) {
  if (is_call(call[[1]], "::")) {
    call[[1]] <- call[[1]][[3]]
  }
  args <- as.list(match.call(match.fun(call[[1]]), call)[-1])
  fn(args)
}


current <- new.env(parent = emptyenv())

get_active_packet <- function() {
  current[[getwd()]]
}
