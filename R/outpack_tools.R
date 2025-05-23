##' Extract metadata from a group of packets.  This is an
##' **experimental** high-level function for interacting with the
##' metadata in a way that we hope will be useful. We'll expand this a
##' bit as time goes on, based on feedback we get so let us know what
##' you think.  See Details for how to use this.
##'
##' Extracting data from outpack metadata is challenging to do in a
##' way that works in data structures familiar to R users, because it
##' is naturally tree structured, and because not all metadata may be
##' present in all packets (e.g., a packet that does not depend on
##' another will not have a dependency section, and one that was run
##' in a context without git will not have git metadata). If you just
##' want the raw tree-structured data, you can always use
##' [orderly2::orderly_metadata] to load the full metadata for any
##' packet (even one that is not currently available on your computer,
##' just known about it) and the structure of the data will remain
##' fairly constant across orderly2 versions.
##'
##' However, sometimes we want to extract data in order to ask
##' specific questions like:
##'
##' * what parameter combinations are available across a range of packets?
##' * when were a particular set of packets used?
##' * what files did these packets produce?
##'
##' Later we'd like to ask even more complex questions like:
##'
##' * at what version did the file `graph.png` change?
##' * what inputs changed between these versions?
##'
##' ...but being able to answer these questions requires a similar
##' approach to interrogating metadata across a range of packets.
##'
##' The `orderly_metadata_extract` function aims to simplify the
##' process of pulling out bits of metadata and arranging it into a
##' `data.frame` (of sorts) for you.  It has a little mini-language in
##' the `extract` argument for doing some simple rewriting of results,
##' but you can always do this yourself.
##'
##' In order to use function you need to know what metadata are
##' available; we will expand the vignette with more worked examples
##' here to make this easier to understand. The function works on
##' top-level keys, of which there are:
##'
##' * id: the packet id (this is always returned)
##' * name: the packet name
##' * parameters: a key-value pair of values, with string keys and
##'   atomic values. There is no guarantee about presence of keys
##'   between packets, or their types.
##' * time: a key-value pair of times, with string keys and time
##'   values (see [DateTimeClasses]; these are stored as seconds since
##'   1970 in the actual metadata). At present `start` and `end` are
##'   always present.
##' * files: files present in each packet. This is a `data.frame` (per
##'   packet), each with columns `path` (relative), `size` (in bytes)
##'   and `hash`.
##' * depends: dependencies used each packet. This is a `data.frame`
##'   (per packet), each with columns `packet` (id), `query` (string,
##'   used to find `packet`) and `files` (another `data.frame` with
##'   columns `there` and `here` corresponding to filenames upstream
##'   and in this packet, respectively)
##' * git: either metadata about the state of git or `null`. If given
##'   then `sha` and `branch` are strings, while `url` is an array of
##'   strings/character vector (can have zero, one or more elements).
##' * session: some information about the session that the packet was run in
##'   (this is unstandardised, and even the orderly version may change)
##' * custom: additional metadata added by its respective engine.  For
##'   packets run by `orderly2`, there will be a `orderly` field here,
##'   which is itself a list:
##'   * artefacts: A [data.frame] with artefact information, containing
##'     columns `description` (a string) and `paths` (a list column of paths).
##'   * shared: A [data.frame] of the copied shared resources with
##'     their original name (`there`) and name as copied into the packet
##'     (`here`).
##'   * role: A [data.frame] of identified roles of files, with columns `path`
##'     and `role`.
##'   * description: A list of information from
##'     [orderly2::orderly_description] with human-readable descriptions and
##'     tags.
##'   * session: A list of information about the session as run,
##'     with a list `platform` containing information about the platform
##'     (R version as `version`, operating system as `os` and system name
##'     as `system`) and `packages` containing columns `package` ,
##'     `version` and `attached`.
##'
##' The nesting here makes providing a universally useful data format
##' difficult; if considering files we have a `data.frame` with a
##' `files` column, which is a list of `data.frame`s; similar
##' nestedness applies to `depends` and the orderly custom
##' data. However, you should be able to fairly easily process the
##' data into the format you need it in.
##'
##' The simplest extraction uses names of top-level keys:
##'
##' ```
##' extract = c("name", "parameters", "files")
##' ```
##'
##' This creates a data.frame with columns corresponding to these
##' keys, one row per packet. Because `name` is always a string, it
##' will be a character vector, but because `parameters` and `files`
##' are more complex, these will be list columns.
##'
##' You must not provide `id`; it is always returned and always first
##' as a character vector column.  If your extraction could possibly
##' return data from locations (i.e., you have `allow_remote = TRUE`
##' or have given a value for `location`) then we add a logical column
##' `local` which indicates if the packet is local to your archive,
##' meaning that you have all the files from it locally.
##'
##' You can rename the columns by providing a name to entries within
##' `extract`, for example:
##'
##' ```
##' extract = c("name", pars = "parameters", "files")
##' ```
##'
##' is the same as above, except that that the `parameters` column has
##' been renamed `pars`.
##'
##' More interestingly, we can index into a structure like
##' `parameters`; suppose we want the value of the parameter `x`, we
##' could write:
##'
##' ```
##' extract = c(x = "parameters.x")
##' ```
##'
##' which is allowed because for *each packet* the `parameters`
##' element is a list.
##'
##' However, we do not know what type `x` is (and it might vary
##' between packets). We can add that information ourselves though and write:
##'
##' ```
##' extract = c(x = "parameters.x is number")
##' ```
##'
##' to create an numeric column. If any packet has a value of `x` that
##' is non-integer, your call to `orderly_metadata_extract` will fail
##' with an error, and if a packet lacks a value of `x`, a missing
##' value of the appropriate type will be added.
##'
##' Note that this does not do any coercion to number, it will error
##' if a non-NULL non-numeric value is found.  Valid types for use
##' with `is <type>` are `boolean`, `number` and `string` (note that
##' these differ slightly from R's names because we want to emphasise
##' that these are *scalar* quantities; also note that there is no
##' `integer` here as this may produce unexpected errors with
##' integer-like numeric values). You can also use `list` but this is
##' the default.  Things in the schema that are known to be scalar
##' atomics (such as `name`) will be automatically simplified.
##'
##' You can index into the array-valued elements (`files` and
##' `depends`) in the same way as for the object-valued elements:
##'
##' ```
##' extract = c(file_path = "files.path", file_hash = "files.hash")
##' ```
##'
##' would get you a list column of file names per packet and another
##' of hashes, but this is probably less useful than the `data.frame`
##' you'd get from extracting just `files` because you no longer have
##' the hash information aligned.
##'
##' You can index fairly deeply; it should be possible to get the
##' orderly "display name" with:
##'
##' ```
##' extract = c(display = "custom.orderly.description.display is string")
##' ```
##'
##' If the path you need to extract has a dot in it (most likely a
##' package name for a plugin, such as `custom.orderly.db`) you need
##' to escape the dot with a backslash (so, `custom.orderly\.db`). You
##' will probably need two slashes or use a raw string (in recent
##' versions of R).
##'
##' @section Custom 'orderly' metadata:
##'
##' Within `custom.orderly`, additional fields can be extracted. The
##'   format of this is subject to change, both in the stored metadata
##'   and schema (in the short term) and in the way we deserialise it.
##'   It is probably best not to rely on this right now, and we will
##'   expand this section when you can.
##'
##' @title Extract metadata from orderly2 packets
##'
##' @param extract A character vector of columns to extract, possibly
##'   named. See Details for the format.
##'
##' @inheritParams orderly_metadata
##' @inheritParams orderly_search
##' @inheritParams orderly_search_options
##'
##' @return A `data.frame`, the columns of which vary based on the
##'   names of `extract`; see Details for more information.
##'
##' @export
orderly_metadata_extract <- function(expr = NULL, name = NULL, location = NULL,
                                     allow_remote = NULL,
                                     fetch_metadata = FALSE,
                                     extract = NULL, options = NULL,
                                     root = NULL) {
  root <- root_open(root, require_orderly = FALSE)
  compatibility_fix_options(options, "orderly_metadata_extract")

  if (expr_is_literal_id(expr, name)) {
    ids <- expr
  } else {
    ids <- orderly_search(expr,
                          name = name,
                          location = location,
                          allow_remote = allow_remote,
                          fetch_metadata = fetch_metadata,
                          root = root)
  }
  extract <- parse_extract(extract, environment())

  is_core_metadata <-
    vlapply(extract$from, function(el) el[[1]] %in% metadata_core_names)
  if (all(is_core_metadata)) {
    meta <- lapply(ids, outpack_metadata_core, root = root)
  } else {
    meta <- lapply(ids, orderly_metadata, root = root)
  }

  envir <- environment()
  ret <- data_frame(id = ids)
  if (isTRUE(allow_remote) || length(location) > 0) {
    loc <- root$index$location(location)
    loc <- loc[loc$packet %in% ids, ]
    ret$local <- ids %in% root$index$unpacked()
    ret$location <- I(unname(split(loc$location, loc$packet)[ids]))
  }
  for (i in seq_len(nrow(extract))) {
    from_i <- extract$from[[i]]
    is_i <- extract$is[[i]]
    value_i <- lapply(meta, function(x) {
      tryCatch(x[[from_i]], error = function(e) NULL)
    })
    ret[[extract$to[[i]]]] <- extract_convert(ids, value_i, from_i, is_i, envir)
  }

  ret
}


parse_extract <- function(extract, call = NULL) {
  if (is.null(extract)) {
    extract <- c("name", "parameters")
  }

  re_dot <- "(?<!\\\\)\\."
  re_type <- "^(.+)\\s+is+\\s+(.+)$"

  extract_as_nms <- sub(re_type, "\\1", gsub(re_dot, "_", extract, perl = TRUE))
  extract_as_nms <- sub("\\.", ".", extract_as_nms, fixed = TRUE)
  if (is.null(names(extract))) {
    to <- extract_as_nms
  } else {
    to <- names(extract)
    i <- !nzchar(to)
    to[i] <- extract_as_nms[i]
  }

  is <- rep(NA_character_, length(extract))

  i <- grepl(re_type, extract)
  if (any(i)) {
    valid <- c("string", "number", "boolean", "list")
    is[i] <- unname(sub(re_type, "\\2", extract[i]))
    extract[i] <- sub(re_type, "\\1", extract[i])
    if (any(err <- !(is.na(is) | is %in% valid))) {
      collapseq <- function(x) paste(squote(x), collapse = ", ")
      invalid <- sprintf("Extraction of '%s' used type '%s'",
                         extract[err], is[err])
      cli::cli_abort(
        c(sprintf("Invalid conversion type %s requested in 'extract'",
                  collapseq(unique(is[err]))),
          i = sprintf("'is' must be one of %s", collapseq(valid)),
          set_names(invalid, rep("x", length(invalid)))),
        call = call)
    }
  }

  extract <- strsplit(extract, re_dot, perl = TRUE)
  extract <- lapply(extract, function(x) gsub("\\.", ".", x, fixed = TRUE))

  if ("id" %in% to) {
    cli::cli_abort(
      "Don't use 'id' as a column to extract; this column is always added",
      call = call)
  }

  if (any(duplicated(to))) {
    dups <- paste(squote(unique(to[duplicated(to)])), collapse = ", ")
    cli::cli_abort(
      c("All destination columns in 'extract' must be unique",
        x = sprintf("Duplicated names: %s", dups)),
      call = call)
  }

  data_frame(
    from = I(as.list(unname(extract))),
    to = to,
    is = is)
}


extract_type <- function(nm, is) {
  if (length(nm) == 1 && nm %in% c("id", "name")) {
    "string"
  } else if (length(nm) == 2 && nm[[1]] == "time") {
    "time"
  } else if (length(nm) == 2 && nm[[1]] == "git" && nm[[2]] != "url") {
    "string"
  } else if (!is.na(is)) {
    is
  } else {
    "list"
  }
}


extract_convert <- function(ids, value, from, is, call) {
  examples <- NULL
  type <- extract_type(from, is)
  if (type == "list") {
    return(I(value))
  } else {
    is_null <- vlapply(value, is.null)
    len <- lengths(value)
    mode <- vcapply(value, storage.mode)
    ## This can't be triggered until we add packages or sources back
    ## into the orderly custom schema
    err <- len != 1 & !is_null
    if (any(err)) {
      msg <- sprintf(
        "Expected all values of '%s' to evaluate to a scalar (if not NULL)",
        paste(from, collapse = "."))
      examples <- sprintf("Value for %s has length %d", ids[err], len[err])
      cli::cli_abort(c(msg, error_examples(examples)), call = call)
    }

    ## convert all integers into reals, just to forget about the
    ## differences between the two of them here as the user can deal
    ## with that in the very rare cases where it matters. jsonlite
    ## does bring `1` as an integer vector, which is rarely what we
    ## want, and it deparses poorly.
    if (type == "number" && any(mode == "integer")) {
      value[mode == "integer"] <- lapply(value[mode == "integer"], as.numeric)
      mode[mode == "integer"] <- "double"
    }

    ## We can use the typed missing value here to test against
    ## storage.mode, to fill in missing values and also to pass
    ## through to vapply to get a vector of the expected type out:
    missing <- switch(type,
                      string = NA_character_,
                      number = NA_real_,
                      boolean = NA,
                      time = NA_real_)
    err <- !is_null & mode != storage.mode(missing)
    if (any(err)) {
      msg <- sprintf(
        "Expected all values of '%s' to be %ss (or NULL)",
        paste(from, collapse = "."), type)
      examples <- sprintf("Found `%s` (a %s) for packet '%s'",
                          vcapply(value[err], deparse1, control = "keepNA"),
                          vcapply(value[err], storage_mode_scalar),
                          ids[err])
      cli::cli_abort(c(msg, error_examples(examples)), call = call)
    }
    value[is_null] <- missing
    value <- vapply(value, identity, missing)
    if (type == "time") {
      value <- num_to_time(value)
    }
    value
  }
}


## Reverses the conversion in extract_convert()
storage_mode_scalar <- function(x) {
  if (inherits(x, "POSIXt")) {
    "time"
  } else {
    mode <- storage.mode(x)
    switch(mode,
           character = "string",
           double = "number",
           integer = "number",
           logical = "boolean",
           mode)
  }
}


error_examples <- function(str, max = 3) {
  if (length(str) > max) {
    str <- c(str[seq_len(max)], sprintf("(...and %d more)", length(str) - max))
  }
  set_names(str, rep("i", length(str)))
}
