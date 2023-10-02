orderly_config_read <- function(path, call = NULL) {
  filename <- file.path(path, "orderly_config.yml")
  assert_file_exists_relative(basename(filename), workdir = path,
                              name = "Orderly configuration", call = call)
  raw <- yaml_read(filename)

  if (!is.null(raw)) {
    assert_named(raw, call = call)
  }

  raw <- resolve_envvar(raw, orderly_envir_read(path, call),
                        "orderly_config.yml")

  check <- list(
    minimum_orderly_version = orderly_config_validate_minimum_orderly_version,
    plugins = orderly_config_validate_plugins)

  required <- "minimum_orderly_version"
  optional <- setdiff(names(check), required)
  check_fields(raw, filename, required, optional)

  ## This simplifies the validation
  owd <- setwd(path)
  on.exit(setwd(owd))
  dat <- list()
  for (x in names(check)) {
    dat[[x]] <- check[[x]](raw[[x]], filename, call)
  }

  dat
}


orderly_config_validate_minimum_orderly_version <- function(value, filename,
                                                            call = NULL) {
  assert_scalar_character(value, call = call)
  version <- numeric_version(value)
  if (version < numeric_version("1.99.0")) {
    cli::cli_abort(
      c("Detected old orderly version, you need to migrate to orderly2",
        i = 'Please see documentation at vignette("migrating")'),
      call = call)
  }
  if (version > current_orderly_version()) {
    cli::cli_abort(sprintf(
      "orderly version '%s' is required, but only '%s' installed",
      version, current_orderly_version()),
      call = call)
  }
  version
}


orderly_config_validate_plugins <- function(plugins, filename, call = NULL) {
  if (is.null(plugins)) {
    return(NULL)
  }
  assert_named(plugins, unique = TRUE, name = sprintf("%s:plugins", filename),
               call = call)

  ret <- list()
  for (nm in names(plugins)) {
    dat <- load_orderly_plugin(nm)
    dat$config <- dat$config(plugins[[nm]], filename)
    ret[[nm]] <- dat
  }
  ret
}


orderly_envir_read <- function(path, call = NULL) {
  filename <- file.path(path, "orderly_envir.yml")
  if (!file.exists(filename)) {
    return(NULL)
  }

  dat <- yaml_read(filename)
  if (length(dat) == 0) {
    return(NULL)
  }

  assert_named(dat, TRUE, basename(filename), call = call)
  n <- lengths(dat)
  nok <- n != 1L
  if (any(nok)) {
    err <- sprintf("Expected '%s' to be scalar, but had length %d",
                   names(dat)[nok], n[nok])
    cli::cli_abort(
      c("All elements of '{basename(filename)}' must be scalar",
        set_names(err, rep("x", length(err))),
        i = "Working directory was '{dirname(filename)}'"),
      call = call)
  }
  vcapply(dat[n == 1], as.character)
}
