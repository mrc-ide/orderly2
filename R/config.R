orderly_config <- function(path) {
  orderly_config_yml_read(path)
}


orderly_config_yml_read <- function(path) {
  filename <- file.path(path, "orderly_config.yml")
  assert_file_exists(basename(filename), workdir = path,
                     name = "Orderly configuration")
  raw <- yaml_read(filename)

  if (!is.null(raw)) {
    assert_named(raw)
  }

  raw <- resolve_env(raw, orderly_envir_read(path), "orderly_config.yml")

  check <- list(
    minimum_orderly_version = orderly_config_validate_minimum_orderly_version,
    plugins = orderly_config_validate_plugins,
    global_resources = orderly_config_validate_global_resources)

  required <- "minimum_orderly_version"
  optional <- setdiff(names(check), required)
  check_fields(raw, filename, required, optional)

  ## This simplifies the validation
  owd <- setwd(path)
  on.exit(setwd(owd))
  dat <- list()
  for (x in names(check)) {
    dat[[x]] <- check[[x]](raw[[x]], filename)
  }

  dat
}


orderly_config_validate_minimum_orderly_version <- function(value, filename) {
  assert_scalar_character(value)
  version <- numeric_version(value)
  if (version < numeric_version("1.99.0")) {
    stop("Migrate from version 1, see docs that we need to write still...")
  }
  if (version > current_orderly_version()) {
    stop(sprintf(
      "orderly version '%s' is required, but only '%s' installed",
      version, current_orderly_version()))
  }
  version
}


orderly_config_validate_global_resources <- function(global_resources,
                                                     filename) {
  if (!is.null(global_resources)) {
    assert_is_directory(global_resources, name = "Global resource directory")
    global_resources
  }
}


orderly_config_validate_plugins <- function(plugins, filename) {
  if (is.null(plugins)) {
    return(NULL)
  }
  assert_named(plugins, unique = TRUE, name = sprintf("%s:plugins", filename))

  ret <- list()
  for (nm in names(plugins)) {
    dat <- load_orderly_plugin(nm)
    dat$config <- dat$config(plugins[[nm]], filename)
    ret[[nm]] <- dat
  }
  ret
}


orderly_envir_read <- function(path) {
  filename <- file.path(path, "orderly_envir.yml")
  if (!file.exists(filename)) {
    return(NULL)
  }

  dat <- yaml_read(filename)
  if (length(dat) == 0) {
    return(NULL)
  }

  assert_named(dat, TRUE, basename(filename))
  n <- lengths(dat)
  nok <- n != 1L
  if (any(nok)) {
    stop(sprintf("Expected all elements of %s to be scalar (check %s)",
                 basename(filename),
                 paste(squote(names(dat)[nok]), collapse = ", ")))
  }
  vcapply(dat[n == 1], as.character)
}
