## This is something that we might improve over time - it will likely
## be useful to have some sort of "register interactive" function
## which we could then just look up.
##
## I am not sure if we also want to allow working interactively from a
## draft directory too.
detect_orderly_interactive_path <- function(path = getwd()) {
  path_split <- fs::path_split(path)[[1]]
  is_plausible <- length(path_split) > 2 &&
    path_split[[length(path_split) - 1]] == "src" &&
    file.exists(file.path(path, "../..", "orderly_config.yml"))
  if (!is_plausible) {
    stop(sprintf("Failed to detect orderly path at '%s'", path))
  }
  orderly_root(file.path(path, "../.."), FALSE)
}
