orderly_context <- function() {
  assert_scalar_character(name)
  p <- get_active_packet()
  is_active <- !is.null(p)
  if (is_active) {
    path <- p$path
    config <- p$orderly3$config
    env <- p$orderly3$envir
    src <- p$orderly3$src
    parameters <- p$parameters
  } else {
    path <- getwd()
    root <- detect_orderly_interactive_path(path)
    config <- orderly_root(root$path, FALSE)$config
    env <- orderly_plugin_environment(name)
    src <- path
    parameters <- current_orderly_parameters(src, env)
  }
  list(is_active = is_active, path = path, config = config, env = env,
       src = src, parameters = parameters)
}
