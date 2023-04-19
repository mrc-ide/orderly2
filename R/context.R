orderly_context <- function() {
  p <- get_active_packet()
  is_active <- !is.null(p)
  if (is_active) {
    path <- p$path
    root <- p$root$path
    config <- p$orderly3$config
    env <- p$orderly3$envir
    src <- p$orderly3$src
    parameters <- p$parameters
  } else {
    path <- getwd()
    root <- detect_orderly_interactive_path(path)$path
    config <- orderly_root(root, FALSE)$config
    env <- orderly_environment("orderly3")
    src <- path
    parameters <- current_orderly_parameters(src, env)
  }
  list(is_active = is_active, path = path, config = config, env = env,
       root = root, src = src, parameters = parameters,
       packet = p)
}


## This is a real trick, and is only used in the case where the report
## is being run interactively, and in that case the correct thing is
## *almost certainly* the global environment, as the user has to do
## some tricks to stop that being the case; for example running
##
##   source("orderly.R", local = TRUE)
##
## We want to find the environment that corresponds to the top level
## environment for orderly; that will be the one that called the
## plugin function. So we'll have a stack of frames (corresponding to
## the environments on the call stack) with *parents* that look like
## this, outermost first:
##
## - (anything else)
## - (calling environment) <-- this is what we're looking for
## - (plugin)
## - (orderly3; orderly_plugin_context)
## - (orderly3; from orderly_context)
## - (orderly3; from orderly_environment)
##
## so we loop down the stack looking for the first call to a function
## in the plugin package, then take the frame *above* that.
##
## When we want this for a non-plugin case (i.e., the caller is in
## orderly3) then we just need to pass name = "orderly3" here
orderly_environment <- function(name) {
  frames <- sys.frames()
  for (i in seq_along(frames)[-1]) {
    if (environmentName(parent.env(frames[[i]])) == name) {
      return(frames[[i - 1]])
    }
  }
  ## This error should never surface if the plugin is configured correctly
  stop("Could not determine calling environment safely - please report")
}
