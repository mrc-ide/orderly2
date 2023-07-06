## There's a lot of detail in here, and this is something that it's
## surprisingly painful to do well. Some of the interface is designed
## so that it will be easy (ish) later on to slot in a version that
## runs in a separate process - essentially as an argument to
## evaluate_script() - but we'll look at that later as it comes with
## its own complications.

## Evaluate some script, within some directory, in some environment,
## and possibly echo the result to screen. Only report success in the
## case where in addition to the script succeeding the global resource
## stacks (connections and graphics devices) are balanced.
##
## Even on script failure, we do try and balance the global resource
## stacks so that the user does not need to be careful about this.
evaluate_script <- function(path, script, envir, echo) {
  info <- session_global_state()
  ## It's important to do the global state check in the packet working
  ## directory (not the calling working directory) otherwise we might
  ## write out files in unexpected places when flushing devices.
  withr::with_dir(path, {
    result <- source_and_capture(script, envir, echo)
    info_end <- check_session_global_state(info)
  })

  if (!result$success) {
    result$message <- sprintf("Script failed with error: %s",
                              result$error$message)
  } else if (!info_end$success) {
    result$success <- FALSE
    result$message <- info_end$message
  }

  result
}


## A slightly tweaked version of sys.source that makes it easier to
## collect the output of a running script, along with things like
## warnings. Does not throw, but returns a "Result" like
## interface. Also includes a stack trace on error (just a text
## version, we might move to rlang's later).
source_and_capture <- function(path, envir, echo) {
  tmp <- tempfile()
  con <- file(tmp, "w")
  on.exit({
    close(con)
    unlink(tmp)
  })

  env <- new.env(parent = emptyenv())
  env$warnings <- collector()

  res <- tryCatch(
    withr::with_output_sink(
      new = con, split = echo,
      withr::with_message_sink(
        stdout(),
        withCallingHandlers(
          source_echo(path, envir),
          warning = function(e) {
            env$warnings$add(e)
            tryInvokeRestart("muffleWarning")
          },
          error = function(e) {
            try(stop(e))
            env$error <- e
            env$traceback <- utils::limitedLabels(sys.calls())
          }))),
    error = identity)

  list(success = is.null(env$error),
       error = env$error,
       traceback = env$traceback,
       warnings = env$warnings$get(),
       output = readLines(tmp))
}


## Like sys.source but a different set of options - this produces a
## more aesthetically pleasing log than source() with no options.
source_echo <- function(path, envir) {
  source(path, local = envir, echo = TRUE, # nolint
         max.deparse.length = Inf, keep.source = TRUE, spaced = FALSE)
}


session_global_state <- function() {
  list(n_open_devices = length(grDevices::dev.list()),
       n_open_sinks = sink.number())
}


check_session_global_state <- function(info) {
  devices <- check_device_stack(info$n_open_devices)
  sinks <- check_sink_stack(info$n_open_sinks)

  success <- devices$success && sinks$success
  if (success) {
    msg <- NULL
  } else {
    msg <- paste(c(devices$message, sinks$message), collapse = " & ")
  }
  list(success = success, message = msg)
}


check_device_stack <- function(expected) {
  check <- length(grDevices::dev.list()) - expected
  success <- check == 0
  if (check == 0) {
    msg <- NULL
  } else if (check > 0) {
    for (i in seq_len(check)) {
      grDevices::dev.off()
    }
    msg <- ngettext(check,
                    "Script left 1 device open",
                    sprintf("Script left %d devices open", check))
  } else {
    msg <- sprintf("Script closed %d more devices than it opened!", abs(check))
  }
  list(success = success, message = msg)
}


check_sink_stack <- function(expected) {
  check <- sink.number() - expected
  success <- check == 0
  if (check == 0) {
    msg <- NULL
  } else if (check > 0) {
    for (i in seq_len(check)) {
      sink(NULL) # nolint
    }
    msg <- ngettext(check,
                    "Script left 1 sink open",
                    sprintf("Script left %d sinks open", check))
  } else {
    msg <- sprintf("Script closed %d more sinks than it opened!", abs(check))
  }
  list(success = success, message = msg)
}
