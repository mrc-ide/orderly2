## Like sys.source but a different set of options - this produces a
## more aesthetically pleasing log than source() with no options.
source_echo <- function(path, envir, echo) {
  source(path, local = envir, echo = echo, # nolint
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
