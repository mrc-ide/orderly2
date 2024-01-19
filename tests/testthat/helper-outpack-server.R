outpack_server <- function(path, timeout = 10) {
  outpack_server <- Sys.which("outpack")
  if (!nzchar(outpack_server)) {
    testthat::skip("outpack_server not installed")
  }
  args <- c("start-server", "--root", path)
  px <- processx::process$new(outpack_server, args)
  withr::defer_parent(px$kill())

  t_end <- Sys.time() + timeout
  success <- FALSE
  while (!success && px$is_alive() && Sys.time() < t_end) {
    result <- tryCatch(httr::GET("http://localhost:8000"), error = identity)
    success <- !inherits(result, "error")
  }
  if (!success) {
    stop("Failed to bring up server!")
  }
  px
}
