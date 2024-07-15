orderly2::orderly_parameters(a = NULL, b = NULL, c = NULL)
orderly2::orderly_dependency(
  "parameters",
  paste("latest(parameter:a == this:a &&",
        "parameter:b == this:b &&",
        "parameter:c == this:c)"),
  c(input.rds = "data.rds"))
orderly2::orderly_artefact("result.rds", description = "Processed data")
d <- readRDS("input.rds")
saveRDS(lapply(d, function(x) x * 2), "result.rds")
