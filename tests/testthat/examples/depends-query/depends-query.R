pars <- orderly::orderly_parameters(a = NULL, b = NULL, c = NULL)

value <- pars$c
orderly::orderly_dependency(
  "parameters",
  paste("latest(parameter:a == this:a &&",
        "parameter:b == this:b &&",
        "parameter:c == environment:value)"),
  c(input.rds = "data.rds"))
orderly::orderly_artefact("result.rds", description = "Processed data")
d <- readRDS("input.rds")
saveRDS(lapply(d, function(x) x * 2), "result.rds")
