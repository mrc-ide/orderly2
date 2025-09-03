pars <- orderly::orderly_parameters(use = NULL)
if (pars$use == "a") {
  orderly::orderly_resource("a.csv")
} else {
  orderly::orderly_resource("b.csv")
}
orderly::orderly_artefact("data.rds", description = "final data")

d <- read.csv(sprintf("%s.csv", pars$use), stringsAsFactors = FALSE)
saveRDS(d, "data.rds")
