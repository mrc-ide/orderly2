orderly3::orderly_parameters(use = NULL)
if (use == "a") {
  orderly3::orderly_resource("a.csv")
} else {
  orderly3::orderly_resource("b.csv")
}
orderly3::orderly_artefact("final data", "data.rds")

d <- read.csv(sprintf("%s.csv", use), stringsAsFactors = FALSE)
saveRDS(d, "data.rds")
