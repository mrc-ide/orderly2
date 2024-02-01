orderly2::orderly_parameters(use = NULL)
if (use == "a") {
  orderly2::orderly_resource("a.csv")
} else {
  orderly2::orderly_resource("b.csv")
}
orderly2::orderly_artefact("final data", "data.rds")

d <- read.csv(sprintf("%s.csv", use), stringsAsFactors = FALSE)
saveRDS(d, "data.rds")
