orderly::orderly_artefact(files = "data.rds", description = "Some data")
d <- data.frame(a = 1:10, x = runif(10), y = 1:10 + runif(10))
saveRDS(d, "data.rds")
