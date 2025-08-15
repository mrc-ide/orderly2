orderly::orderly_artefact("data.rds", description = "Generated data")
example.random::numbers("dat", 10)
saveRDS(dat, "data.rds")
