orderly::orderly_shared_resource(shared_data = "data")
orderly::orderly_artefact("output.rds", description = "combined data")

files <- dir("shared_data")
dat <- lapply(file.path("shared_data", files), read.csv)
names(dat) <- sub("\\..+$", "", files)
saveRDS(dat, "output.rds")
