orderly2::orderly_shared_resource(shared_data = "data")
orderly2::orderly_artefact("combined data", "output.rds")

files <- dir("shared_data")
dat <- lapply(file.path("shared_data", files), read.csv)
names(dat) <- sub("\\..+$", "", files)
saveRDS(dat, "output.rds")
