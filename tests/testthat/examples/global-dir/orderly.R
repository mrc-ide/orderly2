orderly2::orderly_global_resource(global_data = "data")
orderly2::orderly_artefact("combined data", "output.rds")

files <- dir("global_data")
dat <- lapply(file.path("global_data", files), read.csv)
names(dat) <- sub("\\..+$", "", files)
saveRDS(dat, "output.rds")
