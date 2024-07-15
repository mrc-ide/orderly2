orderly2::orderly_strict_mode()
orderly2::orderly_resource("data")
orderly2::orderly_artefact(files = "output", description = "output files")

a <- read.csv("data/a.csv")
b <- read.csv("data/b.csv")

dir.create("output", FALSE, TRUE)
saveRDS(a, "output/a.rds")
saveRDS(b, "output/b.rds")
