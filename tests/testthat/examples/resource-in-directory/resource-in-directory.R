orderly::orderly_artefact(description = "data", files = "data.rds")
a <- read.csv("data/a.csv")
b <- read.csv("data/b.csv")
saveRDS(list(a = a, b = b), "data.rds")
