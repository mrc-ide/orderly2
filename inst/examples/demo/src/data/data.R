orderly_description(
  display = "A demo data set")

x <- jitter(1:30)
y <- 0.4 * x + 5 + rnorm(length(x), sd = 2)
d <- data.frame(x, y)

orderly_artefact("data.rds", description = "A synthetic dataset")
saveRDS(d, "data.rds")
