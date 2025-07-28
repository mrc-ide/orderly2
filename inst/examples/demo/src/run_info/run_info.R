orderly_dependency("data", "latest", c("xy.rds" = "data.rds"))
xy <- readRDS("xy.rds")

info <- orderly_run_info()
print(info)

orderly_artefact("plot.png", description = "A plot of data")
png("plot.png")
plot(xy)
dev.off()
