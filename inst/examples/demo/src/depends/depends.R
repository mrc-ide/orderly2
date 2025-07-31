# Depend on 'data.rds' from 'data' as 'xy.rds', using the most recent
# packet:
orderly_dependency("data", "latest", c("xy.rds" = "data.rds"))

# When the report runs, or when you run through interactively, the
# file 'xy.rds' will be present and you can ready it as normal
xy <- readRDS("xy.rds")

# The rest of the analysis would then proceed as usual
orderly_artefact("plot.png", description = "A plot of data")
png("plot.png")
plot(xy)
dev.off()
