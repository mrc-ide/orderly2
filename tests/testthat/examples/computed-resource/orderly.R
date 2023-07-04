files <- dir(pattern = "*.csv")
orderly2::orderly_resource(files)
orderly2::orderly_artefact("A graph of things", "mygraph.png")

data <- read.csv("data.csv", stringsAsFactors = FALSE)
png("mygraph.png")
plot(data)
dev.off()
