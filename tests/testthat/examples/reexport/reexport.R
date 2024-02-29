orderly2::orderly_resource("data.csv")
orderly2::orderly_artefact("A graph of things", "mygraph.png")
orderly2::orderly_artefact("Original data", "data.csv")

data <- read.csv("data.csv", stringsAsFactors = FALSE)
png("mygraph.png")
plot(data)
dev.off()
