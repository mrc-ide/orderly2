orderly2::orderly_resource("data.csv")
orderly2::orderly_artefact("mygraph.png", description = "A graph of things")
orderly2::orderly_artefact("data.csv", description = "Original data")

data <- read.csv("data.csv", stringsAsFactors = FALSE)
png("mygraph.png")
plot(data)
dev.off()
