orderly::orderly_resource("data.csv")
orderly::orderly_artefact("mygraph.png", description = "A graph of things")
orderly::orderly_artefact("data.csv", description = "Original data")

data <- read.csv("data.csv", stringsAsFactors = FALSE)
png("mygraph.png")
plot(data)
dev.off()
