orderly2::orderly_resource("data.csv")
orderly2::orderly_artefact("mygraph.png", description = "A graph of things")

data <- read.csv("data.csv", stringsAsFactors = FALSE)
png("mygraph.png")
plot(data)
dev.off()
