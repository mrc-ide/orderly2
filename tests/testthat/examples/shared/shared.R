orderly::orderly_shared_resource(shared_data.csv = "data.csv")
orderly::orderly_artefact("mygraph.png", description = "A graph of things")

data <- read.csv("shared_data.csv", stringsAsFactors = FALSE)
png("mygraph.png")
plot(data)
dev.off()
