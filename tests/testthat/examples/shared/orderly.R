orderly2::orderly_shared_resource(shared_data.csv = "data.csv")
orderly2::orderly_artefact("A graph of things", "mygraph.png")

data <- read.csv("shared_data.csv", stringsAsFactors = FALSE)
png("mygraph.png")
plot(data)
dev.off()
