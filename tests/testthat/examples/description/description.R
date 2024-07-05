orderly2::orderly_resource("data.csv")
orderly2::orderly_artefact("mygraph.png", description = "A graph of things")
orderly2::orderly_description(
  "Packet with description",
  "A longer description. Perhaps multiple sentences",
  list(author = "Alice", requester = "Bob"))

data <- read.csv("data.csv", stringsAsFactors = FALSE)
png("mygraph.png")
plot(data)
dev.off()
