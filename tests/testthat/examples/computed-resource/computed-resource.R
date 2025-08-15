files <- dir(pattern = "*.csv")
orderly::orderly_resource(files)
orderly::orderly_artefact("mygraph.png", description = "A graph of things")

data <- read.csv("data.csv", stringsAsFactors = FALSE)
png("mygraph.png")
plot(data)
dev.off()
