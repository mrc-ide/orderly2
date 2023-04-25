orderly3::orderly_global_resource(global_data.csv = "data.csv")
orderly3::orderly_artefact("A graph of things", "mygraph.png")

data <- read.csv("global_data.csv", stringsAsFactors = FALSE)
png("mygraph.png")
plot(data)
dev.off()
