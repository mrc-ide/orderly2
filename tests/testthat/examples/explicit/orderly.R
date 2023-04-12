orderly3::orderly_resource("data.csv")
orderly3::orderly_artefact("A graph of things", "mygraph.png")

data <- read.csv("data.csv", stringsAsFactors = FALSE)
data <- rbind(data, data.frame(x = 11:15, y = runif(5)))
png("mygraph.png")
plot(data)
dev.off()
