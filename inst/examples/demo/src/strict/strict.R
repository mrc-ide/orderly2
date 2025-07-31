## An example showing how strict mode works

## Enable strict mode here:
orderly_strict_mode()

## We declare that 'data.csv' is a resource; it will be copied over
## into the working directory when this packet is run.
orderly_resource("data.csv")

## If the line above is omitted then the run will fail here where we
## try to read the data:
d <- read.csv("data.csv")

orderly_artefact("plot.png", description = "A plot of data")
png("plot.png")
plot(d)
dev.off()
