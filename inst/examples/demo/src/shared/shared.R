# Pull in the file 'shared/palette.R' as 'cols.R'
orderly_shared_resource(cols.R = "palette.R")

# Then source it, as usual
source("cols.R")

# And use the function 'palette()' found within
png("volcano.png")
image(volcano, col = palette())
dev.off()
