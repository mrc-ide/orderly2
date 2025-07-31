# This declares that this orderly report accepts one parameter
# 'max_cyl' with no default (i.e., it is required).
pars <- orderly_parameters(max_cyl = NULL)
orderly_artefact("data.rds", description = "Final data")

# We can use the parameter by subsetting 'pars'; unlike regular R
# lists you will get an error if you try and access a non-existent
# element.
data <- mtcars[mtcars$cyl <= pars$max_cyl, ]
saveRDS(data, "data.rds")
