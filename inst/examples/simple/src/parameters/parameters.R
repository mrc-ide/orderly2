pars <- orderly_parameters(max_cyl = NULL)
orderly_artefact("data.rds", description = "Final data")
data <- mtcars[mtcars$cyl <= pars$max_cyl, ]
saveRDS(data, "data.rds")
