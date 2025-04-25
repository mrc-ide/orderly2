pars <- orderly2::orderly_parameters(a = NULL, b = 2, c = NULL,
                                     .export = FALSE)
saveRDS(list(a = pars$a, b = pars$b, c = pars$c), "data.rds")
