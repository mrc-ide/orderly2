orderly3::orderly_depends("depends", "latest",
                          c(depends_graph.png = "graph.png"),
                          label = "depends")
orderly3::orderly_depends("explicit", "usedby({depends})",
                          c(explicit_graph.png = "mygraph.png"))
orderly3::orderly_artefact("All plots zip", "all_plots.zip")

z <- tempfile()
dir.create(z, FALSE, TRUE)
file.copy("depends_graph.png", file.path(z, "depends_graph.png"))
file.copy("explicit_graph.png", file.path(z, "explicit_graph.png"))
zip::zip("all_plots.zip",
         list.files(z, full.names = TRUE),
         mode = "cherry-pick")

