test_that("No locations except local by default", {
  root <- create_temporary_root()
  expect_equal(orderly_location_list(root = root), "local")
})


test_that("Can add a location", {
  root <- list()
  for (name in c("a", "b", "c")) {
    root[[name]] <- create_temporary_root()
  }

  orderly_location_add("b", "path", list(path = root$b$path), root = root$a)
  expect_setequal(orderly_location_list(root = root$a), c("local", "b"))

  orderly_location_add("c", "path", list(path = root$c$path), root = root$a)
  expect_setequal(orderly_location_list(root = root$a), c("local", "b", "c"))
})


test_that("Can't add a location with reserved name", {
  root <- create_temporary_root()
  upstream <- create_temporary_root()

  expect_error(
    orderly_location_add("local", "path", list(path = upstream$path),
                         root = root),
    "Cannot add a location with reserved name 'local'")
})


test_that("Can't add a location with existing name", {
  root <- list()
  for (name in c("a", "b", "c")) {
    root[[name]] <- create_temporary_root()
  }

  orderly_location_add("upstream", "path", list(path = root$b$path),
                       root = root$a)
  expect_error(
    orderly_location_add("upstream", "path", list(path = root$c$path),
                         root = root$a),
    "A location with name 'upstream' already exists")
  expect_equal(orderly_location_list(root = root$a),
               c("local", "upstream"))
})


test_that("Require that (for now) locations must be paths", {
  root <- create_temporary_root()
  expect_equal(orderly_location_list(root = root), "local")

  other <- temp_file()
  expect_error(
    orderly_location_add("other", "path", list(path = other), root = root),
    "Directory does not exist:")
  fs::dir_create(other)
  expect_error(
    orderly_location_add("other", "path", list(path = other), root = root),
    "Did not find existing orderly (or outpack) root in",
    fixed = TRUE)
})


test_that("Can rename a location", {
  root <- list()
  for (name in c("a", "b")) {
    root[[name]] <- create_temporary_root()
  }

  orderly_location_add("b", "path", list(path = root$b$path), root = root$a)
  expect_setequal(orderly_location_list(root = root$a), c("local", "b"))

  orderly_location_rename("b", "c", root = root$a)
  expect_setequal(orderly_location_list(root = root$a), c("local", "c"))
  expect_setequal(orderly_config(root$a)$location$name, c("local", "c"))
})


test_that("Can't rename a location using an existent name", {
  root <- list()
  for (name in c("a", "b", "c")) {
    root[[name]] <- create_temporary_root()
  }

  orderly_location_add("b", "path", list(path = root$b$path), root = root$a)
  orderly_location_add("c", "path", list(path = root$c$path), root = root$a)

  expect_error(orderly_location_rename("b", "c", root$a),
               "A location with name 'c' already exists")
  expect_error(orderly_location_rename("b", "local", root$a),
               "A location with name 'local' already exists")
})


test_that("Can't rename a  non-existent location", {
  root <- create_temporary_root()
  expect_equal(orderly_location_list(root = root), "local")

  expect_error(orderly_location_rename("a", "b", root),
               "No location with name 'a' exists")
})


test_that("Can't rename default locations", {
  root <- create_temporary_root()

  expect_error(orderly_location_rename("local", "desktop", root),
               "Cannot rename default location 'local'")
  expect_error(orderly_location_rename("orphan", "removed", root),
               "Cannot rename default location 'orphan'")
})


test_that("Can remove a location", {
  root <- list()
  for (name in c("a", "b", "c")) {
    root[[name]] <- create_temporary_root()
  }

  orderly_location_add("b", "path", list(path = root$b$path), root = root$a)
  orderly_location_add("c", "path", list(path = root$c$path), root = root$a)
  expect_setequal(orderly_location_list(root = root$a), c("local", "b", "c"))

  id <- create_random_packet(root$b)
  orderly_location_pull_metadata(root = root$a)

  # remove a location without packets
  orderly_location_remove("c", root = root$a)
  expect_setequal(orderly_location_list(root = root$a),
                  c("local", "b"))

  # remove a location with packets
  orderly_location_remove("b", root = root$a)
  expect_setequal(orderly_location_list(root = root$a),
                  c("local", "orphan"))

  config <- orderly_config(root$a)
  orphan_id <- "orphan"
  expect_equal(root$a$index$data()$location$location, orphan_id)
})


test_that("Removing a location orphans packets only from that location", {
  root <- list()
  for (name in c("a", "b", "c")) {
    root[[name]] <- create_temporary_root()
  }

  orderly_location_add("c", "path", list(path = root$c$path), root = root$b)
  orderly_location_add("b", "path", list(path = root$b$path), root = root$a)
  orderly_location_add("c", "path", list(path = root$c$path), root = root$a)
  expect_setequal(orderly_location_list(root = root$a), c("local", "b", "c"))
  expect_setequal(orderly_location_list(root = root$b), c("local", "c"))

  id1 <- create_random_packet(root$c)
  id2 <- create_random_packet(root$b)
  orderly_location_pull_metadata(root = root$b)
  orderly_location_pull_packet(id1, root = root$b)
  orderly_location_pull_metadata(root = root$a)

  # id1 should now be found in both b and c
  index <- root$a$index$data()
  expect_equal(index$location$location[index$location$packet == id1],
               c("b", "c"))

  # id2 should just be found in b
  expect_equal(index$location$location[index$location$packet == id2], "b")

  # remove location b
  orderly_location_remove("b", root = root$a)
  expect_setequal(orderly_location_list(root = root$a),
                  c("local", "orphan", "c"))

  # id1 should now only be found in c
  index <- root$a$index$data()
  expect_equal(index$location$location[index$location$packet == id1], "c")

  # id2 should be orphaned
  expect_equal(index$location$location[index$location$packet == id2], "orphan")
})


test_that("Can't remove default locations", {
  root <- create_temporary_root()

  expect_error(orderly_location_remove("local",  root),
               "Cannot remove default location 'local'")
  expect_error(orderly_location_remove("orphan", root),
               "Cannot remove default location 'orphan'")
})


test_that("Can't remove non-existent location", {
  root <- create_temporary_root()

  expect_error(orderly_location_remove("b",  root),
               "No location with name 'b' exists")
})


test_that("can pull metadata from a file base location", {
  root_upstream <- create_temporary_root(use_file_store = TRUE)

  ids <- vcapply(1:3, function(i) create_random_packet(root_upstream$path))
  root_downstream <- create_temporary_root(use_file_store = TRUE)

  orderly_location_add("upstream", "path", list(path = root_upstream$path),
                       root = root_downstream)
  expect_equal(orderly_location_list(root = root_downstream),
               c("local", "upstream"))

  orderly_location_pull_metadata("upstream", root = root_downstream)

  ## Sensible tests here will be much easier to write once we have a
  ## decent query interface.
  index <- root_downstream$index$data()
  expect_length(index$metadata, 3)
  expect_setequal(names(index$metadata), ids)
  expect_mapequal(index$metadata, root_upstream$index$data()$metadata)

  expect_s3_class(index$location, "data.frame")
  expect_setequal(index$location$packet, ids)
  expect_equal(index$location$location, rep("upstream", 3))
})


test_that("can pull empty metadata", {
  root_upstream <- create_temporary_root(use_file_store = TRUE)
  root_downstream <- create_temporary_root(use_file_store = TRUE)

  orderly_location_add("upstream", "path", list(path = root_upstream$path),
                       root = root_downstream)
  orderly_location_pull_metadata("upstream", root = root_downstream)

  index <- root_downstream$index$data()
  expect_length(index$metadata, 0)
  ## This is what we need to improve, everywhere
  expect_s3_class(index$location, "data.frame")
})


test_that("pull metadata from subset of locations", {
  root <- list()
  root$a <- create_temporary_root(use_file_store = TRUE)
  for (name in c("x", "y", "z")) {
    root[[name]] <- create_temporary_root(use_file_store = TRUE)
    orderly_location_add(name, "path", list(path = root[[name]]$path),
                         root = root$a)
  }

  expect_equal(orderly_location_list(root = root$a),
               c("local", "x", "y", "z"))

  ## NOTE: This is a little slow (0.2s) with about half of that coming
  ## from the call to utils::sessionInfo which gets bogged down
  ## reading DESCRIPTION files from disk - we might be better off
  ## replacing that with something a bit simpler. Also seeing some
  ## bottlenecks coming potentially from fs (fs::dir_create - looks
  ## like a known bug)
  ids <- list()
  for (name in c("x", "y", "z")) {
    ids[[name]] <- vcapply(1:3, function(i) create_random_packet(root[[name]]))
  }

  location_name <- c("x", "y", "z")

  orderly_location_pull_metadata(c("x", "y"), root = root$a)
  index <- root$a$index$data()
  expect_setequal(names(index$metadata), c(ids$x, ids$y))
  expect_equal(index$location$location, rep(location_name[1:2], each = 3))
  expect_equal(index$metadata[ids$x],
               root$x$index$data()$metadata)
  expect_equal(index$metadata[ids$y],
               root$y$index$data()$metadata)

  orderly_location_pull_metadata(root = root$a)
  index <- root$a$index$data()
  expect_setequal(names(index$metadata), c(ids$x, ids$y, ids$z))
  expect_equal(index$location$location, rep(location_name, each = 3))
  expect_equal(index$metadata[ids$z],
               root$z$index$data()$metadata)
})


test_that("Can't pull metadata from an unknown location", {
  root <- create_temporary_root()
  expect_error(
    orderly_location_pull_metadata("upstream", root = root),
    "Unknown location: 'upstream'")
})


test_that("No-op to pull metadata from no locations", {
  root <- create_temporary_root()
  expect_silent(orderly_location_pull_metadata("local", root = root))
  expect_silent(orderly_location_pull_metadata(root = root))
})


test_that("Can pull metadata through chain of locations", {
  root <- list()
  for (name in c("a", "b", "c", "d")) {
    root[[name]] <- create_temporary_root()
  }

  ## More interesting topology, with a chain of locations, but d also
  ## knowing directly about an earlier location
  ## > a -> b -> c -> d
  ## >      `--------/
  orderly_location_add("a", "path", list(path = root$a$path), root = root$b)
  orderly_location_add("b", "path", list(path = root$b$path), root = root$c)
  orderly_location_add("b", "path", list(path = root$b$path), root = root$d)
  orderly_location_add("c", "path", list(path = root$c$path), root = root$d)

  ## Create a packet and make sure it's in both b and c
  id1 <- create_random_packet(root$a)
  orderly_location_pull_metadata(root = root$b)
  orderly_location_pull_packet(id1, root = root$b)
  orderly_location_pull_metadata(root = root$c)
  orderly_location_pull_packet(id1, root = root$c)

  ## And another in just 'c'
  id2 <- create_random_packet(root$c)

  ## Then when we pull from d it will simultaneously learn about the
  ## packet from both locations:
  orderly_location_pull_metadata(root = root$d)
  index <- root$d$index$data()

  ## Metadata is correct
  expect_length(index$metadata, 2)
  expect_equal(names(index$metadata), c(id1, id2))
  expect_equal(index$metadata, root$c$index$data()$metadata)

  ## Location information contains both sources
  expect_equal(nrow(index$location), 3)
  expect_equal(index$location$packet, c(id1, id1, id2))

  expect_equal(index$location$location, c("b", "c", "c"))
})


test_that("can pull a packet from one location to another, using file store", {
  root <- list()
  for (name in c("src", "dst")) {
    root[[name]] <- create_temporary_root(use_file_store = TRUE)
  }

  id <- create_random_packet(root$src)
  orderly_location_add("src", "path", list(path = root$src$path),
                       root = root$dst)
  orderly_location_pull_metadata(root = root$dst)
  orderly_location_pull_packet(id, root = root$dst)

  index <- root$dst$index$data()
  expect_equal(index$unpacked, id)
  expect_true(file.exists(
    file.path(root$dst$path, "archive", "data", id, "data.rds")))
  meta <- outpack_metadata_core(id, root$dst)
  expect_true(all(root$dst$files$exists(meta$files$hash)))
})


test_that("can pull a packet from one location to another, archive only", {
  root <- list()
  for (name in c("src", "dst")) {
    root[[name]] <- create_temporary_root()
  }

  id <- create_random_packet(root$src)
  orderly_location_add("src", "path", list(path = root$src$path),
                       root = root$dst)
  orderly_location_pull_metadata(root = root$dst)
  orderly_location_pull_packet(id, root = root$dst)

  index <- root$dst$index$data()
  expect_equal(index$unpacked, id)
  expect_true(file.exists(
    file.path(root$dst$path, "archive", "data", id, "data.rds")))
})


test_that("detect and avoid modified files in source repository", {
  root <- list()
  for (name in c("src", "dst")) {
    root[[name]] <- create_temporary_root()
  }

  tmp <- fs::dir_create(temp_file())

  saveRDS(runif(10), file.path(tmp, "a.rds"))
  saveRDS(runif(10), file.path(tmp, "b.rds"))
  id <- character(2)
  for (i in seq_along(id)) {
    p <- outpack_packet_start(tmp, "data", root = root$src)
    outpack_packet_end(p)
    id[[i]] <- p$id
  }

  orderly_location_add("src", "path", list(path = root$src$path),
                       root = root$dst)
  orderly_location_pull_metadata(root = root$dst)

  ## Corrupt the file in the first id by truncating it:
  file.create(file.path(root$src$path, "archive", "data", id[[1]], "a.rds"))
  expect_message(
    orderly_location_pull_packet(id[[1]], root = root$dst),
    sprintf("Rejecting file 'a.rds' in 'data/%s'", id[[1]]))

  expect_equal(
    hash_file(file.path(root$dst$path, "archive", "data", id[[1]], "a.rds")),
    hash_file(file.path(root$src$path, "archive", "data", id[[2]], "a.rds")))
  expect_equal(
    hash_file(file.path(root$dst$path, "archive", "data", id[[1]], "b.rds")),
    hash_file(file.path(root$src$path, "archive", "data", id[[2]], "b.rds")))
})


test_that("Do not unpack a packet twice", {
  root <- list()
  for (name in c("src", "dst")) {
    root[[name]] <- create_temporary_root()
  }

  id <- create_random_packet(root$src)
  orderly_location_add("src", "path", list(path = root$src$path),
                       root = root$dst)
  orderly_location_pull_metadata(root = root$dst)
  orderly_location_pull_packet(id, root = root$dst)

  expect_equal(
    orderly_location_pull_packet(id, root = root$dst),
    character(0))
})


test_that("Sensible error if packet not known", {
  root <- list()
  for (name in c("src", "dst")) {
    root[[name]] <- create_temporary_root()
  }

  id <- create_random_packet(root$src)
  orderly_location_add("src", "path", list(path = root$src$path),
                       root = root$dst)
  expect_error(
    orderly_location_pull_packet(id, root = root$dst),
    "Failed to find packet at location 'src': '.+'")
})


test_that("Can pull a tree recursively", {
  ## Bit of tedious setup here; this just does a simple graph
  ## >  a -> b -> c
  root <- list()
  for (name in c("src", "dst")) {
    root[[name]] <- create_temporary_root()
  }

  id <- list(a = create_random_packet(root$src, "a"))

  src_b <- temp_file()
  fs::dir_create(src_b)
  code <- "saveRDS(readRDS('input.rds') * 2, 'output.rds')"
  writeLines(code, file.path(src_b, "script.R"))
  p_b <- outpack_packet_start(src_b, "b", root = root$src)
  id$b <- p_b$id
  outpack_packet_use_dependency(p_b, id$a, c("input.rds" = "data.rds"))
  outpack_packet_run(p_b, "script.R")
  outpack_packet_end(p_b)

  src_c <- temp_file()
  fs::dir_create(src_c)
  code <- "saveRDS(readRDS('input.rds') * 2, 'output.rds')"
  writeLines(code, file.path(src_c, "script.R"))
  p_c <- outpack_packet_start(src_c, "c", root = root$src)
  id$c <- p_c$id
  outpack_packet_use_dependency(p_c, id$b, c("input.rds" = "output.rds"))
  outpack_packet_run(p_c, "script.R")
  outpack_packet_end(p_c)

  orderly_location_add("src", "path", list(path = root$src$path),
                       root = root$dst)
  orderly_location_pull_metadata(root = root$dst)
  expect_equal(
    orderly_location_pull_packet(id$c, recursive = TRUE, root = root$dst),
    c(id$a, id$b, id$c))

  index <- root$dst$index$data()
  expect_equal(index$unpacked,
               root$src$index$data()$unpacked)

  expect_equal(
    orderly_location_pull_packet(id$c, recursive = TRUE, root = root$dst),
    character(0))
})


test_that("Can resolve locations", {
  root <- list()
  for (name in c("dst", "a", "b", "c", "d")) {
    root[[name]] <- create_temporary_root()
    if (name != "dst") {
      orderly_location_add(name, "path", list(path = root[[name]]$path),
                           root = root$dst)
    }
  }

  expect_equal(
    location_resolve_valid(NULL, root$dst, FALSE, FALSE),
    c("a", "b", "c", "d"))
  expect_equal(
    location_resolve_valid(NULL, root$dst, TRUE, FALSE),
    c("local", "a", "b", "c", "d"))
  expect_equal(
    location_resolve_valid(c("a", "b", "local", "d"), root$dst, FALSE, FALSE),
    c("a", "b", "d"))
  expect_equal(
    location_resolve_valid(c("a", "b", "local", "d"), root$dst, TRUE, FALSE),
    c("a", "b", "local", "d"))

  expect_error(
    location_resolve_valid(TRUE, root$dst, TRUE, FALSE),
    "Invalid input for 'location'; expected NULL or a character vector")

  expect_error(
    location_resolve_valid("other", root$dst, TRUE, FALSE),
    "Unknown location: 'other'")
  expect_error(
    location_resolve_valid(c("a", "b", "f", "g"), root$dst, TRUE, FALSE),
    "Unknown location: 'f', 'g'")
})


test_that("informative error message when no locations configured", {
  root <- create_temporary_root()
  expect_equal(
    location_resolve_valid(NULL, root, FALSE, TRUE),
    character(0))
  expect_error(
    location_resolve_valid(NULL, root, FALSE, FALSE),
    "No suitable location found")
  expect_error(
    orderly_location_pull_packet(outpack_id(), root = root),
    "No suitable location found")
})


## The test setup here is hard to do because we don't yet support
## having location_path filtering metadata to the packets that it can
## actually provide.
test_that("Can filter locations", {
  root <- list()
  for (name in c("dst", "a", "b", "c", "d")) {
    root[[name]] <- create_temporary_root()
    if (name != "dst") {
      orderly_location_add(name, "path", list(path = root[[name]]$path),
                           root = root$dst)
    }
  }

  ids_a <- vcapply(1:3, function(i) create_random_packet(root$a$path))
  orderly_location_add("a", "path", list(path = root$a$path), root = root$b)
  orderly_location_pull_metadata(root = root$b)
  orderly_location_pull_packet(ids_a, root = root$b)

  ids_b <- c(ids_a,
             vcapply(1:3, function(i) create_random_packet(root$b$path)))
  ids_c <- vcapply(1:3, function(i) create_random_packet(root$c$path))
  orderly_location_add("a", "path", list(path = root$a$path), root = root$d)
  orderly_location_add("c", "path", list(path = root$c$path), root = root$d)
  orderly_location_pull_metadata(root = root$d)
  orderly_location_pull_packet(ids_a, root = root$d)
  orderly_location_pull_packet(ids_c, root = root$d)
  ids_d <- c(ids_c,
             vcapply(1:3, function(i) create_random_packet(root$d$path)))

  orderly_location_pull_metadata(root = root$dst)

  ids <- unique(c(ids_a, ids_b, ids_c, ids_d))
  expected <- function(ids, location_name) {
    data_frame(packet = ids,
               location = location_name)
  }
  locs <- function(location) {
    location_resolve_valid(location, root$dst,
                           include_local = FALSE,
                           allow_no_locations = FALSE)
  }

  expect_equal(
    location_build_pull_plan(ids, locs(NULL), root = root$dst),
    expected(ids,
             c("a", "a", "a", "b", "b", "b", "c", "c", "c", "d", "d", "d")))
  ## Invert order:
  expect_equal(
    location_build_pull_plan(ids, locs(c("d", "c", "b", "a")), root = root$dst),
    expected(ids,
             c("d", "d", "d", "b", "b", "b", "d", "d", "d", "d", "d", "d")))
  ## Drop redundant locations
  expect_equal(
    location_build_pull_plan(ids, locs(c("b", "d")), root = root$dst),
    expected(ids,
             c("b", "b", "b", "b", "b", "b", "d", "d", "d", "d", "d", "d")))

  ## Some corner cases:
  expect_equal(
    location_build_pull_plan(ids_a[[1]], locs(NULL), root = root$dst),
    expected(ids_a[[1]], "a"))
  expect_equal(
    location_build_pull_plan(character(), locs(NULL), root = root$dst),
    expected(character(), character()))

  ## Failure to find things:
  err <- expect_error(
    location_build_pull_plan(ids, locs(c("a", "b", "c")), root = root$dst),
    "Failed to find packets at location 'a', 'b', 'c'")
})


test_that("nonrecursive pulls are prevented by configuration", {
  root <- list()
  for (name in c("src", "dst")) {
    root[[name]] <- create_temporary_root(require_complete_tree = TRUE)
  }

  id <- create_random_packet_chain(root$src, 3)

  expect_error(
    orderly_location_pull_packet(id[["c"]], recursive = FALSE, root = root$dst),
    "'recursive' must be TRUE (or NULL) with your configuration",
    fixed = TRUE)
})


test_that("if recursive pulls are required, pulls are recursive by default", {
  root <- list()
  for (name in c("src", "shallow", "deep")) {
    root[[name]] <- create_temporary_root(
      require_complete_tree = name == "deep")
  }

  id <- create_random_packet_chain(root$src, 3)

  for (r in root[c("shallow", "deep")]) {
    orderly_location_add("src", "path", list(path = root$src$path), root = r)
    orderly_location_pull_metadata(root = r)
  }

  orderly_location_pull_packet(id[["c"]], recursive = NULL, root = root$shallow)
  expect_equal(root$shallow$index$data()$unpacked, id[["c"]])

  orderly_location_pull_packet(id[["c"]], recursive = NULL, root = root$deep)
  expect_setequal(root$deep$index$data()$unpacked, id)
})


test_that("can't add unknown location type", {
  root <- create_temporary_root()
  expect_error(
    orderly_location_add("other", "magic", list(arg = 1), root = root),
    "type must be one of 'path', 'http'")
})


test_that("validate arguments to path locations", {
  root <- create_temporary_root()
  expect_error(
    orderly_location_add("other", "path", list(root = "mypath"),
                         root = root),
    "Fields missing from args: 'path'")
  expect_error(
    orderly_location_add("other", "http", list(server = "example.com"),
                         root = root),
    "Fields missing from args: 'url'")
  expect_equal(orderly_location_list(root = root), "local")
})


test_that("can load a custom location driver", {
  skip_if_not_installed("mockery")
  mock_driver <- mockery::mock("value")
  mock_gev <- mockery::mock(mock_driver)
  mockery::stub(orderly_location_custom, "getExportedValue", mock_gev)
  args <- list(driver = "foo::bar", a = 1, b = "other")
  expect_equal(orderly_location_custom(args), "value")

  mockery::expect_called(mock_gev, 1)
  expect_equal(mockery::mock_args(mock_gev)[[1]], list("foo", "bar"))

  mockery::expect_called(mock_driver, 1)
  expect_equal(mockery::mock_args(mock_driver)[[1]], list(a = 1, b = "other"))
})


test_that("can load a custom location driver using an R6 generator", {
  skip_if_not_installed("mockery")
  mock_driver <- structure(
    list(new = mockery::mock("value")),
    class = "R6ClassGenerator")
  mock_gev <- mockery::mock(mock_driver)
  mockery::stub(orderly_location_custom, "getExportedValue", mock_gev)
  args <- list(driver = "foo::bar", a = 1, b = "other")
  expect_equal(orderly_location_custom(args), "value")

  mockery::expect_called(mock_gev, 1)
  expect_equal(mockery::mock_args(mock_gev)[[1]], list("foo", "bar"))

  mockery::expect_called(mock_driver$new, 1)
  expect_equal(mockery::mock_args(mock_driver$new)[[1]],
               list(a = 1, b = "other"))
})


test_that("can add a custom outpack location", {
  skip_if_not_installed("mockery")
  root <- create_temporary_root()
  args <- list(driver = "foo::bar", a = 1, b = 2)
  orderly_location_add("a", "custom", args = args, root = root)

  loc <- as.list(root$config$location[2, ])
  expect_equal(loc$name, "a")
  expect_equal(loc$type, "custom")
  expect_equal(loc$args[[1]], list(driver = "foo::bar", a = 1, b = 2))

  mock_orderly_location_custom <- mockery::mock("value")
  mockery::stub(location_driver, "orderly_location_custom",
                mock_orderly_location_custom)
  expect_equal(location_driver(loc$name, root), "value")
  mockery::expect_called(mock_orderly_location_custom, 1)
  expect_equal(mockery::mock_args(mock_orderly_location_custom)[[1]],
               list(list(driver = "foo::bar", a = 1, b = 2)))
})


test_that("can pull packets as a result of a query", {
  root <- list()
  for (name in c("src", "dst")) {
    root[[name]] <- create_temporary_root(use_file_store = TRUE)
  }
  ids <- vcapply(1:3, function(i) {
    create_random_packet(root$src$path, parameters = list(i = i))
  })
  orderly_location_add("src", "path", list(path = root$src$path),
                       root = root$dst$path)
  ids_moved <- orderly_location_pull_packet(
    "parameter:i < 3",
    name = "data",
    options = list(pull_metadata = TRUE, allow_remote = TRUE),
    root = root$dst$path)
  expect_setequal(ids_moved, ids[1:2])
})


test_that("pull packet sets allow_remote to TRUE if not given", {
  root <- list()
  for (name in c("src", "dst")) {
    root[[name]] <- create_temporary_root()
  }

  id <- create_random_packet(root$src)
  orderly_location_add("src", "path", list(path = root$src$path),
                       root = root$dst)
  orderly_location_pull_metadata(root = root$dst)
  expect_error(
    orderly_location_pull_packet(NULL, options = list(allow_remote = FALSE),
                                 root = root$dst),
    "If specifying 'options', 'allow_remote' must be TRUE")
})


test_that("handle metadata where the hash does not match reported", {
  here <- create_temporary_root()
  there <- create_temporary_root()
  orderly_location_add("server", "path", list(path = there$path), root = here)
  id <- create_random_packet(there)

  path_metadata <- file.path(there$path, ".outpack", "metadata", id)
  json <- jsonlite::prettify(read_string(path_metadata))
  writeLines(json, path_metadata)

  err <- expect_error(
    orderly_location_pull_metadata(root = here),
    "Hash of metadata for '.+' from 'server' does")
  expect_equal(
    unname(err$message),
    sprintf("Hash of metadata for '%s' from 'server' does not match!", id))
  expect_equal(names(err$body), c("i", "i", "x", "i"))
  expect_match(err$body[[3]], "This is bad news")
  expect_match(err$body[[4]], "remove this location")
})


test_that("handle metadata where two locations differ in hash for same id", {
  root <- list()
  for (name in c("a", "b", "us")) {
    root[[name]] <- create_temporary_root()
  }

  id <- outpack_id()
  create_random_packet(root$a, id = id)
  create_random_packet(root$b, id = id)

  orderly_location_add("a", "path", list(path = root$a$path), root = root$us)
  orderly_location_add("b", "path", list(path = root$b$path), root = root$us)

  orderly_location_pull_metadata(location = "a", root = root$us)
  err <- expect_error(
    orderly_location_pull_metadata(location = "b", root = root$us),
    "Location 'b' has conflicting metadata")
  expect_equal(names(err$body), c("x", "i", "i", "i"))
  expect_match(err$body[[1]],
               "We have been offered metadata from 'b' that has a different")
  expect_match(err$body[[2]], sprintf("Conflicts for: '%s'", id))
  expect_match(err$body[[3]], "please let us know")
  expect_match(err$body[[4]], "remove this location")
})
