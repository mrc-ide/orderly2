describe("server integration tests", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path <- root$path
  server <- outpack_server(path)
  url <- "http://localhost:8000"

  client_http <- orderly_location_http$new(url)
  client_path <- orderly_location_path$new(path)

  it("returns sensible list data when empty", {
    expect_identical(client_http$list(),
                     client_path$list())
  })

  ids <- create_random_packet_chain(path, 4)

  it("returns sensible list data when non-empty", {
    expect_identical(client_http$list(),
                     client_path$list())
  })

  it("returns compatible metadata", {
    expect_identical(trimws(client_http$metadata(ids)),
                     client_path$metadata(ids))
  })

  it("throws compatible error on missing metadata", {
    msg <- "Some packet ids not found: 'id-missing'"
    expect_error(client_http$metadata("id-missing"), msg)
    expect_error(client_path$metadata("id-missing"), msg)
  })

  it("can fetch files", {
    hash <- root$files$list()[[1]]
    dest <- temp_file()
    res <- client_http$fetch_file(hash, dest)
    expect_identical(res, dest)
    expect_identical(hash_file(dest), hash)
  })

  it("throws compatible error on missing file", {
    path1 <- temp_file()
    path2 <- temp_file()
    msg <- "Hash 'hash:abc123' not found at location"
    err_http <- expect_error(
      client_http$fetch_file("hash:abc123", path1),
      msg)
    err_path <- expect_error(
      client_path$fetch_file("hash:abc123", path1),
      msg)
    expect_false(file.exists(path1))
    expect_false(file.exists(path2))
  })
})


describe("http location integration tests", {
  root <- create_temporary_root(path_archive = "archive", use_file_store = TRUE)
  path <- root$path
  server <- outpack_server(path)
  url <- "http://localhost:8000"

  ids <- vcapply(1:3, function(i) create_random_packet(path))

  it("can pull metadata", {
    root_downstream <- create_temporary_root(use_file_store = TRUE)
    expect_null(names(root_downstream$index$data()$metadata))
    orderly_location_add("upstream", "http", list(url = url),
                         root = root_downstream)
    expect_equal(orderly_location_list(root = root_downstream),
                 c("local", "upstream"))
    orderly_location_pull_metadata("upstream", root = root_downstream)

    idx <- root_downstream$index$data()
    expect_equal(names(idx$metadata), ids)
  })

  it("can locate files from the store", {
    hash <- root$files$list()[[1]]
    dest <- temp_file()
    loc <- orderly_location_http$new(url)
    res <- loc$fetch_file(hash, dest)
    expect_identical(res, dest)
    expect_identical(hash_file(res), hash)
  })

  it("throws sensible error if file not found in store", {
    loc <- orderly_location_http$new(url)
    h <- "md5:c7be9a2c3cd8f71210d9097e128da316"
    dest <- temp_file()
    expect_error(
      loc$fetch_file(h, dest),
      "Hash 'md5:c7be9a2c3cd8f71210d9097e128da316' not found at location")
    expect_false(file.exists(dest))
  })

  it("can push into server", {
    skip_on_os("windows") # mrc-4442
    root_downstream <- create_temporary_root(use_file_store = TRUE)
    ids_downstream <- create_random_packet_chain(root_downstream, 3)
    orderly_location_add("upstream", "http", list(url = url),
                         root = root_downstream)
    plan <- orderly_location_push(ids_downstream[[3]], "upstream",
                                  root = root_downstream)
    expect_setequal(plan$packet_id, ids_downstream)
    idx <- root$index$data()
    expect_true(all(ids_downstream %in% names(idx$metadata)))
    expect_true(all(root_downstream$files$list() %in% root$files$list()))
  })

  it("throws sensible error if metadata hash does not match expected", {
    root_tmp <- create_temporary_root(use_file_store = TRUE)
    id_tmp <- create_random_packet(root_tmp)

    hash_bad <- hash_data("", "sha256")
    meta <- read_string(
      file.path(root_tmp$path, ".outpack", "metadata", id_tmp))

    ## Trigger the error directly:
    cl <- outpack_http_client$new(url)
    err <- expect_error(cl$post(sprintf("/packet/%s", hash_bad), meta,
                                httr::content_type("text/plain")),
                        "Hash of packet does not match")

    ## Then on the method, should return same error here:
    loc <- orderly_location_http$new(url)
    mock_get_metadata_hash <- mockery::mock(hash_bad)
    mockery::stub(loc$push_metadata, "get_metadata_hash",
                  mock_get_metadata_hash)
    expect_error(loc$push_metadata(id_tmp, root_tmp),
                 err$message,
                 fixed = TRUE)
  })

  it("throws sensible error if file hash does not match expected", {
    skip_on_os("windows") # mrc-4442
    loc <- orderly_location_http$new(url)

    tmp <- withr::local_tempfile()
    writeLines("correct", tmp)
    hash_correct <- hash_file(tmp, "sha256")
    writeLines("corrupt", tmp)
    hash_corrupt <- hash_file(tmp, "sha256")

    ## This error back is not incredible, as it's not clear what is
    ## expected, but that's something we can imporove on the rust
    ## front. This should never get surfaced to the user though.
    expect_error(
      loc$push_file(tmp, hash_correct),
      sprintf("Hash %s does not match file contents. Expected %s",
              hash_correct, hash_corrupt))
  })
})
