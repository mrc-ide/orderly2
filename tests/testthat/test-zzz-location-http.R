describe("server integration tests", {
  root <- create_temporary_root(path_archive = NULL,
                                use_file_store = TRUE,
                                require_complete_tree = TRUE)
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
    dest <- temp_file()
    msg <- "Hash 'md5:abc123' not found at location"
    err_http <- expect_error(
      client_http$fetch_file("md5:abc123", dest),
      msg)
    expect_false(file.exists(dest))
  })

  it("can list missing packets", {
     new_id <- outpack_id()
     expect_setequal(client_http$list_unknown_packets(new_id), new_id)
     expect_setequal(client_http$list_unknown_packets(ids), character(0))
     expect_setequal(client_http$list_unknown_packets(c(new_id, ids)), new_id)
  })

  it("can list missing files", {
    existing_hashes <- orderly_metadata(ids[[1]], root = path)$files$hash
    new_hash <- hash_data(toString(runif(100)), algorithm = "sha256")

    expect_setequal(
      client_http$list_unknown_files(new_hash), new_hash)
    expect_setequal(
      client_http$list_unknown_files(existing_hashes), character(0))
    expect_setequal(
      client_http$list_unknown_files(c(new_hash, existing_hashes)), new_hash)
  })
})


describe("http location integration tests", {
  root <- create_temporary_root(path_archive = NULL,
                                use_file_store = TRUE,
                                require_complete_tree = TRUE)
  path <- root$path
  server <- outpack_server(path)
  url <- "http://localhost:8000"

  ids <- vcapply(1:3, function(i) create_random_packet(path))

  it("can pull metadata", {
    root_downstream <- create_temporary_root(use_file_store = TRUE)
    expect_null(names(root_downstream$index$data()$metadata))
    orderly_location_add_http("upstream", url = url, root = root_downstream)
    expect_equal(orderly_location_list(root = root_downstream),
                 c("local", "upstream"))
    orderly_location_fetch_metadata("upstream", root = root_downstream)

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

  it("can push a single packet", {
    root_downstream <- create_temporary_root(use_file_store = TRUE)
    ids_downstream <- create_random_packet(root_downstream, n_files = 1)
    orderly_location_add_http("upstream", url = url, root = root_downstream)

    plan <- orderly_location_push(ids_downstream, "upstream",
                                  root = root_downstream)
    expect_equal(length(plan$packet_id), 1)
    expect_equal(length(plan$files), 1)

    idx <- root$index$data()
    expect_true(all(ids_downstream %in% names(idx$metadata)))
    expect_true(all(root_downstream$files$list() %in% root$files$list()))
  })

  it("can push a packet chain into server", {
    root_downstream <- create_temporary_root(use_file_store = TRUE)
    ids_downstream <- create_random_packet_chain(root_downstream, 3)
    orderly_location_add_http("upstream", url = url, root = root_downstream)
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

    hash <- root_tmp$index$metadata(id_tmp)$files$hash
    meta <- read_string(
      file.path(root_tmp$path, ".outpack", "metadata", id_tmp))

    orderly_location_http$new(url)$push_file(
      find_file_by_hash(root_tmp, hash),
      hash
    )

    ## Trigger the error directly:
    cl <- outpack_http_client$new(url)
    err <- expect_error(
      cl$request(sprintf("/packet/%s", hash),
                 function(r) httr2::req_body_raw(r, meta, "text/plain")),
      "Expected hash '.+' but found '.+'")
  })

  it("throws sensible error if file hash does not match expected", {
    loc <- orderly_location_http$new(url)

    tmp <- withr::local_tempfile()
    writeLines("correct", tmp)
    hash_correct <- hash_file(tmp, "sha256")
    writeLines("corrupt", tmp)
    hash_corrupt <- hash_file(tmp, "sha256")

    expect_error(
      loc$push_file(tmp, hash_correct),
      sprintf("Expected hash '%s' but found '%s'",
              hash_correct, hash_corrupt))
  })
})
