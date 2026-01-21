# Package index

## Run a report

- [`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md)
  : Run a report
- [`orderly_list_src()`](https://mrc-ide.github.io/orderly/reference/orderly_list_src.md)
  : List source reports

## From within a running report

These are the functions that get called from your orderly file

- [`orderly_strict_mode()`](https://mrc-ide.github.io/orderly/reference/orderly_strict_mode.md)
  : Enable orderly strict mode
- [`orderly_parameters()`](https://mrc-ide.github.io/orderly/reference/orderly_parameters.md)
  : Declare orderly parameters
- [`orderly_description()`](https://mrc-ide.github.io/orderly/reference/orderly_description.md)
  : Describe the current packet
- [`orderly_resource()`](https://mrc-ide.github.io/orderly/reference/orderly_resource.md)
  : Declare orderly resources
- [`orderly_shared_resource()`](https://mrc-ide.github.io/orderly/reference/orderly_shared_resource.md)
  : Copy shared resources into a packet directory
- [`orderly_dependency()`](https://mrc-ide.github.io/orderly/reference/orderly_dependency.md)
  : Declare a dependency
- [`orderly_artefact()`](https://mrc-ide.github.io/orderly/reference/orderly_artefact.md)
  : Declare orderly artefacts
- [`orderly_run_info()`](https://mrc-ide.github.io/orderly/reference/orderly_run_info.md)
  : Information about currently running report

## Read packet metadata

- [`orderly_metadata()`](https://mrc-ide.github.io/orderly/reference/orderly_metadata.md)
  : Read outpack metadata
- [`orderly_metadata_read()`](https://mrc-ide.github.io/orderly/reference/orderly_metadata_read.md)
  : Read outpack metadata json file
- [`orderly_metadata_extract()`](https://mrc-ide.github.io/orderly/reference/orderly_metadata_extract.md)
  : Extract metadata from orderly packets

## Search

- [`orderly_query()`](https://mrc-ide.github.io/orderly/reference/orderly_query.md)
  : Construct outpack query
- [`orderly_search_options()`](https://mrc-ide.github.io/orderly/reference/orderly_search_options.md)
  : Packet search options
- [`orderly_search()`](https://mrc-ide.github.io/orderly/reference/orderly_search.md)
  : Query orderly's database
- [`orderly_query_explain()`](https://mrc-ide.github.io/orderly/reference/orderly_query_explain.md)
  : Explain a query

## Initialise and configure

- [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  : Initialise an orderly repository
- [`orderly_config()`](https://mrc-ide.github.io/orderly/reference/orderly_config.md)
  : Read configuration
- [`orderly_config_set()`](https://mrc-ide.github.io/orderly/reference/orderly_config_set.md)
  : Set configuration options

## Interact with locations

### Manage locations

- [`orderly_location_add()`](https://mrc-ide.github.io/orderly/reference/orderly_location_add.md)
  [`orderly_location_add_path()`](https://mrc-ide.github.io/orderly/reference/orderly_location_add.md)
  [`orderly_location_add_http()`](https://mrc-ide.github.io/orderly/reference/orderly_location_add.md)
  [`orderly_location_add_packit()`](https://mrc-ide.github.io/orderly/reference/orderly_location_add.md)
  : Add a new location
- [`orderly_location_list()`](https://mrc-ide.github.io/orderly/reference/orderly_location_list.md)
  : List known pack locations
- [`orderly_location_remove()`](https://mrc-ide.github.io/orderly/reference/orderly_location_remove.md)
  : Remove a location
- [`orderly_location_rename()`](https://mrc-ide.github.io/orderly/reference/orderly_location_rename.md)
  : Rename a location

### Use locations

- [`orderly_location_fetch_metadata()`](https://mrc-ide.github.io/orderly/reference/orderly_location_fetch_metadata.md)
  : Fetch metadata from a location
- [`orderly_location_pull()`](https://mrc-ide.github.io/orderly/reference/orderly_location_pull.md)
  : Pull one or more packets from a location
- [`orderly_location_push()`](https://mrc-ide.github.io/orderly/reference/orderly_location_push.md)
  : Push tree to location

## Help for developing

- [`orderly_new()`](https://mrc-ide.github.io/orderly/reference/orderly_new.md)
  : Create a new report
- [`orderly_cleanup()`](https://mrc-ide.github.io/orderly/reference/orderly_cleanup.md)
  [`orderly_cleanup_status()`](https://mrc-ide.github.io/orderly/reference/orderly_cleanup.md)
  : Clean up source directory
- [`orderly_gitignore_update()`](https://mrc-ide.github.io/orderly/reference/orderly_gitignore_update.md)
  : Update a gitignore file
- [`orderly_interactive_set_search_options()`](https://mrc-ide.github.io/orderly/reference/orderly_interactive_set_search_options.md)
  : Set search options for interactive use

## Create plugins

- [`orderly_plugin_add_metadata()`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_add_metadata.md)
  : Add metadata from plugin
- [`orderly_plugin_context()`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_context.md)
  : Fetch plugin context
- [`orderly_plugin_register()`](https://mrc-ide.github.io/orderly/reference/orderly_plugin_register.md)
  : Register an orderly plugin

## Everything else

- [`orderly_copy_files()`](https://mrc-ide.github.io/orderly/reference/orderly_copy_files.md)
  : Copy files from a packet
- [`orderly_example()`](https://mrc-ide.github.io/orderly/reference/orderly_example.md)
  : Copy a simple orderly example
- [`orderly_example_show()`](https://mrc-ide.github.io/orderly/reference/orderly_example_show.md)
  : Show an example file
- [`orderly_prune_orphans()`](https://mrc-ide.github.io/orderly/reference/orderly_prune_orphans.md)
  : Prune orphan packet metadata
- [`orderly_validate_archive()`](https://mrc-ide.github.io/orderly/reference/orderly_validate_archive.md)
  : Validate unpacked packets.
- [`orderly_hash_file()`](https://mrc-ide.github.io/orderly/reference/orderly_hash.md)
  [`orderly_hash_data()`](https://mrc-ide.github.io/orderly/reference/orderly_hash.md)
  : Compute a hash
- [`orderly_parse_file()`](https://mrc-ide.github.io/orderly/reference/orderly_parse_file.md)
  [`orderly_parse_expr()`](https://mrc-ide.github.io/orderly/reference/orderly_parse_file.md)
  : Parse the orderly entrypoint script
- [`orderly_compare_packets()`](https://mrc-ide.github.io/orderly/reference/orderly_compare_packets.md)
  : Compare the metadata and contents of two packets.
- [`orderly_comparison_explain()`](https://mrc-ide.github.io/orderly/reference/orderly_comparison_explain.md)
  : Print the details of a packet comparison.
- [`orderly_migrate_source()`](https://mrc-ide.github.io/orderly/reference/orderly_migrate_source.md)
  : Migrate orderly source code
