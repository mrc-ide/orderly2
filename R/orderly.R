orderly_list_src <- function(path) {
  pos <- fs::dir_ls(file.path(path, "src"), type = "directory")
  basename(pos)[file_exists(file.path(pos, "orderly.R"))]
}
