## A simple content-addressable file store; we'll use this for the
## out-of-working-copy storage of outpack packets.  The basic
## structure is similar to the git store, in that we store hashes
## split at their first byte.  However, there's no clever packing, and
## the content is stored as is (not in git's 'blob <content>|tree
## <content>' format), and we include an additional layer with the
## hash used.
##' @importFrom R6 R6Class
file_store <- R6::R6Class(
  "file_store",
  cloneable = FALSE,
  public = list(
    path = NULL,

    initialize = function(path) {
      fs::dir_create(path)
      self$path <- path
      lockBinding("path", self)
    },

    filename = function(hash) {
      dat <- hash_parse(hash)
      file.path(self$path, dat$algorithm, substr(dat$value, 1, 2),
                substr(dat$value, 3, nchar(dat$value)))
    },

    get = function(hash, dst, overwrite) {
      src <- self$filename(hash)
      if (any(!file.exists(src))) {
        missing <- hash[!file.exists(src)]
        message <- sprintf("Hash not found in store:\n%s",
                           paste(sprintf("  - %s", missing), collapse = "\n"))
        stop(not_found_error(message, missing))
      }
      fs::dir_create(dirname(dst))

      # Set copy_mode = FALSE: files in the store are read-only. It's easier on
      # the user if we make them writable again.
      copy_files(src, dst, overwrite = overwrite, copy_mode = FALSE)

      invisible(dst)
    },

    exists = function(hash) {
      file.exists(self$filename(hash))
    },

    ## TODO: bulk set
    ##
    ## TODO: allow computing hash here with nothing (hash = NULL),
    ## though that requires working out what the algorithm should be.
    ## Our current use knows the hash at the point of insertion and
    ## the validation is very useful!
    put = function(src, hash, move = FALSE) {
      hash_validate_file(src, hash)
      dst <- self$filename(hash)
      if (!fs::file_exists(dst)) {
        fs::dir_create(dirname(dst))
        if (move) {
          fs::file_move(src, dst)
        } else {
          copy_files(src, dst, overwrite = FALSE)
        }
        fs::file_chmod(dst, "a-w")
      } else if (move) {
        unlink(src)
      }
      invisible(hash)
    },

    list = function() {
      files <- withr::with_dir(
        self$path,
        as.character(fs::dir_ls(recurse = 2, type = "file",
                                glob = "tmp/*", invert = TRUE)))
      sub("/", "", sub("/", ":", files))
    },

    destroy = function() {
      fs::dir_delete(self$path)
    },

    tmp = function() {
      path <- file.path(self$path, "tmp")
      fs::dir_create(file.path(self$path, "tmp"))
      tempfile(tmpdir = path)
    }
  )
)
