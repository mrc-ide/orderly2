# This is an orderly script - edit it to suit your needs. You might include
#
# * orderly::orderly_parameters():
#       declare parameters that your report accepts
# * orderly::orderly_description():
#       describe your report with friendly names, descriptions and metadata
# * orderly::orderly_resource():
#       declare files in your source tree that are inputs
# * orderly::orderly_shared_resource():
#       use files from the root directory's 'shared/' directory
# * orderly::orderly_dependency():
#       use files from a previously-run packet
# * orderly::orderly_artefact():
#       declare files that you promise to produce, and describe them
# * orderly::orderly_strict_mode():
#       enable some optional checks
#
# See the docs for more information:
#     https://mrc-ide.github.io/orderly/reference/
#
# To generate templates without this header, pass template = FALSE to
# orderly_new(); this header can be safely deleted if you don't need it.
