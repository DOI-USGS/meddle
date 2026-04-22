#' create a skeleton file (or safely append to existing) for attributes
#'
#' create skeleton from object
#'
#' @param object a regognized R object or vector of file paths that can be read in to a recognized object class
#' @param attr.file a file to write the attribute skeleton to
#' @param \dots additional parameters passed to methods
#' @details
#' if the attribute file (\code{attr.file}) doesn't exist, a skeleton formatted file is created.
#' If the file does exist, it is opened and any new fields are joined in. Existing content in
#' \code{attr.file} is not overwritten.
#'
#' @examples
#' \dontrun{
#' attr.file <- tempfile(fileext = '.csv')
#' attribute_skeleton(system.file(package='meddle','extdata','example_shapefile'), attr.file)
#' read_attr_file(attr.file)
#' }
#' @export
attribute_skeleton <- function(object, attr.file, key.col = NULL, ...){
  UseMethod("attribute_skeleton")
}

#' @keywords internal
#' @export
attribute_skeleton.data.frame <- function(object, attr.file, key.col = NULL, ...){
  if (!is.null(key.col)){
    update_attr_table_long(object, key.col, attr.file)
  } else {
    update_attr_table(attr_names(object), attr.file)
  }
}

#' attribute skeleton for an assumed file or files
#'
#' will attempt to figure out the content, read in the file(s) and then
#' pass to appropriate method
#'
#' @keywords internal
#' @export
attribute_skeleton.character <- function(object, attr.file = as.attr_file(object), key.col = NULL, ...){

  # attempt to read in the file based on extension
  class(object) <- get_filetype(object)

  if (!is.null(key.col)){
    data <- read_data(object)
    update_attr_table_long(data, key.col, attr.file)
  } else {
    update_attr_table(attr_names(object), attr.file)
  }
}

#' @keywords internal
#' @export
attribute_skeleton.file <- function(object, attr.file = as.attr_file(object), ...){

  attribute_skeleton.character(object=object, attr.file = attr.file, ...)
}

#' @keywords internal
#' @export
attribute_skeleton.SpatialPointsDataFrame <- function(object, attr.file, ...){
  attrs <- names(object)
  update_attr_table(attrs, attr.file)
}

#' @keywords internal
#' @export
attribute_skeleton.SpatialPolygonsDataFrame <- function(object, attr.file, ...){
  attrs <- names(object)
  update_attr_table(attrs, attr.file)
}

