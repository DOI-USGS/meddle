
update_attr_table <- function(attrs, attr.file){
  new.attrs <- create_attr_table(attrs)
  old.attrs <- read_attr_file(attr.file)
  attr.table <- merge_attr_table(new.attrs, old.attrs)
  write_attr_file(attr.table, file = attr.file)
}

#' create attribute table for long/tidy-format data
#'
#' builds an attribute table where the key column gets one \code{edom}
#' (enumerated domain) row per unique value and all other columns get a
#' single \code{rdom} (range domain) row.
#'
#' @param data a data.frame of long/tidy-format data
#' @param key.col character name of the column that holds measured-quantity names
#' @return a data.frame with columns \code{attr-label}, \code{attr-def},
#'   \code{attr-defs}, \code{data-min}, \code{data-max}, \code{data-units},
#'   \code{domain-type}, \code{edomv}, \code{edomvd}, and \code{edomvds}
#' @keywords internal
create_attr_table_long <- function(data, key.col){
  cols <- names(data)
  rows <- list()
  for (col in cols) {
    if (col == key.col) {
      vals <- unique(data[[col]])
      for (v in vals) {
        rows[[length(rows) + 1]] <- data.frame(
          'attr-label'  = col,
          'attr-def'    = ' ',
          'attr-defs'   = ' ',
          'data-min'    = ' ',
          'data-max'    = ' ',
          'data-units'  = ' ',
          'domain-type' = 'edom',
          'edomv'       = as.character(v),
          'edomvd'      = ' ',
          'edomvds'     = ' ',
          stringsAsFactors = FALSE, check.names = FALSE
        )
      }
    } else {
      rows[[length(rows) + 1]] <- data.frame(
        'attr-label'  = col,
        'attr-def'    = ' ',
        'attr-defs'   = ' ',
        'data-min'    = ' ',
        'data-max'    = ' ',
        'data-units'  = ' ',
        'domain-type' = 'rdom',
        'edomv'       = ' ',
        'edomvd'      = ' ',
        'edomvds'     = ' ',
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

#' merge long-format attribute tables
#'
#' merge two long-format attribute tables, preserving user-filled values
#' from the older table. Matching is done on \code{(attr-label, edomv)} for
#' \code{edom} rows and on \code{attr-label} for \code{rdom} rows.
#'
#' @param new.attrs a new attribute table (data.frame) produced by
#'   \code{\link{create_attr_table_long}}
#' @param old.attrs an existing attribute table (data.frame), or \code{NULL}
#' @return a merged attribute table
#' @keywords internal
merge_attr_table_long <- function(new.attrs, old.attrs){
  if (!is.null(old.attrs) && 'domain-type' %in% names(old.attrs)){
    for (j in 1:nrow(new.attrs)){
      label <- new.attrs$`attr-label`[j]
      dtype <- new.attrs$`domain-type`[j]
      if (dtype == 'edom'){
        edomv <- new.attrs$`edomv`[j]
        match.i <- which(old.attrs$`attr-label` == label &
                           old.attrs$`domain-type` == 'edom' &
                           old.attrs$`edomv` == edomv)
        if (length(match.i) > 0) {
          new.attrs[j, ] <- old.attrs[match.i[1], ]
        }
      } else {
        match.i <- which(old.attrs$`attr-label` == label &
                           old.attrs$`domain-type` == 'rdom')
        if (length(match.i) > 0) {
          new.attrs[j, ] <- old.attrs[match.i[1], ]
        }
      }
    }
  }
  return(new.attrs)
}

#' update a long-format attribute file
#'
#' reads an existing attribute file (if present), creates a fresh attribute
#' table from the data, merges user-filled values back in, and writes the
#' result. This is the long-format counterpart to \code{\link{update_attr_table}}.
#'
#' @param data a data.frame of long/tidy-format data
#' @param key.col character name of the column that holds measured-quantity names
#' @param attr.file file path for the attribute CSV to read/write
#' @keywords internal
update_attr_table_long <- function(data, key.col, attr.file){
  new.attrs <- create_attr_table_long(data, key.col)
  old.attrs <- read_attr_file(attr.file)
  attr.table <- merge_attr_table_long(new.attrs, old.attrs)
  write_attr_file(attr.table, file = attr.file)
}

create_attr_table <- function(attrs){
  empty.ar <- rep(' ', length(attrs))
  data.frame('attr-label'=attrs, 'attr-def' = empty.ar,
             'attr-defs' = empty.ar, 'data-min' = empty.ar,
             'data-max' = empty.ar, 'data-units' = empty.ar,
             stringsAsFactors = FALSE, check.names = FALSE)
}

#' merge two attribute tables
#'
#' merge two attribute tables, assuming that the latter one
#' has more complete data, but the newer one has all of the applicable fields.
#' Fields in \code{old.attrs} that don't appear in \code{new.attrs} will be dropped.
#' Likewise, fields in \code{new.attrs} but not in \code{old.attrs} will retain
#' the values in \code{new.attrs}.
#'
#' @param new.attrs an attribute table (data.frame)
#' @param old.attrs an attribute table (data.frame)
#' @return a merged attribute table.
merge_attr_table <- function(new.attrs, old.attrs){

  # to do: check that these two share the same headers and number of columns
  if (!is.null(old.attrs)){
    old.labels <- old.attrs$`attr-label`
    for (j in 1:nrow(new.attrs)){
      attr.label <- new.attrs$`attr-label`[j]
      if (attr.label %in% old.labels){
        replc.i <- which(old.labels == attr.label)[1]
        new.attrs[j, ] <- old.attrs[replc.i, ]
      }
    }
  }
  return(new.attrs)
}

#' write a file formatted as attributes
#'
#' write external file for attributes
#'
#' @param x the data to be written to file
#' @param file a filepath to a file that is comma or tab delimited
#' @rdname attr_file
#' @export
write_attr_file <- function(x, file){
  ext <- get_filetype(file)
  if (ext == 'csvfile'){
    write.table(x, file, sep=',', row.names = FALSE, quote=TRUE)
  } else if (ext == 'tsvfile'){
    write.table(x, file, sep='\t', row.names = FALSE, quote=TRUE)
  } else stop(file, ' type not supported for write_attr_file', call. = FALSE)

}

#' read in a file formatted as attributes
#'
#' read in external file for attributes
#'
#' @param attr.file a filepath to a file that is comma or tab delimited
#' @return the data.frame of formatted attributes
#' @rdname attr_file
#' @export
read_attr_file <- function(attr.file){
  # have package default or user setting of delimiter for attr.table?
  if (file.exists(attr.file)){
    class(attr.file) <- get_filetype(attr.file)
    read_data(attr.file, check.names = FALSE, na.strings = ' ')
  } else {
    NULL
  }
}

#' read attributes from a recognized file type
#'
#' return just the header names (attributes) for delimited files or shapefiles
#'
#' @param x the full path for the file(s) or an object
#' @return the attributes as a character vector
#' @examples
#' # attribute names from a shapefile:
#' attr_names(system.file(package='meddle','extdata','example_shapefile'))
#'
#' # attribute names from a Spatial object:
#' sf <- read_data(system.file(package='meddle','extdata','example_shapefile'))
#' attr_names(sf)
#' @export
attr_names <- function(x){
  UseMethod("attr_names")
}


#' @export
#' @keywords internal
attr_names.character <- function(x){
  stopifnot(file.exists(x))
  class(x) <- get_filetype(x)
  attr_names(x)
}

split_normalize_headers <- function(x, sep){
  out <- strsplit(readLines(x, n = 1L), sep)[[1]]
  gsub('\"', "", out)
}

#' @export
#' @keywords internal
attr_names.gzfile <- function(x){
  if (grepl('tsv', x)){
    sep <- '[\t]'
  } else if (grepl('csv', x)){
    sep <- '[,]'
  } else {
    stop('type ', x, ' not recognized by read_data', call. = FALSE)
  }
  split_normalize_headers(gzfile(x), sep=sep)
}

#' @export
#' @keywords internal
attr_names.csvfile <- function(x){
  split_normalize_headers(x, sep = '[,]')
}

#' @export
#' @keywords internal
attr_names.tsvfile <- function(x){
  split_normalize_headers(x, sep = '[\t]')
}

#' @export
#' @keywords internal
attr_names.shapedir <- function(x){
  x <- file.path(x, dir(x))
  attr_names.shapefile(x)
}

#' @export
#' @keywords internal
attr_names.Spatial <- function(x){
  names(x)
}

#' @export
#' @keywords internal
#' @importFrom sf st_drop_geometry
attr_names.sf <- function(x){
  names(sf::st_drop_geometry(x))
}

#' @export
#' @keywords internal
attr_names.data.frame <- function(x){
  names(x)
}

#' get attributes from a shapefile
#'
#' extract the attribute names from a shapefile file collection.
#'
#'
#' @param filename a character vector of shapefile file names
#' @return a character vector of attribute names for use in metadata rendering
#'
#' @details
#' Vector of shapefile file names (the \code{filename} arguments) must include a *.dbf file.
#' This function only reads the shapefile's dbf, so it skips reading in the geometry.
#' @importFrom foreign read.dbf
#' @export
#' @keywords internal
attr_names.shapefile <- function(x){
  filename <- x
  dbf.file <- filename[grepl(pattern = '.dbf', x = filename)]
  data <- foreign::read.dbf(dbf.file)
  return(names(data))
}

#' create name for attributes file if one doesn't exist
#'
#' create attribute file name
#'
#' @param filename the name of the files that this attribute file will be based on
#' @return a filepath for an attributes file
#'
#' @importFrom tools file_path_sans_ext
as.attr_file <- function(filename){
  paste0(tools::file_path_sans_ext(filename[1]), '_attributes.csv')
}

#' read and format attributes from delimited file or data.frame
#'
#' formats an attribute table into a list appropriate for metadata rendering
#'
#' @param x a file name for attributes file or a data.frame from \code{\link{read_attr_file}}
#' @return a list of attributes and values for rendering
#' @keywords internal
#' @importFrom methods is
#' @export
as.attr_list <- function(x){
  UseMethod("as.attr_list")
}

#' @export
#' @keywords internal
as.attr_list.character <- function(x){
  stopifnot(file.exists(x))
  x <- read_attr_file(x)
  as.attr_list(x)
}

#' @export
#' @keywords internal
as.attr_list.data.frame <- function(x){
  if (!'domain-type' %in% names(x)){
    out <- list()
    for (i in 1:nrow(x)){
      out[[i]] <- as.list(x[i,])
    }
    return(list('attributes'=out))
  }

  out <- list()
  seen <- character(0)
  for (i in 1:nrow(x)){
    label <- x$`attr-label`[i]
    if (label %in% seen) next
    seen <- c(seen, label)
    dtype <- x$`domain-type`[i]
    if (dtype == 'rdom'){
      entry <- list(
        'attr-label' = x$`attr-label`[i],
        'attr-def'   = x$`attr-def`[i],
        'attr-defs'  = x$`attr-defs`[i],
        'rdom' = list(
          'data-min'   = x$`data-min`[i],
          'data-max'   = x$`data-max`[i],
          'data-units' = x$`data-units`[i]
        )
      )
    } else {
      edom.rows <- x[x$`attr-label` == label & x$`domain-type` == 'edom', ]
      edom.list <- lapply(1:nrow(edom.rows), function(k){
        list('edomv'  = edom.rows$`edomv`[k],
             'edomvd' = edom.rows$`edomvd`[k],
             'edomvds'= edom.rows$`edomvds`[k])
      })
      entry <- list(
        'attr-label' = edom.rows$`attr-label`[1],
        'attr-def'   = edom.rows$`attr-def`[1],
        'attr-defs'  = edom.rows$`attr-defs`[1],
        'edom' = edom.list
      )
    }
    out[[length(out) + 1]] <- entry
  }
  return(list('attributes'=out))
}

#' @export
#' @keywords internal
as.attr_list.list <- function(x){
  # validate here?
  return(x)
}
