#' append replace list
#'
#' merge multiple lists with replacement
#'
#' @param list0 the list to append to (w/ replacement)
#' @param \dots additional lists or .yaml filepaths to add to or replace elements in list0
#' @return the merged list with later elements in \dots replacing earlier ones
#' @details
#' order matters for ... arguments. The LAST argument will overwrite anything that proceeds it.
#' That means that \code{append_list_replace(list(dog='Larry'), list(dog='Cindy'))}
#' will use \code{dog='Cindy'}.
append_list_replace <- function(list0, ...){
  new.lists <- list(...)
  if (length(new.lists) < 1){
    return(list0)
  } else {
    list.out <- list0
    for (i in 1:length(new.lists)){

      obj <- new.lists[[i]]

      if (inherits(obj, 'character')){
        # assume it is a file path
        stopifnot(file.exists(obj))
        tmp.list <- yaml.load_file(obj, eval.expr = TRUE)
      } else if (inherits(obj, 'list')){
        tmp.list <- obj
      } else {
        stop('class "', class(obj), '" not supported')
      }
      if (is.null(names(tmp.list))){
        stop('currently, unnamed lists are not supported')
      }

      list.out[names(list.out) %in% names(tmp.list)] <- NULL
      list.out <- append(list.out, tmp.list)
    }
  }

  return(list.out)
}


#' evaluate content blocked within r code
#'
#' helper function for evaluating code from text
#'
#' @param x any R object
#' @return the return value from evaluating the function, or the raw input (unaltered)
#' if there is no function to be evaluated.
#' @details use "`r mean(c(2,3))`" to specify evaluating the \code{mean(c(2,3))}.
#' If this syntax is not used, the values are passed through unaltered.
#' @examples
#' eval_content("`r mean(c(2,3))`")
#' eval_content("`mean(c(2,3))`")
#' eval_content(list(c=3))
#' @keywords internal
#' @export
eval_content <- function(x){
  if (grepl('`r ',  x[1])){
    if (length(x) > 1)
      stop('arrays not supported for evaluation', call. = FALSE)
    exp <- gsub("`r (.*?)`", "\\1", x)
    data <- eval(parse(text=as.character(exp)))
  } else {
    data <- x
  }
  return(data)
}



as.template <- function(x){
  stopifnot(is.character(x))
  if (file.exists(x)){
    x <- paste(readLines(x),collapse = '\n')
  }
  return(x)
}

as.template.connection <- function(x){
  stop('not implemented')
}
