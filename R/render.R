
#' render xml from data and template
#'
#' create metadata file from input data or yaml file
#' 
#' @md
#' @param data filepath or list
#' @param filename name of file to write metadata to
#' @param \dots additional lists or yaml filepaths to include or other arguments passed to methods
#'   (e.g., \code{template="metadata.mustache"})
#' @param template character template or a filepath to a template to user in
#'   rendering the metadata. If missing, a default template will be used.
#' @keywords internal
#' @examples
#' render(list('dogname'='fred','catname'='midred'),
#'    filename=NULL, list('dogname'='betty'), template="my dog's name is: {{dogname}}")
#'
#' @seealso \code{\link[whisker]{whisker.render}}
#'
#' @export
render <- function(data, filename, ...){
  UseMethod("render")
}

#' @describeIn render render text to a file from a yaml file
#' @importFrom yaml yaml.load_file
#' @export
render.character <- function(data, filename, ..., template){
  stopifnot(file.exists(data))
  config.text <- yaml::yaml.load_file(data, eval.expr = TRUE)
  current.dir <- getwd()
  # setting working directory to the file that is being used, evaluation will be relative to that directory
  setwd(dirname(data))
  external.resources <- config.text[['external']]
  config.text[['external']] <- NULL # for the time being
  tryCatch({
    # evaluate any function calls
    config.text <- lapply(config.text, eval_content)
    # now evaluate external resources
    for (j in seq_len(length(external.resources))){
      config.text <- append_list_replace(config.text, eval_content(external.resources[j]))
    }
  }, error = function(err){
    setwd(current.dir)
    stop(err)
  })
  setwd(current.dir)

  if(missing(template)) {
    render(data = config.text, filename = filename, ...)
  } else {
    render(data = config.text, filename = filename, ..., template=template)
  }
}


#' @describeIn render render text to a file from a list
#' @export
#' @importFrom whisker whisker.render
#' @importFrom utils packageName
render.list <- function(data, filename, ..., template){
  if (missing(template)){
    template <- system.file(package=packageName(), 'extdata', "FGDC_template.mustache")
  }

  text <- append_list_replace(data, ...)
  template <- as.template(template)
  output <- whisker::whisker.render(template, text)
  if (is.null(filename)){
    return(output)
  } else {
    cat(output, file = filename)
    xml <- xml2::read_xml(filename)
    xml2::write_xml(xml, filename)
  }

}

