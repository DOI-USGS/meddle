#' get the bounding box of the feature(s)
#'
#' Calculate the bounding box of the feature(s) in  lat/lon, formatted for metadata field entry
#'
#' @param obj a spatial object from the \code{sp} package
#' @return a list with \code{wbbox}, \code{ebbox}, \code{nbbox}, \code{sbbox} fields
#' @importFrom sp bbox proj4string
#' @examples
#' library(sp)
#' Sr1 <- Polygon(cbind(c(-89,-89.5,-89,-88.5,-89),c(42,42,44,44,42)))
#' Srs1 <- Polygons(list(Sr1), "s1")
#' Sr2 <- Polygon(cbind(c(-105,-105.5,-106,-105.5,-105),c(31.4,32,34,34,32)))
#' Srs2 <- Polygons(list(Sr2), "s2")
#' p <- SpatialPolygons(list(Srs1, Srs2), proj4string=CRS("+init=epsg:4326 +proj=longlat
#' +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#' p.df <- SpatialPolygonsDataFrame(Sr = p,
#'    data = data.frame(x=c(0,40), y=c(30,300), z=c(0,0), row.names=c('s1','s2')))
#' extract_feature(p.df)
#' \dontrun{
#' library(rgdal)
#' writeOGR(p.df, 'inst/extdata/example_shapefile/','example_shapefile',
#'    driver = 'ESRI Shapefile', overwrite_layer = TRUE)
#' }
#' @export
feature_bbox <- function(obj){
  UseMethod("feature_bbox")
}

#' @keywords internal
#' @export
feature_bbox.Spatial <- function(obj){
  if (!grepl(pattern = 'WGS84', proj4string(obj))){
    stop('obj must be in WGS84 to calculate a valid bounding box')
  }
  bounds <- bbox(obj)
  return(list(wbbox=bounds[1,1], ebbox=bounds[1,2],
              nbbox=bounds[2,2], sbbox=bounds[2,1]))
}

#' @keywords internal
#' @export
feature_bbox.sf <- function(obj){
  feature_bbox(sf::st_geometry(obj))
}

#' @importFrom sf st_crs st_bbox
#' @keywords internal
#' @export
feature_bbox.sfc <- function(obj){
  if (!grepl(pattern = 'WGS84', sf::st_crs(obj)$proj4string)){
    stop('obj must be in WGS84 to calculate a valid bounding box')
  }
  bounds <- sf::st_bbox(obj)
  return(list(wbbox=bounds[['xmin']], ebbox=bounds[['xmax']],
              nbbox=bounds[['ymax']], sbbox=bounds[['ymin']]))
}

feature_type <- function(obj){
  UseMethod("feature_type")
}
#' get the FGDC feature type of spatial object
#'
#' Extract the FGDC feature type from an \code{sp} object
#'
#' @param obj a spatial object from the \code{sf} package or \code{sp} package
#' @param return a list with \code{feature-ref} and \code{feature-type} fields
#' @details not all \code{sf} or \code{sp} spatial classes are supported.
#' If you run into one that you need, file an issue https://github.com/USGS-R/meddle/issues
#' @keywords internal
#' @export
feature_type.SpatialPolygons <- function(obj){
  list('feature-ref'="Vector", 'feature-type'="G-polygon")
}
#' @keywords internal
#' @export
feature_type.SpatialPoints <- function(obj){
  list('feature-ref'="Point", 'feature-type'="Point")
}

#' @keywords internal
#' @export
feature_type.sfc_LINESTRING <- function(obj){
  list('feature-ref'="Vector", 'feature-type'="String")
}
#' @keywords internal
#' @export
feature_type.sfc_MULTILINESTRING <- function(obj){
  list('feature-ref'="Vector", 'feature-type'="String")
}
#' @keywords internal
#' @export
feature_type.sfc_MULTIPOLYGON <- function(obj){
  list('feature-ref'="Vector", 'feature-type'="G-polygon")
}
#' @keywords internal
#' @export
feature_type.sfc_POLYGON <- function(obj){
  list('feature-ref'="Vector", 'feature-type'="G-polygon")
}
#' @keywords internal
#' @export
feature_type.sfc_POINT <- function(obj){
  list('feature-ref'="Point", 'feature-type'="Point")
}
#' @keywords internal
#' @export
feature_type.sfc_MULTIPOINT <- function(obj){
  list('feature-ref'="Point", 'feature-type'="Point")
}
#' @importFrom sf st_geometry
#' @keywords internal
#' @export
feature_type.sf <- function(obj){
  feature_type(sf::st_geometry(obj))
}

feature_count <- function(obj){
  UseMethod("feature_count")
}
#' get the feature count from a spatial object
#'
#' Tally the number of features in a spatial object
#'
#' @param obj a spatial object from the \code{sf} package or \code{sp} package
#' @param return a list with \code{feature-count} field
#' @details not all \code{sf} or \code{sp} spatial classes are supported.
#' If you run into one that you need, file an issue https://github.com/USGS-R/meddle/issues
#' @keywords internal
#' @export
feature_count.Spatial <- function(obj){
  list('feature-count'=length(obj))
}
#' @keywords internal
#' @export
feature_count.sfc_MULTIPOLYGON <- function(obj){
  list('feature-count'=length(obj))
}
#' @keywords internal
#' @export
feature_count.sfc_POINT <- function(obj){
  list('feature-count'=length(obj))
}

#' @importFrom sf st_cast
#' @keywords internal
#' @export
feature_count.sfc_MULTIPOINT <- function(obj){
  list('feature-count'=length(st_cast(obj, to = 'POINT')))
}
#' @keywords internal
#' @export
feature_count.sfc_POLYGON <- function(obj){
  list('feature-count'=length(obj))
}
#' @keywords internal
#' @export
feature_count.sfc_LINESTRING <- function(obj){
  list('feature-count'=length(obj))
}
#' @keywords internal
#' @export
feature_count.sfc_MULTILINESTRING <- function(obj){
  list('feature-count'=length(st_cast(obj, to = 'LINESTRING')))
}
#' @keywords internal
#' @export
feature_count.sf <- function(obj){
  feature_count(sf::st_geometry(obj))
}

#' get the states that features overlap with
#'
#' summarize the spatial extent of data relative to overlap w/ US states
#'
#' @param obj a spatial object from the \code{sf} package or \code{sp} package
#' @return a list with \code{states} field
#' @details not all \code{sf} or \code{sp} spatial classes are supported.
#' If you run into one that you need, file an issue https://github.com/USGS-R/meddle/issues
#' @export
#' @examples
#' library(sf)
#' sfc = st_sfc(st_point(c(-89, 45)), st_point(c(-109, 42)), crs = 4326)
#' sf = st_sf(data.frame(val = c('WI_point', 'WY_point'), geom=sfc))
#' feature_states(sf)
feature_states <- function(obj){
  UseMethod("feature_states")
}

#' @keywords internal
#' @importFrom sf st_as_sf
#' @export
feature_states.Spatial <- function(obj){

  feature_states(sf::st_as_sf(obj))
}

#' @keywords internal
#' @export
#' @importFrom dataRetrieval stateCdLookup
feature_states.sfc <- function(obj){

  states <- get_states()

  state.overlap <- overlaps(obj, states)

  as.state_name <- function(x){
    s <- strsplit(tolower(x), " ")
    s_cap <- lapply(s, function(words) {
      ifelse(
        words == 'of',
        words,
        paste0(toupper(substring(words, 1,1)), substring(words, 2))
      )
    })
    sapply(s_cap, paste0, collapse=" ")
  }
  state.names <- unname(sapply(states$ID[state.overlap], as.state_name))

  feature.states <- lapply(sort(state.names), function(x){
    list('state-name'=x, 'state-abbr' = dataRetrieval::stateCdLookup(x))
  })
  return(list(states = feature.states))
}
#' @keywords internal
#' @export
#' @importFrom sf st_geometry
feature_states.sf <- function(obj){
  feature_states(st_geometry(obj))
}

#' @importFrom maps map
#' @import spData
#' @importFrom sf st_crs st_as_sf
get_states <- function(){
  us_48 <- sf::st_transform(spData::us_states, crs = 4326)
  # getting around dplyr::select(ID = NAME) since we don't have that pkg imported
  us_48$ID <- us_48$NAME
  us_48 <- us_48[c("ID", "geometry")]
  us_48$names <- paste0("USA:", us_48$ID)
  us_hi <- sf::st_as_sf(map("world", "USA:Hawaii", fill=TRUE, plot=FALSE))
  us_hi <- rename_geometry(us_hi, "geometry")
  us_hi$names <- rep("USA:Hawaii", nrow(us_hi))
  us_hi$ID <- rep("Hawaii", nrow(us_hi))
  us_ak <- sf::st_as_sf(map("world", "USA:Alaska", fill=TRUE, plot=FALSE))
  us_ak <- rename_geometry(us_ak, "geometry")
  us_ak$names <- rep("USA:Alaska", nrow(us_ak))
  us_ak$ID <- rep("Alaska", nrow(us_ak))
  us_pr <- sf::st_as_sf(map("world2Hires", "Puerto Rico", fill=TRUE, plot=FALSE))
  us_pr <- rename_geometry(us_pr, "geometry")
  us_pr$names <- rep("USA:Puerto Rico", nrow(us_pr))
  us_pr$ID <- rep("Puerto Rico", nrow(us_pr))
  sf::st_geometry(us_pr) <- sf::st_geometry(us_pr) - c(360, 0) # units for PR need a latitude shift
  sf::st_crs(us_pr) <- sf::st_crs(4326)
  us_ak <- sf::st_transform(us_ak, crs = 4326)
  us_hi <- sf::st_transform(us_hi, crs = 4326)

  usa <- rbind(rbind(rbind(us_48, us_ak), us_hi), us_pr)
  return(usa)
}

#' a fix for geom names that are different in `sf` objects
#' from https://gis.stackexchange.com/questions/386584/sf-geometry-column-naming-differences-r
#' @keywords internal
#' @importFrom sf st_geometry<-
rename_geometry <- function(obj, name){
  current = attr(obj, "sf_column")
  names(obj)[names(obj)==current] <- name
  st_geometry(obj) <- name
  return(obj)
}


#' @importFrom sf st_intersects
overlaps <- function(x, y){
  UseMethod("overlaps")
}

overlaps.SpatialPoints <- function(x, y){
  overlaps(sf::st_as_sf(x), y)
}

overlaps.sfc <- function(x, y){
  unique(unlist(sf::st_intersects(x, y)))
}
#' @importFrom sp over
overlaps.SpatialPolygons <- function(x, y){
  unname(!is.na(over(y, x)))
}
#' @importFrom sp over
overlaps.SpatialPolygonsDataFrame <- function(x, y){
  unname(!is.na(over(y, x)))[, 1]
}

#' extract and summarize spatial data
#'
#' create metadata list from sp object
#'
#' @param obj a spatial object from the \code{sf} package or \code{sp} package
#' or a filepath that can be read in with \code{read_data()}
#' @param out desired outputs; must align with valid internal function names
#' Including \code{out} a character vector of summary values
#' @return a list according to names in spatial lookup tables for tag conversion
#' @export
#' @examples
#' library(sp)
#' Sr1 = Polygon(cbind(c(-89,-89.5,-89,-88.5,-89),c(42,42,44,44,42)))
#' Srs1 = Polygons(list(Sr1), "s1")
#' p = SpatialPolygons(list(Srs1), proj4string=CRS("+init=epsg:4326 +proj=longlat
#' +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#' extract_feature(p)
extract_feature <- function(obj, out){
  UseMethod("extract_feature")
}

#' @keywords internal
#' @export
extract_feature.character <- function(obj, out){
  stopifnot(file.exists(obj))
  extract_feature(read_data(obj), out = out)
}

#' @keywords internal
#' @export
extract_feature.Spatial <- function(obj, out = c('bbox', 'type', 'count', 'states')){
  feature <- list()
  for (fun in out){
    feature <- append(feature, do.call(paste0('feature_', fun), list(obj = obj)))
  }
  return(feature)
}

#' @keywords internal
#' @export
extract_feature.sfc <- function(obj, out = c('bbox', 'type', 'count', 'states')){
  feature <- list()
  for (fun in out){
    feature <- append(feature, do.call(paste0('feature_', fun), list(obj = obj)))
  }
  return(feature)
}

#' @importFrom sf st_geometry
#' @keywords internal
#' @export
extract_feature.sf <- function(obj, out = c('bbox', 'type', 'count', 'states')){
  extract_feature(sf::st_geometry(obj), out = out)
}
