# GEOCODING FOR FREE
# This set of functions uses tmap tools to access the OSM Nominatum API
# returning the geocode of an address, then converting to the format 
# used for the rtweet package

require(tmaptools)

geocode_for_free <- function (address, ...) 
{
    if (missing(address)) 
        stop("must supply address", call. = FALSE)
    stopifnot(is.atomic(address))
    res_geocode_osm <- tmaptools::geocode_OSM(address, ...)
    convert_osm_to_rtweet(res_geocode_osm)
}

convert_osm_to_rtweet <- function(geocode_osm) {
    place <- geocode_osm$query
    boxp <- geocode_osm$bbox %>%
        set_names(c("sw.lng", "sw.lat", "ne.lng", "ne.lat")) %>%
        .[c("sw.lng", "sw.lat", "ne.lng", "ne.lat")]
    point <- geocode_osm$coords %>%
        set_names(c("lng", "lat")) %>%
        .[c("lat", "lng")]
  return(as.coords(place, boxp, point))
}

as.coords <- function(place, box, point) {
    coords <- list(place = place, box = box, point =  point)
    class(coords) <- c("coords", "list")
    coords
}

near_geocode <- function(geocode, distance = 1000) {
    c(geocode$point, str_c(distance, "mi")) %>% str_c(collapse = ",") 
}
