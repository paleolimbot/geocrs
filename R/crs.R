
#' Construct CRS objects
#'
#' @param x An object that can be interpreted by either GDAL or PROJ
#'   as a CRS object.
#' @param code A code used to describe a CRS, as perscribed by the
#'   authority `auth_code`
#' @param auth_code An authority. Defaults to EPSG.
#' @param wkt,wkt2,wkt_esri Various flavours of WKT
#' @param
#'
#' @return An object of (at least) class `geocrs`.
#' @export
#'
crs_code <- function(code, auth_name = "EPSG") {
  new_geocrs_code(code, auth_name)
}

new_geocrs_code <- function(code, auth_name) {
  structure(list(code = code, auth_name = auth_name), class = c("geocrs_code", "geocrs"))
}

#' @rdname crs_code
#' @export
crs_proj4 <- function(proj4string) {
  new_geocrs_proj4(proj4string)
}

new_geocrs_proj4 <- function(proj4string) {
  structure(list(proj4string = proj4string), class = c("geocrs_proj4", "geocrs"))
}

#' @rdname crs_code
#' @export
crs_wkt <- function(wkt) {
  new_geocrs_wkt(wkt)
}

new_geocrs_wkt <- function(wkt2) {
  structure(list(wkt = wkt), class = c("geocrs_wkt", "geocrs"))
}

#' @rdname crs_code
#' @export
crs_wkt2 <- function(wkt2) {
  new_geocrs_wkt2(wkt2)
}

new_geocrs_wkt2 <- function(wkt2) {
  structure(list(wkt2 = wkt2), class = c("geocrs_wkt2", "geocrs"))
}

#' @rdname crs_code
#' @export
crs_wkt_esri <- function(wkt_esri) {
  new_geocrs_wkt(wkt_esri)
}

new_geocrs_wkt_esri <- function(wkt2) {
  structure(list(wkt_esri = wkt_esri), class = c("geocrs_wkt", "geocrs"))
}
