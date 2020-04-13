
#' Create a CRS from an object
#'
#' @param x An object that can be interpreted as a CRS
#' @param ... Unused
#'
#' @return An object of (at least) class
#' @export
#'
#' @examples
#' as_crs(4326)
#' as_crs("+proj=longlat +datum=WGS84 +no_defs +type=crs")
#' as_crs('
#'   GEOGCS[
#'     "WGS 84",
#'     DATUM[
#'       "WGS_1984",
#'       SPHEROID["WGS 84",6378137,298.257223563, AUTHORITY["EPSG","7030"]],
#'       AUTHORITY["EPSG","6326"]
#'     ],
#'     PRIMEM["Greenwich",0, AUTHORITY["EPSG","8901"]],
#'     UNIT["degree",0.0174532925199433, AUTHORITY["EPSG","9122"]],
#'     AUTHORITY["EPSG","4326"]
#'    ]
#' ')
#'
#' as_crs('
#' {
#'   "type":"GeographicCRS",
#'   "name":"WGS 84",
#'   "datum":{
#'     "type":"GeodeticReferenceFrame",
#'     "name":"World Geodetic System 1984",
#'     "ellipsoid":{
#'       "name":"WGS 84",
#'       "semi_major_axis":6378137,
#'       "inverse_flattening":298.257223563
#'      }
#'    },
#'    "coordinate_system":{
#'      "subtype":"ellipsoidal",
#'      "axis":[
#'        {
#'          "name":"Geodetic latitude",
#'          "abbreviation":"Lat",
#'          "direction":"north","unit":"degree"
#'        },
#'        {
#'          "name":"Geodetic longitude",
#'          "abbreviation":"Lon",
#'          "direction":"east","unit":"degree"
#'        }
#'      ]
#'    },
#'    "area":"World",
#'    "bbox":{
#'      "south_latitude":-90,
#'      "west_longitude":-180,
#'      "north_latitude":90,
#'      "east_longitude":180
#'    },
#'    "id":{
#'    "authority":"EPSG",
#'    "code":4326
#'   }
#' }
#' ')
#'
as_crs <- function(x, ...) {
  UseMethod("as_crs")
}

#' @export
as_crs.geocrs <- function(x, ...) {
  x
}

#' @export
as_crs.numeric <- function(x, ...) {
  crs_code(x, auth_name = "EPSG")
}

#' @export
as_crs.integer <- function(x, ...) {
  crs_code(x, auth_name = "EPSG")
}

#' @export
as_crs.character <- function(x, ...) {
  if (grepl("^\\s*[A-Z]+:[A-Z0-9.]+\\s*$", x)) {
    split <- strsplit(x, ":")[[1]]
    crs_code(split[2], split[1])
  } else if (grepl("^\\s*\\+", x)) {
    crs_proj4(x)
  } else if(grepl("^\\s*[A-Z_]+\\s*\\[", x)) {
    crs_wkt(x, type = "unknown")
  } else if(grepl("^\\s*\\{", x)) {
    crs_json(x)
  } else {
    stop(sprintf("Don't know how to interpret `%s` as a CRS.", x))
  }
}

#' Construct CRS objects
#'
#' @param code A code used to describe a CRS, as perscribed by the
#'   authority `auth_name`
#' @param auth_name An authority. Defaults to EPSG.
#' @param proj4string A PROJ4 string.
#' @param wkt Various flavours of WKT
#' @param type Optionally specify the flavor of WKT
#' @param json PROJ JSON
#' @inheritParams as_crs
#'
#' @return An object of (at least) class `geocrs`.
#' @export
#'
crs_code <- function(code, auth_name = "EPSG") {
  code <- trimws(code)
  auth_name <- trimws(auth_name)

  stopifnot(
    grepl("^[A-Z]+$", auth_name),
    grepl("^[A-Z0-9.]+$", code)
  )

  # check that code is in crs_view
  crs_view <- geocrs::geocrs_crs_view
  if (!any(crs_view$auth_name == auth_name & crs_view$code == code, na.rm = TRUE)) {
    stop(sprintf("No such CRS: '%s:%s'", auth_name, code))
  }

  new_geocrs_code(list(x = paste0(auth_name, ":", code)))
}

new_geocrs_code <- function(x) {
  structure(x, class = c("geocrs_code", "geocrs"))
}

#' @rdname crs_code
#' @export
crs_proj4 <- function(proj4string) {
  new_geocrs_proj4(list(x = proj4string))
}

#' @rdname crs_code
#' @export
as_crs_proj4 <- function(x, ...) {
  UseMethod("as_crs_proj4")
}

#' @export
as_crs_proj4.default <- function(x, ...) {
  as_crs_proj4(as_crs(x, ...))
}

#' @export
as_crs_proj4.geocrs_proj4 <- function(x, ...) {
  x
}

#' @export
as_crs_proj4.geocrs_code <- function(x, ...) {
  crs_view <- geocrs::geocrs_crs_proj4
  split <- strsplit(x$x, ":", fixed = TRUE)[[1]]
  result <- crs_view$as_proj4[crs_view$auth_name == split[1] & crs_view$code == split[2]]
  if (is.na(result)) {
    stop(sprintf("Can't represent '%s' as a proj4 string", x))
  }

  new_geocrs_proj4(list(x = result))
}

new_geocrs_proj4 <- function(x) {
  structure(x, class = c("geocrs_proj4", "geocrs"))
}

#' @rdname crs_code
#' @export
crs_wkt <- function(wkt, type = c("unknown", "WKT2:2015", "WKT2:2019", "WKT1:GDAL", "WKT1:ESRI")) {
  type <- match.arg(type)
  new_geocrs_wkt(list(x = wkt), type = type)
}

#' @rdname crs_code
#' @export
as_crs_wkt <- function(x, ...) {
  UseMethod("as_crs_wkt")
}

#' @export
as_crs_wkt.default <- function(x, type, ...) {
  as_crs_wkt(as_crs(x, ...), type)
}

#' @export
as_crs_wkt.geocrs_wkt <- function(x, type, ...) {
  x
}

#' @export
as_crs_wkt.geocrs_code <- function(x, type, ...) {
  if (type == "WKT2:2019") {
    crs_view <- geocrs::geocrs_crs_wkt2
  } else if (type == "WKT1:GDAL") {
    crs_view <- geocrs::geocrs_crs_wkt_gdal
  } else {
    stop(sprintf("Can't create CRS WKT with type '%s'", type))
  }

  crs_view <- geocrs::geocrs_crs_wkt2
  split <- strsplit(x$x, ":", fixed = TRUE)[[1]]
  result <- crs_view[[3]][crs_view$auth_name == split[1] & crs_view$code == split[2]]

  new_geocrs_wkt(list(x = result), type = type)
}

new_geocrs_wkt <- function(x, type) {
  structure(x, type = type, class = c("geocrs_wkt", "geocrs"))
}

#' @rdname crs_code
#' @export
crs_json <- function(json) {
  # check that JSON can be parsed
  obj <- jsonlite::fromJSON(json)
  new_geocrs_json(list(x = json))
}

#' @rdname crs_code
#' @export
as_crs_json <- function(x, ...) {
  UseMethod("as_crs_json")
}

#' @export
as_crs_json.default <- function(x, ...) {
  as_crs_json(as_crs(x, ...))
}

#' @export
as_crs_json.geocrs_json <- function(x, ...) {
  x
}

#' @export
as_crs_json.geocrs_code <- function(x, ...) {
  crs_view <- geocrs::geocrs_crs_json
  split <- strsplit(x$x, ":", fixed = TRUE)[[1]]
  result <- crs_view$as_json[crs_view$auth_name == split[1] & crs_view$code == split[2]]

  new_geocrs_json(list(x = result))
}

new_geocrs_json <- function(x, type) {
  structure(x, class = c("geocrs_json", "geocrs"))
}

#' @export
print.geocrs_json <- function(x, ...) {
  cat(sprintf("<%s>\n", class(x)[1]))
  cat(jsonlite::prettify(x$x, indent = 2))
  cat("\n")
  invisible(x)
}

#' @export
print.geocrs <- function(x, ...) {
  cat(sprintf("<%s>\n", class(x)[1]))
  cat(x$x)
  cat("\n")
  invisible(x)
}

#' @export
as.character.geocrs <- function(x, ...) {
  x$x
}

