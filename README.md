
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geocrs

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of geocrs is to provide tools for the basic validation and
manipulation of coordinate reference system objects. It doesn’t require
a copy of PROJ, but does contain a partial copy of the latest PROJ
database.

## Installation

You can install the development version of geocrs from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("paleolimbot/geocrs")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(geocrs)

# create a CRS by guessing the input
as_crs(4326)
#> <geocrs_code>
#> EPSG:4326
as_crs("EPSG:4326")
#> <geocrs_code>
#> EPSG:4326
as_crs("+proj=longlat +datum=WGS84 +no_defs +type=crs")
#> <geocrs_proj4>
#> +proj=longlat +datum=WGS84 +no_defs +type=crs
```

Basic validation:

``` r
crs_code(0)
#> Error in crs_code(0): No such CRS: 'EPSG:0'
crs_code(4326, auth_name = "ESPG")
#> Error in crs_code(4326, auth_name = "ESPG"): No such CRS: 'ESPG:4326'
crs_json("wheee")
#> Error: lexical error: invalid char in json text.
#>                                        wheee
#>                      (right here) ------^
```

Basic conversion:

``` r
as_crs_proj4(4326)
#> <geocrs_proj4>
#> +proj=longlat +datum=WGS84 +no_defs +type=crs
as_crs_wkt(4326, type = "WKT2:2019")
#> <geocrs_wkt>
#> GEOGCRS["WGS 84", DATUM["World Geodetic System 1984", ELLIPSOID["WGS 84",6378137,298.257223563, LENGTHUNIT["metre",1]]], PRIMEM["Greenwich",0, ANGLEUNIT["degree",0.0174532925199433]], CS[ellipsoidal,2], AXIS["geodetic latitude (Lat)",north, ORDER[1], ANGLEUNIT["degree",0.0174532925199433]], AXIS["geodetic longitude (Lon)",east, ORDER[2], ANGLEUNIT["degree",0.0174532925199433]], USAGE[ SCOPE["unknown"], AREA["World"], BBOX[-90,-180,90,180]], ID["EPSG",4326]]
as_crs_json(4326)
#> <geocrs_json>
#> {
#>   "type": "GeographicCRS",
#>   "name": "WGS 84",
#>   "datum": {
#>     "type": "GeodeticReferenceFrame",
#>     "name": "World Geodetic System 1984",
#>     "ellipsoid": {
#>       "name": "WGS 84",
#>       "semi_major_axis": 6378137,
#>       "inverse_flattening": 298.257223563
#>     }
#>   },
#>   "coordinate_system": {
#>     "subtype": "ellipsoidal",
#>     "axis": [
#>       {
#>         "name": "Geodetic latitude",
#>         "abbreviation": "Lat",
#>         "direction": "north",
#>         "unit": "degree"
#>       },
#>       {
#>         "name": "Geodetic longitude",
#>         "abbreviation": "Lon",
#>         "direction": "east",
#>         "unit": "degree"
#>       }
#>     ]
#>   },
#>   "area": "World",
#>   "bbox": {
#>     "south_latitude": -90,
#>     "west_longitude": -180,
#>     "north_latitude": 90,
#>     "east_longitude": 180
#>   },
#>   "id": {
#>     "authority": "EPSG",
#>     "code": 4326
#>   }
#> }
```
