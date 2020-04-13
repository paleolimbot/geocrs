
library(tidyverse)

db_file <- file.path("data-raw/proj-7.0.0.db")

# do this lazily because it takes 15 min or so to build PROJ
if (!file.exists(db_file)) {
  # download PROJ 7
  if (!dir.exists("data-raw/proj-7.0.0")) {
    source_url <- "https://download.osgeo.org/proj/proj-7.0.0.tar.gz"
    curl::curl_download(source_url, "data-raw/proj-source.tar.gz")
    untar("data-raw/proj-source.tar.gz", exdir = "data-raw")
  }

  # make sure the dir exists
  proj_dir <- list.files("data-raw", "^proj-[0-9.]+", include.dirs = TRUE, full.names = TRUE)
  stopifnot(dir.exists(proj_dir), length(proj_dir) == 1)

  # run configure and make (to make database)
  # this takes a hot second
  withr::with_dir(proj_dir, {
    system("./configure")
    system("make")
  })
  file.copy(file.path(proj_dir, "data", "proj.db"), db_file)
}

# extract some useful bits of the db into package data
db <- DBI::dbConnect(RSQLite::SQLite(), db_file)
src <- src_dbi(db)

# explore db tables
# src_tbls(src) %>%
#   set_names() %>%
#   map(~tbl(src, .x) %>% colnames())

# seems to be the view we're looking for
crs_view <- tbl(src, "crs_view") %>% collect()

defs <- crs_view %>%
  mutate(def = paste0(auth_name, ":", code)) %>%
  pull(def)

pb <- dplyr::progress_estimated(length(defs))
results_json <- map(defs, ~{
  pb$tick()$print()
  processx::run("projinfo", args = c("-o", "PROJJSON", .x))
})

pb <- dplyr::progress_estimated(length(defs))
results_proj4 <- map(defs, ~{
  pb$tick()$print()
  processx::run("projinfo", args = c("-o", "PROJ", .x))
})

pb <- dplyr::progress_estimated(length(defs))
results_proj4 <- map(defs, ~{
  pb$tick()$print()
  processx::run("projinfo", args = c("-o", "PROJ", .x))
})

pb <- dplyr::progress_estimated(length(defs))
results_wkt2 <- map(defs, ~{
  pb$tick()$print()
  processx::run("projinfo", args = c("-o", "WKT2:2019", .x))
})

pb <- dplyr::progress_estimated(length(defs))
results_wkt_gdal <- map(defs, ~{
  pb$tick()$print()
  processx::run("projinfo", args = c("-o", "WKT1:GDAL", .x))
})

# try to minify for disk size
json <- map_chr(results_json, "stdout") %>%
  str_replace_all("\n", "") %>%
  str_replace("^.*?\\{", "{") %>%
  str_replace_all('([\\[{])\\s+', "\\1") %>%
  str_replace_all('([:,])\\s+', "\\1") %>%
  str_replace_all('\\s+([\\]}])', "\\1") %>%
  str_remove(fixed('"$schema":"https://proj.org/schemas/v0.2/projjson.schema.json",'))

proj4 <- map_chr(results_proj4, "stdout") %>%
  str_replace(regex("^.*?\\+", dotall = TRUE, multiline = TRUE), "+") %>%
  str_trim()

proj4[str_detect(proj4, "string:")] <- NA_character_

wkt2 <- map_chr(results_wkt2, "stdout") %>%
  str_replace(regex("^.*?([A-Z_]+)\\[", dotall = TRUE, multiline = TRUE), "\\1[") %>%
  str_replace_all("\\s+", " ") %>%
  str_trim()

wkt_gdal <- map_chr(results_wkt_gdal, "stdout") %>%
  str_replace(regex("^.*?([A-Z_]+)\\[", dotall = TRUE, multiline = TRUE), "\\1[") %>%
  str_replace_all("\\s+", " ") %>%
  str_trim()

wkt_gdal[str_detect(wkt_gdal, "string:")] <- NA_character_

json_lst <- map(json, jsonlite::fromJSON)
json_aoi <- map_dfr(
  json_lst,
  ~as_tibble(.x$bbox %||% tibble(
      south_latitude = NA_real_,
      west_longitude = NA_real_,
      north_latitude = NA_real_,
      east_longitude = NA_real_
    )
  )
)

crs_codes <- tibble(auth_name = crs_view$auth_name, code = crs_view$code)

geoproj_crs_view <- crs_view
geoproj_crs_area_of_use <- bind_cols(crs_codes, json_aoi)
geoproj_crs_json <- bind_cols(crs_codes, tibble(as_json = json))
geoproj_crs_proj4 <- bind_cols(crs_codes, tibble(as_proj4 = proj4))
geoproj_crs_wkt2 <- bind_cols(crs_codes, tibble(wkt2 = wkt2))
geoproj_crs_wkt_gdal <- bind_cols(crs_codes, tibble(wkt_gdal = wkt_gdal))

usethis::use_data(geoproj_crs_view, overwrite = TRUE)
usethis::use_data(geoproj_crs_area_of_use, overwrite = TRUE)
usethis::use_data(geoproj_crs_json, overwrite = TRUE)
usethis::use_data(geoproj_crs_proj4, overwrite = TRUE)
usethis::use_data(geoproj_crs_wkt2, overwrite = TRUE)
usethis::use_data(geoproj_crs_wkt_gdal, overwrite = TRUE)

DBI::dbDisconnect(db)
