#'
#' Download and unzip Sudan map from Humanitarian Data Exchange
#' 
#' @param download_url Download URL for Sudan maps from Humanitarian Data
#'   Data exchange
#'   
#' @return A list of data source name and name/s of layer/s
#'
#'

download_sudan_maps <- function(download_url) {
  data_files <- list.files("data")
  
  if ("sudan_maps.zip" %in% data_files) {
    path_to_zip <- "data/sudan_maps.zip"
  } else {
    download.file(
      url = download_url, destfile = "data/sudan_maps.zip"
    )
    
    path_to_zip <- "data/sudan_maps.zip"
  }
  
  dsn <- unzip(zipfile = path_to_zip, list = TRUE) |>
    (\(x) gsub(pattern = "/*|[^/]+$", replacement = "", x = x$Name))() |>
    unique() |>
    (\(x) file.path("data", x))()
  
  unzip(zipfile = path_to_zip, overwrite = TRUE, exdir = "data")
    
  layers <- sf::st_layers(dsn) |>
    (\(x) x$name)()
  
  list(dsn = dsn, layers = layers)
}
