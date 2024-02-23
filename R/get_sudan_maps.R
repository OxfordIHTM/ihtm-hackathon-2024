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


#'
#' Process Sudan maps to create matching codes with data
#'
#'

process_locality_map <- function(map_object = sudan2, df = child) {
  locality_id <- vector(mode = "integer", length = nrow(map_object))
  
  matched_names <- map_object$admin2Name_en |>
    (\(x) x[x %in% unique(df$locality_name)])()
  
  ref_list <- df |>
    dplyr::select(state_name, locality_name, locality_id) |>
    dplyr::distinct(state_name, locality_name, locality_id) |>
    dplyr::mutate(
      state_alt_name = dplyr::case_when(
        state_name == "Al-Gazeera" ~ "Aj Jazirah",
        state_name == "Sinar" ~ "Sennar",
        state_name == "Al-Gadarif" ~ "Gedaref",
        state_name == "South Kourdofan" ~ "South Kordofan",
        state_name == "West Kourdofan" ~ "West Kordofan",
        state_name == "North Kourdofan" ~ "North Kordofan",
        .default = state_name
      ),
      locality_name = gsub(pattern = "El", replacement = "Al", x = locality_name),
      locality_name = gsub(pattern = "Ed", replacement = "Ad", x = locality_name)
    ) |>
    dplyr::select("state_alt_name", "locality_name", "locality_id") |>
    dplyr::left_join(
      data.frame(map_object) |>
        dplyr::select(admin2Name_en, admin1Name_en, admin2Pcode, admin1Pcode),
      by = c("state_alt_name" = "admin1Name_en", "locality_name" = "admin2Name_en"),
      keep = TRUE
    )
  
  ref_list[ref_list$locality_name == "Sharg El Gezira", "admin2Pcode"]    <- "SD15033"
  ref_list[ref_list$locality_name == "Greater Medani", "admin2Pcode"]     <- "SD15030"
  ref_list[ref_list$locality_name == "Al Hassahisa", "admin2Pcode"]       <- "SD15034"
  ref_list[ref_list$locality_name == "Ganub Elgazira", "admin2Pcode"]     <- "SD15031"
  ref_list[ref_list$locality_name == "Um Elqura", "admin2Pcode"]          <- "SD15032"
  ref_list[ref_list$locality_name == "Al Damar", "admin2Pcode"]           <- "SD16008"
  ref_list[ref_list$locality_name == "Es Suki", "admin2Pcode"]            <- "SD14041"
  ref_list[ref_list$locality_name == "Al Dali", "admin2Pcode"]            <- "SD14039"
  ref_list[ref_list$locality_name == "Al Roseiris", "admin2Pcode"]        <- "SD08107"
  ref_list[ref_list$locality_name == "Ad Damazine", "admin2Pcode"]        <- "SD08105"
  ref_list[ref_list$locality_name == "Al Tadamon", "admin2Pcode"]         <- "SD08108"
  ref_list[ref_list$locality_name == "Nertiti", "admin2Pcode"]            <- "SD06131"
  ref_list[ref_list$locality_name == "North Jebel Mara", "admin2Pcode"]   <- "SD06132"
  ref_list[ref_list$locality_name == "Central Jebel Mara", "admin2Pcode"] <- "SD06139"
  
  
  ref_list[ref_list$locality_name == "Gedaref Town", "admin2Pcode"] <- "SD06139"
  
  
  
  x <- ref_list |>
    dplyr::filter(is.na(admin2Name_en))
  
  y <- data.frame(sudan2) |>
    dplyr::filter(!admin2Pcode %in% ref_list$admin2Pcode)
  
  y[stringdist::amatch(x$locality_name, y$admin2Name_en, method = "jw", maxDist = 0.45), ]
}
