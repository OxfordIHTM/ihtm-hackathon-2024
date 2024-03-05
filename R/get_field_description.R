#'
#' Get field description
#' 
#' @param df Data.frame object for dataset from which the field name is
#'   to be looked from.
#' @param field_name A character value or a vector of characters values of 
#'   data field/s for which field description/s is/are needed.
#' 
#' @return A named character value or named character vector of field
#'   descriptions
#' 
#' @examples
#' get_field_description(child, "oedema")
#' 

get_field_description <- function(df = NULL,
                                  field_name = NULL) {
  ## Check whether df is NULL ----
  if (is.null(df)) {
    stop("Please provide data object for dataset from which the field name is to be looked from.")
  }
  
  ## Check whether field_name is NULL ----
  if (is.null(field_name)) {
    stop("Please provide field name/s for which field description/s should be retrieved for.")
  }
  
  if ("age" %in% names(df)) {
    field_description <- create_child_dictionary(df = df) |>
      dplyr::filter(field %in% field_name) |>
      (\(x) { y <- x$field_description; names(y) <- x$field; y })()
  }
  
  if ("anc4" %in% names(df)) {
    field_description <- create_maternal_dictionary(df = df) |>
      dplyr::filter(field %in% field_name) |>
      (\(x) { y <- x$field_description; names(y) <- x$field; y })()
  }
  
  if ("Cured" %in% names(df)) {
    field_description <- create_cmam_dictionary(df = df) |>
      dplyr::filter(field %in% field_name) |>
      (\(x) { y <- x$field_description; names(y) <- x$field; y })()
  }
  
  field_description
}
