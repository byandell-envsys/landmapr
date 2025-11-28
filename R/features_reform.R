#' Reform Features as valid SF object
#'
#' @param features object from native-land.ca
#'
#' @return valid SF object
#' @export
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter mutate
#' @importFrom purrr transpose
#' @importFrom sf st_as_sf st_multipolygon st_polygon

features_reform <- function(features) {
  if(!length(features)) return(NULL)
  # Check if features is an error; probably wrong API key.
  is_error <- names(features)
  if(!is.null(is_error) && is_error == "error") {
    stop(features)
  }
  
  out <- tibble::as_tibble(
    lapply(purrr::transpose(
      purrr::transpose(features)$properties),
      unlist))
  
  # Set up geometry coordinates
  tran2 <- function(x) {
    # Some geometry$coordinates have a 3rd element (of 0) for points.
    # Take first two elements of every list element.
    # Transpose do make column matrix
    t(sapply(x, function(y) unlist(y)[1:2]))
  }
  tran2l <- function(x) tran2(x[[1]])
  
  # Main routine
  tran_feature <- function(feature) {
    type <- feature$geometry$type
    out <- switch(type,
                  Polygon = {
                    # Polygon has node dimension in second list level.
                    # Return as list of matrices.
                    lapply(feature$geometry$coordinates, tran2)
                  },
                  MultiPolygon = {
                    # MultiPolygon has node dimension in third level.
                    # Return as list of lists of matrices.
                    list(lapply(feature$geometry$coordinates, tran2l))
                  })
    
    # Split by polygon or multipolygon
    # If it fails, set to NA
    tryCatch(
      switch(type,
             Polygon = sf::st_polygon(out),
             MultiPolygon = sf::st_multipolygon(out)),
      error = function(e) NA)
  }
  geometry <- lapply(features, tran_feature)
  
  # Check of is.na takes care of issues not caught.
  # All issues currently taken care of.
  out <- dplyr::filter(out, !is.na(geometry)) |>
    dplyr::mutate(geometry = geometry[!is.na(geometry)])
  
  sf::st_as_sf(out)
}
