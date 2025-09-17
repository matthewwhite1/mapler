# sf error checker
sf_helper <- function(farms_coords) {
  # Error checking
  if (!any(class(farms_coords) == "sf") || !any(class(farms_coords) == "data.frame")) {
    stop("farms_coords must have both class sf and data.frame.")
  } else if (!any(sf::st_is_valid(farms_coords))) {
    stop("farms_coords does not have a valid geometry.")
  } else if (any(sf::st_is_empty(farms_coords))) {
    stop("farms_coords cannot have any empty geometries")
  } else if (!all(sf::st_geometry_type(farms_coords) %in% c("POINT", "MULTIPOINT"))) {
    stop("farms_coords geometries can only be POINT or MULTIPOINT")
  }
}
