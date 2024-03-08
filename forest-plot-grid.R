##' Rotate coordinates using a reference point and an azimut.
##' 
##' See: https://r.geocompx.org/geometry-operations and
##' https://r-spatial.github.io/sf/articles/sf3.html
##'
##' @title Rotate coordinates
##' @param x sf or sfc object.
##' @param coords Lower left point coordinates. Must be projected coordinates.
##' @param azimut Azimut of the plot. Must be between 270° and 360°. 
##' @return Geometry after rotation
##' @author Ghislain Vieilledent
rotate_coord <- function(x, coords, azimut) {
  # Degrees to radians
  r <- (azimut-360) * pi / 180
  m <- matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
  x_geom_rot <- (x - coords) * m + coords
  return(x_geom_rot)
}


##' @title Generate a regular grid from one point and one azimut
##' @param coords Lower left point coordinates in lat/lon, c(x, y).
##' @param azimut Azimut of the plot. Must be between 270° and 360°. 
##' @param distance Plot length (in m).
##' @param bin Cell size (in m).
##' @param proj Projected crs.
##' @return Corners and grid as vector points and polygons.
##' @author Ghislain Vieilledent
##' @import sf 
get_grid <- function(coords, azimut, proj, distance=100, bin=10) {
 
  # Initialize starting point
  A0 <- c("lon" = coords[1], "lat" = coords[2]) |> 
    sf::st_point(dim=XY) |>
    sf::st_geometry() |>
    sf::st_set_crs("epsg:4326")

  # Reproject
  A0_coords_proj <- A0 |>
    sf::st_transform(proj) |>
    sf::st_coordinates()

  # Corners
  corners_geom <- sf::st_make_grid(
    cellsize=bin,
    n=distance/bin,
    offset=A0_coords_proj,
    what="corners",
    crs=proj,
    square=TRUE)
  # Rotation
  corners_geom_rot <- rotate_coord(corners_geom, A0_coords_proj, azimut)
  # sf object
  letters <- rep(LETTERS[seq(1, distance/bin + 1)], distance/bin + 1)
  numbers <- rep(seq(0, distance/bin), each = distance / bin + 1)
  corners_rot <- data.frame(name = paste0(letters, numbers)) |>
    sf::st_set_geometry(corners_geom_rot) |>
    sf::st_set_crs(proj) |>
    sf::st_transform("epsg:4326") |>
    sf::st_write("corners.gpkg", delete_dsn=TRUE, quiet=TRUE)

  # Grid
  grid_geom <- sf::st_make_grid(
    cellsize=bin,
    n=distance/bin,
    offset=A0_coords_proj,
    what="polygons",
    crs=proj,
    square=TRUE)
  # Rotation
  grid_geom_rot <- rotate_coord(grid_geom, A0_coords_proj, azimut)
  # SF object
  letters <- rep(LETTERS[seq(1, distance/bin)], distance/bin)
  numbers <- rep(seq(1, distance/bin), each = distance / bin)
  grid_rot <- data.frame(name=paste0(letters, numbers)) |>
    sf::st_set_geometry(grid_geom_rot) |>
    sf::st_set_crs(proj) |>
    sf::st_transform("epsg:4326") |>
    sf::st_write("grid.gpkg", delete_dsn=TRUE, quiet=TRUE)
}

# End
