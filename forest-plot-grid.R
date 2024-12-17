##' Rotate coordinates using a reference point and an azimut.
##' 
##' See: https://r.geocompx.org/geometry-operations and
##' https://r-spatial.github.io/sf/articles/sf3.html
##'
##' @title Rotate coordinates
##' @param x sf or sfc object.
##' @param A0_coords A0 (x, y) coordinates. Must be projected coordinates.
##' @param azimut True azimut (geographical) of the plot. Must be between 0° and 360°. 
##' @return Geometry after rotation
##' @author Ghislain Vieilledent
rotate_coord <- function(x, A0_coords, azimut) {
  # Degrees to radians
  r <- azimut * pi / 180
  m <- matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
  x_geom_rot <- (x - A0_coords) * m + A0_coords
  return(x_geom_rot)
}


##' @title Generate a regular grid from one point and one azimut
##' @param coords Lower left point coordinates in lon, lat c(x, y).
##' @param azimut True azimut (geographical) of the plot. Must be between 0° and 360°. 
##' @param proj Projected crs (eg. espg:3163).
##' @param distance Plot length (in m).
##' @param bin Cell size (in m).
##' @param output_dir Output directory.
##' @return Corners and grid as vector points and polygons.
##' @author Ghislain Vieilledent
##' @import sf
##' @import checkmate
get_grid <- function(coords, azimut, proj, distance=100, bin=10, output_dir=".") {

  # Check
  checkmate::assert_vector(coords, len=2)
  checkmate::assert_double(azimut, lower=0, upper=360)
  checkmate::assert_string(proj)
  checkmate::assert_true(distance %% bin == 0)
  
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
    sf::st_write(file.path(output_dir, "corners.gpkg"), delete_dsn=TRUE, quiet=TRUE)

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
    sf::st_write(file.path(output_dir, "grid.gpkg"), delete_dsn=TRUE, quiet=TRUE)
}

##' @title Get tree lon/lat coordinates
##' @param tree_id Tree ID.
##' @param tree_xy Matrix of tree XY coordinates (2 columns X, Y) with A0 origin.
##' @param A0_coords A0 coordinates in lon, lat c(x, y).
##' @param azimut True azimut (geographical) of the plot. Must be between 0° and 360°. 
##' @param proj Projected crs (eg. espg:3163).
##' @param output_file 
##' @return Tree lon/lat coordinates
##' @author Ghislain Vieilledent
get_tree_lonlat <- function(trees_id, trees_xy, A0_coords, azimut, proj, output_file="trees.gpkg") {

  # Check
  checkmate::assert_vector(A0_coords, len=2)
  checkmate::assert_double(azimut, lower=0, upper=360)
  checkmate::assert_string(proj)
  
  # Initialize starting point
  A0 <- c("lon" = A0_coords[1], "lat" = A0_coords[2]) |> 
    sf::st_point(dim=XY) |>
    sf::st_geometry() |>
    sf::st_set_crs("epsg:4326")

  # Reproject A0
  A0_coords_proj <- A0 |>
    sf::st_transform(proj) |>
    sf::st_coordinates()

  # Add A0 coord to tree coord
  mat_A0 <- matrix(rep(A0_coords_proj, each=nrow(trees_xy)), ncol=2)
  trees_XY <- trees_xy + mat_A0
  trees_XY <- data.frame(X=trees_XY[, 1], Y=trees_XY[, 2])

  # Get projected tree geometry
  trees_geom <- trees_XY |> sf::st_as_sf(coords=c("X", "Y")) |> sf::st_geometry() |>
    sf::st_set_crs(proj)

  # Rotation
  trees_geom_rot <- rotate_coord(trees_geom, A0_coords_proj, azimut)

  # SF object
  trees_rot <- data.frame(name=trees_id) |>
    sf::st_set_geometry(trees_geom_rot) |>
    sf::st_set_crs(proj) |>
    sf::st_transform("epsg:4326")
  trees_lonlat <- trees_rot |> sf::st_coordinates()
  trees_rot$longitude <- trees_lonlat[, 1]
  trees_rot$latitude <- trees_lonlat[, 2]
  sf::st_write(trees_rot, output_file, delete_dsn=TRUE, quiet=TRUE)

  return(trees_lonlat)
}

# End
