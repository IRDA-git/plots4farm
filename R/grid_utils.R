#' Crée une grille alignée sur un polygone
#'
#' Génère une grille rectangulaire dont les cellules sont alignées
#' sur les arêtes d'un objet SF de type POLYGON ou MULTIPOLYGON.
#'
#' @param polygon Un objet 
#'   \code{sf} de type POLYGON ou MULTIPOLYGON.
#' @param nx Entier ≥ 1 : nombre de colonnes le long de l'axe X du polygone.
#' @param ny Entier ≥ 1 : nombre de lignes le long de l'axe Y du polygone.
#' @return Un \code{data.frame sf} contenant les sous-polygones (cellules) de la grille,
#'   découpés à l'intérieur du polygone original.
#' @examples
#' \dontrun{
#' library(sf)
#' poly <- st_read("mon_polygone.shp")
#' grille <- create_aligned_grid(poly, nx = 4, ny = 3)
#' plot(grille["geometry"])
#' }
#' @importFrom sf st_geometry st_coordinates st_minimum_rotated_rectangle st_polygon st_sf st_sfc st_crs st_intersection
#' @export
create_aligned_grid <- function(polygon, nx = 5, ny = 5) {
  
  # 1. Extract the raw coordinate matrix from the first (and only) geometry
  poly_geom <- sf::st_geometry(polygon)[[1]]
  coords_full <- sf::st_coordinates(poly_geom)[, 1:2]
  
  # 2. Deduplicate but preserve order: get the first 4 unique points
  coords_uniq <- coords_full[!duplicated(coords_full), , drop = FALSE]
  
  if (nrow(coords_uniq) >= 4) {
    pts4   <- coords_uniq[1:4, ]
    origin <- pts4[1, ]
    x_axis <- pts4[2, ] - origin   # vector from corner 1 → 2
    y_axis <- pts4[4, ] - origin   # vector from corner 1 → 4
  } else {
    # fallback to minimum rotated rectangle if we can’t find 4 corners
    mrr     <- sf::st_minimum_rotated_rectangle(poly_geom)
    mcoords <- sf::st_coordinates(mrr)[1:5, 1:2]
    origin  <- mcoords[1, ]
    x_axis  <- mcoords[2, ] - origin
    y_axis  <- mcoords[4, ] - origin
  }
  
  # 3. Build normalized break sequences
  xb <- seq(0, 1, length.out = nx + 1)
  yb <- seq(0, 1, length.out = ny + 1)
  
  # 4. Construct the grid cells in parallelogram‐space
  grid_polys <- vector("list", nx * ny)
  k <- 1
  for (i in seq_len(nx)) {
    for (j in seq_len(ny)) {
      p1 <- origin + x_axis * xb[i]     + y_axis * yb[j]
      p2 <- origin + x_axis * xb[i + 1] + y_axis * yb[j]
      p3 <- origin + x_axis * xb[i + 1] + y_axis * yb[j + 1]
      p4 <- origin + x_axis * xb[i]     + y_axis * yb[j + 1]
      grid_polys[[k]] <- sf::st_polygon(list(rbind(p1, p2, p3, p4, p1)))
      k <- k + 1
    }
  }
  
  # 5. Clip to the original polygon
  grid <- sf::st_sf(geometry = sf::st_sfc(grid_polys, crs = sf::st_crs(polygon)))
  sf::st_intersection(grid, polygon)
}