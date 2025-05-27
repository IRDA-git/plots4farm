#' Découpe une parcelle en sous-polygones numérotés
#'
#' Filtre un objet \code{sf} de points selon le nom de parcelle,
#' assemble les points en polygone, crée une grille alignée,
#' réduit éventuellement chaque cellule (inset), ordonne les cellules
#' (nord→sud ou ouest→est), et assigne des identifiants séquentiels.
#'
#' @param points Un objet \code{sf} de géométries POINT, avec une colonne \code{Name}
#'   contenant les identifiants des parcelles.
#' @param nom.parcelle Chaîne de caractères, nom exact de la parcelle à filtrer (e.g. "C200-1").
#' @param id.start Entier, première valeur d'ID à attribuer aux cellules.
#' @param orientation Caractère, "Y" (nord→sud) ou "X" (ouest→est).
#' @param inv.orientation Booléen ; si TRUE, inverse l'ordre de tri.
#' @param nx Entier ≥1, nombre de colonnes dans la grille.
#' @param ny Entier ≥1, nombre de lignes dans la grille.
#' @param inset Nombre ≥0, distance en mètres pour réduire chaque cellule (buffer négatif).
#' @return Un \code{sf} de POLYGON contenant :
#'   \describe{
#'     \item{geometry}{la géométrie de chaque cellule}
#'     \item{id}{ID en caractère, débutant à \code{id.start}}
#'   }
#' @examples
#' \dontrun{
#' library(sf)
#' pts <- read_sf("plots_C200.shp")
#' grille <- plan.parcelle(
#'   points = pts,
#'   nom.parcelle = "C200-1",
#'   id.start = 101,
#'   orientation = "Y",
#'   inv.orientation = FALSE,
#'   nx = 8,
#'   ny = 1,
#'   inset = 0.5
#' )
#' plot(grille["id"])
#' }
#' @importFrom sf st_combine st_concave_hull st_buffer st_make_valid st_as_sf st_geometry_type st_is_empty st_coordinates st_centroid
#' @importFrom dplyr filter mutate arrange desc select
#' @importFrom stringr str_detect fixed
#' @import magrittr
#' @export
plan.parcelle <- function(points, nom.parcelle, id.start = 101, orientation = c("X", "Y"), inv.orientation = FALSE,
                          nx = 8, ny = 1, inset = 0) {
  # 1. Input checks
  if (!inherits(points, "sf"))  {
    stop("`points` doit être un objet sf.", call. = FALSE)
  }
  if (!is.character(nom.parcelle) || length(nom.parcelle) != 1) {
    stop("`nom.parcelle` doit être du texte (e.g. \"C200-1\").",
         call. = FALSE)
  }
  if (!is.character(orientation) || length(orientation) != 1) {
    stop("`orientation` doit être \"X\" si les parcelles sont orientées d'Ouest en Est ou \"Y\" si les parcelles sont orientées du Nord au Sud.")
  }
  orientation <- match.arg(orientation)
  if (!is.logical(inv.orientation) || length(inv.orientation) != 1) {
    stop("`inv.orientation` doit être TRUE ou FALSE.", call. = FALSE)
  }
  if (!(is.numeric(nx) && nx >= 1 && nx == as.integer(nx))) {
    stop("`nx` doit être un integer ≥ 1.", call. = FALSE)
  }
  if (!(is.numeric(ny) && ny >= 1 && ny == as.integer(ny))) {
    stop("`ny` doit être un integer ≥ 1.", call. = FALSE)
  }
  if (!(is.numeric(id.start) && id.start == as.integer(id.start))) {
    stop("`id_start` doit être un integer.", call. = FALSE)
  }
  if (!(is.numeric(inset) && inset >= 0)) {
    stop("`inset` doit être un nombre >= 0 (distance de buffer négatif en mètres).", call. = FALSE)
  }

  # 2. Filtre par le nom de parcelle
  subset <- points %>%
    dplyr::filter(stringr::str_detect(Name, stringr::fixed(nom.parcelle)))
  if (nrow(subset) == 0) {
    stop("Aucun point ne correspond au nom de parcelle :", nom.parcelle, call. = FALSE)
  }

  # 3. Combiner les points en polygones
  poly <- subset %>%
    sf::st_combine() %>% sf::st_concave_hull(ratio = 0) %>% sf::st_as_sf()
  if (!all(sf::st_geometry_type(poly) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("La géométrie résultant de la transformation des points n'est pas un POLYGON/MULTIPOLYGON.", call. = FALSE)
  }

  # 4. Création de la grille
  grid.sf <- create_aligned_grid(poly, nx = nx, ny = ny)
  if (!inherits(grid.sf, "sf") || nrow(grid.sf) == 0) {
    stop("La création de la grille a échoué ou renvoie 0 entités.",
         call. = FALSE)
  }

  # 5. Appliquer inset (buffer négatif) si demandé
  if (inset > 0) {
    grid.sf <- sf::st_buffer(grid.sf, dist = -inset)
    grid.sf <- sf::st_make_valid(grid.sf)
    grid.sf <- grid.sf[!sf::st_is_empty(grid.sf), ]
  }

  # 6. Ordonner selon l'orientation
  centroids <- sf::st_centroid(grid.sf$geometry)
  coords    <- sf::st_coordinates(centroids)[, orientation]
  grid.sf   <- grid.sf %>%
    dplyr::mutate(.mid = coords) %>%
    {
      if (orientation == "Y") {
        if (!inv.orientation) dplyr::arrange(., dplyr::desc(.mid)) else dplyr::arrange(., .mid)
      } else {
        if (!inv.orientation) dplyr::arrange(., .mid) else dplyr::arrange(., dplyr::desc(.mid))
      }
    } %>%
    dplyr::select(-.mid)

  # 6. Assignation des IDs
  expected_n <- nx * ny
  actual_n   <- nrow(grid.sf)
  if (actual_n != expected_n) {
    warning(sprintf(
      "%d parcelles attendues (nx * ny), mais %d obtenues. IDs assignés séquentiellement.",
      expected_n, actual_n
    ), call. = FALSE)
  } else {
    message(sprintf("Assignation de %d IDs (de %d à %d).",
                    actual_n, id.start, id.start + actual_n - 1))
  }
  grid.sf <- grid.sf %>%
    dplyr::mutate(Name = as.character(seq(id.start, by = 1, length.out = actual_n)))

  return(grid.sf)
}
