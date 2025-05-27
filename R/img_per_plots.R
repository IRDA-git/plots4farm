require(tidyverse)
require(sf)
require(exifr)

#' Associe des photos à des parcelles et numéros de champ, et les classe en dossiers
#'
#' Lit un shapefile de champs et un shapefile de parcelles, ainsi qu'un dossier de photos,
#' extrait les coordonnées GPS EXIF, crée un sf de points, projette,
#' joint spatialement avec les champs (NoChamp), puis avec les parcelles (Name),
#' et copie chaque fichier photo dans un dossier organisé par NoChamp/Name.
#'
#' @param parcelles Chemin vers le fichier vectoriel des parcelles contenant la colonne `Name`.
#' @param photos     Chemin vers le dossier contenant les fichiers image.
#' @param field_path Chemin vers le fichier vectoriel des champs contenant la colonne `NoChamp`.
#' @param proj       EPSG ou CRS cible pour la projection (par défaut 4326).
#' @param output_dir Chemin vers le dossier racine où classer les photos (créera sous-dossiers).
#' @return Un objet sf POINT avec les colonnes :
#'   - `Photo`   : chemin original du fichier image  
#'   - `NoChamp` : numéro du champ (ou NA)  
#'   - `Name`    : nom de la parcelle intersectée (ou NA)  
#'   - `geometry`: coordonnées projetées  
#' @examples
#' \dontrun{
#' result <- img_per_plots(
#'   parcelles  = "plots_C200.shp",
#'   field_path = "Champs.json",
#'   photos     = "photos_folder/",
#'   proj       = 32188,
#'   output_dir = "sorted_photos/"
#' )
#' }
#' @importFrom sf read_sf st_as_sf st_transform st_zm st_join
#' @importFrom exifr read_exif
#' @importFrom dplyr filter select
#' @export
img_per_plots <- function(parcelles, photos, field_path, proj = 4326, output_dir) {
  # 1. Charger et projeter les champs
  field_sf <- sf::read_sf(field_path) %>%
    sf::st_transform(proj) %>%
    sf::st_zm()
  
  # 2. Charger et projeter les parcelles
  shape_sf <- sf::read_sf(parcelles) %>%
    sf::st_transform(proj) %>%
    sf::st_zm()
  
  # 3. Extraire en bloc les métadonnées EXIF des images
  exif_df <- exifr::read_exif(photos, tags = c("SourceFile","GPSLatitude","GPSLongitude"), recursive = TRUE) %>%
    dplyr::filter(!is.na(GPSLatitude) & !is.na(GPSLongitude)) %>%
    dplyr::select(
      Photo     = SourceFile,
      Latitude  = GPSLatitude,
      Longitude = GPSLongitude
    )
  
  # 4. Transformer en sf POINT et projeter
  img_sf <- sf::st_as_sf(
    exif_df,
    coords = c("Longitude", "Latitude"),
    crs    = 4326
  ) %>%
    sf::st_transform(proj) %>%
    sf::st_zm()
  
  # 5. Jointure spatiale avec les champs pour récupérer NoChamp
  img_sf <- sf::st_join(
    img_sf,
    field_sf[, "NoChamp", drop = FALSE],
    join = sf::st_intersects
  )
  
  # 6. Jointure spatiale avec les parcelles pour récupérer Name
  img_sf <- sf::st_join(
    img_sf,
    shape_sf[, "Name", drop = FALSE],
    join = sf::st_intersects
  )
  
  # 7. Créer et organiser les dossiers, copier les photos
  if (missing(output_dir) || !nzchar(output_dir)) {
    stop("`output_dir` doit être spécifié et non vide.", call. = FALSE)
  }
  # Créer le dossier racine s'il n'existe pas
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(output_dir)) {
      stop(sprintf("Impossible de créer le dossier racine '%s'. Vérifiez les permissions ou le chemin.", output_dir), call. = FALSE)
    }
  }
  # Pour chaque photo, créer dossier field/plot et copier
  for (i in seq_len(nrow(img_sf))) {
    fc <- as.character(img_sf$NoChamp[i])
    pn <- as.character(img_sf$Name[i])
    # gérer les NA en les plaçant dans "NA"
    fc <- ifelse(is.na(fc) || fc == "", "NA", fc)
    pn <- ifelse(is.na(pn) || pn == "", "NA", pn)
    dest_dir <- file.path(output_dir, fc, pn)
    # Créer sous-dossier si nécessaire
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(dest_dir)) {
        warning(sprintf("Impossible de créer le sous-dossier '%s'. Photo non copiée.", dest_dir), call. = FALSE)
        next
      }
    }
    # copier le fichier, vérifier le succès
    ok <- file.copy(img_sf$Photo[i], file.path(dest_dir, basename(img_sf$Photo[i])))
    if (!ok) {
      warning(sprintf("Échec de la copie de '%s' vers '%s'.", img_sf$Photo[i], dest_dir), call. = FALSE)
    }
  }
  
  return(img_sf)
}



