# Equivalencias taxonómicas: nombres de BirdNET -> nombres de Observation.org.
#
# Observation.org usa la Lista IOC de Aves del Mundo; BirdNET usa la de
# eBird/Clements. Casi siempre coinciden, pero no siempre. Para cada nombre:
#   1. Si el usuario indicó una equivalencia, se usa esa ("user").
#   2. Si el nombre existe en IOC, se usa tal cual ("ioc").
#   3. Si es un nombre de Clements con equivalencia conocida en IOC ("clements").
#   4. Si no, queda "sin correspondencia" y hay que resolverlo antes de exportar.
# Fuente: Lista IOC v15.2 (CC BY 3.0), ver inst/extdata/taxonomia/LEEME.md

taxonomy_cache <- new.env(parent = emptyenv())

#' Lista IOC de especies (nombre científico, inglés y español)
#' @noRd
ioc_species <- function() {
  if (is.null(taxonomy_cache$ioc)) {
    taxonomy_cache$ioc <- utils::read.csv(app_sys("extdata", "taxonomia", "ioc_15.2_aves.csv"),
                                          encoding = "UTF-8", stringsAsFactors = FALSE)
  }
  taxonomy_cache$ioc
}

#' Equivalencias Clements -> IOC conocidas
#' @noRd
clements_to_ioc <- function() {
  if (is.null(taxonomy_cache$clements)) {
    taxonomy_cache$clements <- utils::read.csv(
      app_sys("extdata", "taxonomia", "clements_a_ioc_15.2.csv"),
      encoding = "UTF-8", stringsAsFactors = FALSE)
  }
  taxonomy_cache$clements
}

#' Crea la tabla de equivalencias del usuario si no existe
#' @noRd
db_init_taxonomy <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS taxon_map (
      source_name  TEXT PRIMARY KEY,   -- nombre en BirdNET / en la anotación
      target_name  TEXT NOT NULL,      -- nombre para Observation.org
      updated_by   TEXT,
      updated_at   TEXT
    )")
  invisible(con)
}

#' Equivalencias para un conjunto de nombres
#'
#' @param names Nombres científicos (los efectivos, tras las correcciones).
#' @return data.frame con source_name, target_name (NA = sin correspondencia),
#'   method ("user", "ioc", "clements" o "none"), spanish y english (nombres
#'   comunes IOC del nombre de destino).
#' @noRd
resolve_taxonomy <- function(con, names) {
  names <- unique(names[!is.na(names) & nzchar(names)])
  ioc <- ioc_species()
  clem <- clements_to_ioc()
  user <- DBI::dbGetQuery(con, "SELECT source_name, target_name FROM taxon_map")

  target <- rep(NA_character_, length(names))
  method <- rep("none", length(names))
  in_user <- match(names, user$source_name)
  in_ioc <- names %in% ioc$ioc_name
  in_clem <- match(names, clem$clements_name)

  target[in_ioc] <- names[in_ioc]
  method[in_ioc] <- "ioc"
  use_clem <- !in_ioc & !is.na(in_clem)
  target[use_clem] <- clem$ioc_name[in_clem[use_clem]]
  method[use_clem] <- "clements"
  use_user <- !is.na(in_user)
  target[use_user] <- user$target_name[in_user[use_user]]
  method[use_user] <- "user"

  idx <- match(target, ioc$ioc_name)
  data.frame(source_name = names, target_name = target, method = method,
             spanish = ioc$spanish[idx], english = ioc$english[idx],
             stringsAsFactors = FALSE)
}

#' Guarda (o quita, con target_name NULL/"") una equivalencia del usuario
#' @noRd
db_set_taxon_map <- function(con, source_name, target_name, user = NA_character_) {
  target_name <- trimws(target_name %||% "")
  if (!nzchar(target_name)) {
    DBI::dbExecute(con, "DELETE FROM taxon_map WHERE source_name = ?", params = list(source_name))
    return(invisible(TRUE))
  }
  DBI::dbExecute(con, "
    INSERT INTO taxon_map (source_name, target_name, updated_by, updated_at) VALUES (?, ?, ?, ?)
    ON CONFLICT(source_name) DO UPDATE SET target_name = excluded.target_name,
      updated_by = excluded.updated_by, updated_at = excluded.updated_at",
    params = list(source_name, target_name, user, format_utc(Sys.time())))
  invisible(TRUE)
}
