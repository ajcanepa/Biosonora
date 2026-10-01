# Listado de especies detectadas.
#
# Para cada especie (la corregida por el revisor si la hay): confianza máxima
# y media de BirdNET, número de detecciones, número de grabaciones y estado
# de validación. No se incluyen:
#   - las detecciones marcadas como incorrectas,
#   - la voz humana (privada) ni los ruidos (perros, motores...),
#   - las detecciones de BirdNET por debajo del umbral de confianza.
# Las anotaciones manuales no tienen confianza (no influyen en máximo y media).

#' Calcula el listado de especies
#'
#' @param filters Lista con min_confidence (0-1), recording_ids, site_ids,
#'   date_from y date_to (fechas locales "AAAA-MM-DD"). NULL = sin filtrar.
#' @param local_tz Zona horaria para filtrar por fecha.
#' @return data.frame ordenado por número de detecciones.
#' @noRd
compute_species_list <- function(con, filters = list(), local_tz = "Europe/Madrid") {
  det <- DBI::dbGetQuery(con, "
    SELECT d.*, r.start_utc, r.site_id FROM detections d
    JOIN recordings r ON r.recording_id = d.recording_id
    WHERE d.category = 'species' AND d.validation != 'incorrect'")
  det <- add_effective_species(det)

  min_conf <- filters$min_confidence %||% 0
  keep <- is.na(det$confidence) | det$confidence >= min_conf
  if (length(filters$recording_ids)) keep <- keep & det$recording_id %in% filters$recording_ids
  if (length(filters$site_ids)) keep <- keep & det$site_id %in% filters$site_ids
  if (!is.null(filters$date_from) || !is.null(filters$date_to)) {
    local_date <- as.Date(format(parse_utc(det$start_utc), tz = local_tz, "%Y-%m-%d"))
    if (!is.null(filters$date_from)) keep <- keep & !is.na(local_date) & local_date >= as.Date(filters$date_from)
    if (!is.null(filters$date_to)) keep <- keep & !is.na(local_date) & local_date <= as.Date(filters$date_to)
  }
  det <- det[keep, , drop = FALSE]
  if (nrow(det) == 0) return(empty_species_list())

  groups <- split(det, det$species)
  out <- do.call(rbind, lapply(groups, function(g) {
    conf <- g$confidence[!is.na(g$confidence)]
    common <- g$species_common[!is.na(g$species_common) & g$species_common != ""]
    counts <- table(factor(g$validation, levels = validation_states))
    data.frame(
      scientific_name = g$species[1],
      common_name = if (length(common)) names(sort(table(common), decreasing = TRUE))[1] else NA_character_,
      max_confidence = if (length(conf)) max(conf) else NA_real_,
      mean_confidence = if (length(conf)) mean(conf) else NA_real_,
      n_detections = nrow(g),
      n_recordings = length(unique(g$recording_id)),
      n_correct = as.integer(counts[["correct"]]),
      n_incorrect = 0L, # las incorrectas no entran en el listado
      n_doubtful = as.integer(counts[["doubtful"]]),
      n_pending = as.integer(counts[["pending"]]),
      stringsAsFactors = FALSE
    )
  }))
  out$status <- species_status(out)
  rownames(out) <- NULL
  out[order(-out$n_detections, out$scientific_name), , drop = FALSE]
}

#' Estado de validación de una especie
#'
#' "validated": al menos una detección confirmada como correcta.
#' "doubtful": ninguna correcta y alguna dudosa.
#' "pending": todas sin revisar.
#' @noRd
species_status <- function(x) {
  ifelse(x$n_correct > 0, "validated", ifelse(x$n_doubtful > 0, "doubtful", "pending"))
}

empty_species_list <- function() {
  data.frame(scientific_name = character(), common_name = character(),
             max_confidence = numeric(), mean_confidence = numeric(),
             n_detections = integer(), n_recordings = integer(), n_correct = integer(),
             n_incorrect = integer(), n_doubtful = integer(), n_pending = integer(),
             status = character(), stringsAsFactors = FALSE)
}

#' Guarda el listado en la base de datos (con los filtros usados)
#' @return list_id
#' @noRd
db_save_species_list <- function(con, species, filters, user = NA_character_) {
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "INSERT INTO species_lists (generated_at, generated_by, filters) VALUES (?, ?, ?)",
                   params = list(format_utc(Sys.time()), user,
                                 as.character(jsonlite::toJSON(filters, auto_unbox = TRUE, null = "null"))))
    id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
    if (nrow(species)) {
      items <- cbind(list_id = id, species)
      DBI::dbAppendTable(con, "species_list_items", items)
    }
  })
  id
}

#' Listado preparado para exportar a CSV (confianza en porcentaje)
#' @noRd
species_list_for_export <- function(species, lang = "es") {
  data.frame(
    scientific_name = species$scientific_name,
    common_name = species$common_name,
    max_confidence_pct = round(100 * species$max_confidence, 1),
    mean_confidence_pct = round(100 * species$mean_confidence, 1),
    n_detections = species$n_detections,
    n_recordings = species$n_recordings,
    n_correct = species$n_correct,
    n_doubtful = species$n_doubtful,
    n_pending = species$n_pending,
    validation_status = vapply(paste0("species_status.", species$status), tr, "", lang = lang),
    stringsAsFactors = FALSE
  )
}
