# Exportaciones: CSV para Observation.org y listado completo de detecciones.
#
# Observation.org (formato de Instrucciones/observations-import-example.xlsx):
#   - Una observación por especie, punto de muestreo y día (hora local).
#   - Solo detecciones validadas como correctas (y anotaciones manuales). De
#     forma opcional, las no revisadas por encima de un umbral (is certain = False).
#   - Nunca: voz humana, ruidos, detecciones incorrectas ni dudosas.
#   - Valores fijos (inst/golem-config.yml): method, counting method y activity.
#   - notes: "ID mediante <clasificador>" según cómo se identificaron las
#     detecciones de esa observación (p. ej. "ID mediante BirdNet APP",
#     "ID mediante anotación manual" o, si se mezclan, los dos unidos con " + ").
#   - CSV en UTF-8 sin BOM, separado por comas, fechas AAAA-MM-DD, horas HH:MM,
#     coordenadas en grados decimales con punto.

# Columnas exactas del ejemplo de Observation.org, en su orden
observation_columns <- c(
  "date", "time", "scientific name", "lat", "lng", "accuracy", "number", "notes",
  "sex", "is certain", "is escape", "activity", "life stage", "method",
  "counting method", "related species", "external reference", "obscurity", "embargo date"
)

#' Detecciones con todos los datos necesarios para exportar
#'
#' Incluye la especie efectiva (corregida), la fecha y hora local del inicio
#' de cada detección y los datos del punto de muestreo.
#' @noRd
export_detections_base <- function(con, local_tz = "Europe/Madrid") {
  det <- DBI::dbGetQuery(con, "
    SELECT d.*, r.file_name, r.file_path, r.start_utc, r.recorder_type, r.model, r.device_id,
           r.datetime_reliable, r.site_id, s.name AS site_name, s.lat, s.lon, s.accuracy_m
    FROM detections d
    JOIN recordings r ON r.recording_id = d.recording_id
    LEFT JOIN sites s ON s.site_id = r.site_id
    ORDER BY r.start_utc, d.start_s")
  det <- add_effective_species(det)
  start <- parse_utc(det$start_utc) + det$start_s
  det$local_datetime <- format(start, tz = local_tz, "%Y-%m-%d %H:%M:%S")
  det$local_date <- substr(det$local_datetime, 1, 10)
  det$local_time <- substr(det$local_datetime, 12, 16)
  det
}

#' Prepara la exportación a Observation.org
#'
#' @param options Lista con: include_unverified (lógico), min_confidence
#'   (umbral para las no revisadas, 0-1), date_from, date_to (AAAA-MM-DD),
#'   site_ids.
#' @return Lista con rows (data.frame con las columnas de Observation.org),
#'   unmatched (especies sin correspondencia), missing_coords (grabaciones sin
#'   punto o coordenadas), unreliable (grabaciones con fecha poco fiable),
#'   n_detections y n_unverified.
#' @noRd
build_observation_export <- function(con, options = list(), local_tz = "Europe/Madrid") {
  det <- export_detections_base(con, local_tz)
  min_conf <- options$min_confidence %||% 0.5
  verified <- det$validation == "correct"
  unverified_ok <- isTRUE(options$include_unverified) & det$validation == "pending" &
    !is.na(det$confidence) & det$confidence >= min_conf
  keep <- det$category == "species" & (verified | unverified_ok)
  if (!is.null(options$date_from)) keep <- keep & det$local_date >= options$date_from
  if (!is.null(options$date_to)) keep <- keep & det$local_date <= options$date_to
  if (length(options$site_ids)) keep <- keep & det$site_id %in% options$site_ids
  det <- det[keep, , drop = FALSE]

  # Problemas que impiden exportar
  no_coords <- is.na(det$site_id) | is.na(det$lat) | is.na(det$lon)
  missing_coords <- unique(det[no_coords, c("recording_id", "file_name", "site_name")])
  tax <- resolve_taxonomy(con, det$species)
  unmatched <- tax[is.na(tax$target_name), c("source_name", "method")]
  unreliable <- unique(det[!as.logical(det$datetime_reliable), c("recording_id", "file_name")])

  ok <- !no_coords & det$species %in% tax$source_name[!is.na(tax$target_name)]
  det <- det[ok, , drop = FALSE]
  det$target_name <- tax$target_name[match(det$species, tax$source_name)]

  rows <- observation_rows(det)
  list(rows = rows, unmatched = unmatched, missing_coords = missing_coords,
       unreliable = unreliable, n_detections = nrow(det),
       n_unverified = sum(det$validation != "correct"))
}

#' Agrupa las detecciones: una observación por especie, punto y día
#' @noRd
observation_rows <- function(det) {
  if (nrow(det) == 0) {
    out <- as.data.frame(matrix(character(), ncol = length(observation_columns)))
    names(out) <- observation_columns
    return(out)
  }
  cfg <- observation_org_config()
  groups <- split(det, list(det$target_name, det$site_id, det$local_date), drop = TRUE)
  out <- do.call(rbind, lapply(groups, function(g) {
    data.frame(
      date = g$local_date[1],
      time = min(g$local_time),
      scientific_name = g$target_name[1],
      lat = round(g$lat[1], 6),
      lng = round(g$lon[1], 6),
      accuracy = if (is.na(g$accuracy_m[1])) NA_integer_ else as.integer(round(g$accuracy_m[1])),
      number = 1L,
      notes = observation_note(g$classifier, g$source, cfg),
      sex = NA_character_,
      is_certain = if (any(g$validation == "correct")) "True" else "False",
      is_escape = "False",
      activity = cfg$activity,
      life_stage = NA_character_,
      method = cfg$method,
      counting_method = cfg$counting_method,
      related_species = NA_character_,
      external_reference = NA_character_,
      obscurity = NA_character_,  # Observation.org oculta las especies sensibles
      embargo_date = NA_character_,
      stringsAsFactors = FALSE
    )
  }))
  names(out) <- observation_columns
  rownames(out) <- NULL
  out[order(out$date, out$time, out$`scientific name`), , drop = FALSE]
}

#' Valores fijos de Observation.org (inst/golem-config.yml)
#' @noRd
observation_org_config <- function() {
  cfg <- tryCatch(get_golem_config("observation_org"), error = function(e) list())
  list(
    method = cfg$method %||% "Oído",
    counting_method = cfg$counting_method %||% "Visto/Sin contar",
    activity = cfg$activity %||% "Presente",
    notes_template = cfg$notes_template %||% "ID mediante {classifier}",
    classifier_names = unlist(cfg$classifier_names %||% list(BirdNET = "BirdNet APP")),
    manual_name = cfg$manual_name %||% "anotación manual"
  )
}

#' Nombre visible de un clasificador
#'
#' Se busca por el principio del nombre interno ("BirdNET" vale para
#' "BirdNET-Analyzer 2.4.0"); si no está configurado, se usa el interno.
#' @noRd
classifier_display_name <- function(classifier, cfg = observation_org_config()) {
  names_map <- cfg$classifier_names
  vapply(classifier, function(x) {
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    hit <- names(names_map)[startsWith(tolower(x), tolower(names(names_map)))]
    if (length(hit)) unname(names_map[hit[which.max(nchar(hit))]]) else x
  }, character(1), USE.NAMES = FALSE)
}

#' Nota de una observación según cómo se identificaron sus detecciones
#'
#' Primero los modelos (en orden alfabético) y al final la anotación manual:
#' "ID mediante BirdNet APP", "ID mediante anotación manual" o
#' "ID mediante BirdNet APP + anotación manual".
#' @param classifier,source Columnas de las detecciones de la observación.
#' @noRd
observation_note <- function(classifier, source, cfg = observation_org_config()) {
  models <- classifier_display_name(classifier[source != "manual"], cfg)
  models <- sort(unique(models[!is.na(models)]))
  methods <- c(models, if (any(source == "manual")) cfg$manual_name)
  if (length(methods) == 0) methods <- cfg$manual_name
  gsub("{classifier}", paste(methods, collapse = " + "), cfg$notes_template, fixed = TRUE)
}

#' Escribe el CSV para Observation.org (UTF-8 sin BOM, comas, punto decimal)
#' @noRd
write_observation_csv <- function(rows, file) {
  old <- options(scipen = 100) # nunca notación científica en las coordenadas
  on.exit(options(old))
  rows <- rows[, observation_columns, drop = FALSE]
  con <- file(file, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  utils::write.csv(rows, con, row.names = FALSE, na = "")
}

#' Listado completo de detecciones y anotaciones (sin voz humana)
#'
#' @param options Lista con min_confidence (0-1; las manuales siempre entran)
#'   y only_validated (solo correctas).
#' @param lang Idioma de las cabeceras.
#' @noRd
build_detections_export <- function(con, options = list(), lang = "es",
                                    local_tz = "Europe/Madrid") {
  det <- export_detections_base(con, local_tz)
  keep <- det$category != "human" &
    (is.na(det$confidence) | det$confidence >= (options$min_confidence %||% 0))
  if (isTRUE(options$only_validated)) keep <- keep & det$validation == "correct"
  det <- det[keep, , drop = FALSE]
  recorder <- ifelse(is.na(det$model), det$recorder_type, det$model)
  out <- data.frame(
    det$file_name,
    ifelse(is.na(det$device_id), recorder, paste(recorder, det$device_id)),
    det$local_date, substr(det$local_datetime, 12, 19), rep(local_tz, nrow(det)),
    det$site_name, det$lat, det$lon,
    round(det$start_s, 2), round(det$end_s, 2), round(det$low_hz), round(det$high_hz),
    det$species, det$species_common, det$scientific_name,
    round(100 * det$confidence, 1),
    tr_values("validation.", det$validation, lang),
    tr_values("source.", det$source, lang),
    det$notes, det$validated_by,
    stringsAsFactors = FALSE
  )
  names(out) <- tr_values("export.col_", c(
    "file", "recorder", "date", "time", "timezone", "site", "lat", "lon", "start", "end",
    "low_freq", "high_freq", "scientific_name", "common_name", "birdnet_name", "confidence",
    "validation", "source", "notes", "reviewer"), lang)
  out
}
