# Importación de resultados de BirdNET.
#
# Formatos admitidos (se reconocen por el NOMBRE de las columnas, no por su
# orden, para tolerar diferencias entre versiones):
#
#  - "analyzer_csv": CSV de BirdNET-Analyzer
#      Start (s), End (s), Scientific name, Common name, Confidence[, File, ...]
#  - "raven": tabla de selección de Raven (separada por tabuladores)
#      Selection, View, Channel, Begin Time (s), End Time (s), Low Freq (Hz),
#      High Freq (Hz), Common Name, Species Code, Confidence, Begin Path,
#      File Offset (s)   [tabla de un audio: SIN nombre científico]
#      La tabla combinada añade "Scientific Name". En ella "Begin Time" se
#      acumula entre archivos: el inicio real en cada audio es "File Offset (s)".
#  - "simple": tabla sencilla (p. ej. paquete birdnetR)
#      start, end, scientific_name, common_name, confidence
#
# Los tres formatos se han comprobado con archivos reales (analyzer_csv y
# raven, con BirdNET-Analyzer 2.4.0 ejecutado desde Biosonora).

# Banda de frecuencias que analiza BirdNET (se usa si el archivo no la indica)
birdnet_default_band <- c(0, 15000)

# Clases de BirdNET que no son especies. Las de voz humana son privadas.
birdnet_human_classes <- c("Human vocal", "Human non-vocal", "Human whistle")
birdnet_other_classes <- c("Dog", "Engine", "Environmental", "Fireworks", "Gun",
                           "Noise", "Power tools", "Siren")

birdnet_formats <- list(
  analyzer_csv = list(
    required = c("Start (s)", "End (s)", "Scientific name", "Common name", "Confidence"),
    map = c(start_s = "Start (s)", end_s = "End (s)", scientific_name = "Scientific name",
            common_name = "Common name", confidence = "Confidence", file_ref = "File")
  ),
  raven = list(
    required = c("Begin Time (s)", "End Time (s)", "Confidence"),
    map = c(start_s = "Begin Time (s)", end_s = "End Time (s)",
            scientific_name = "Scientific Name", common_name = "Common Name",
            confidence = "Confidence", file_ref = "Begin Path",
            low_hz = "Low Freq (Hz)", high_hz = "High Freq (Hz)", offset_s = "File Offset (s)")
  ),
  simple = list(
    required = c("start", "end", "scientific_name", "common_name", "confidence"),
    map = c(start_s = "start", end_s = "end", scientific_name = "scientific_name",
            common_name = "common_name", confidence = "confidence", file_ref = "file")
  )
)

#' Lee un archivo de resultados de BirdNET
#'
#' @param path Ruta del archivo (.csv o .txt).
#' @param display_name Nombre que se muestra en los mensajes (p. ej. el nombre
#'   original de un archivo subido).
#' @return data.frame con: file_ref, start_s, end_s, low_hz, high_hz,
#'   scientific_name, common_name, confidence, category. Atributo "format".
#' @noRd
read_birdnet_file <- function(path, display_name = basename(path)) {
  first_line <- tryCatch(readLines(path, n = 1, warn = FALSE, encoding = "UTF-8"),
                         error = function(e) character())
  if (length(first_line) == 0 || !nzchar(first_line)) {
    biosonora_abort("error.birdnet_empty", file = display_name)
  }
  sep <- if (grepl("\t", first_line, fixed = TRUE)) "\t" else ","
  raw <- tryCatch(
    utils::read.table(path, sep = sep, header = TRUE, quote = "\"", check.names = FALSE,
                      stringsAsFactors = FALSE, comment.char = "", encoding = "UTF-8",
                      na.strings = c("", "NA"), fill = TRUE, strip.white = TRUE),
    error = function(e) biosonora_abort("error.birdnet_unreadable", file = display_name)
  )
  names(raw) <- sub("^﻿", "", trimws(names(raw))) # BOM de Excel

  format <- detect_birdnet_format(names(raw))
  if (is.na(format)) {
    biosonora_abort("error.birdnet_unknown_format", file = display_name,
                    columns = paste(names(raw), collapse = ", "))
  }
  spec <- birdnet_formats[[format]]
  get <- function(field) {
    col <- spec$map[field]
    if (!is.na(col) && col %in% names(raw)) raw[[col]] else rep(NA, nrow(raw))
  }

  out <- data.frame(
    file_ref = as.character(get("file_ref")),
    start_s = suppressWarnings(as.numeric(get("start_s"))),
    end_s = suppressWarnings(as.numeric(get("end_s"))),
    low_hz = suppressWarnings(as.numeric(get("low_hz"))),
    high_hz = suppressWarnings(as.numeric(get("high_hz"))),
    scientific_name = trimws(as.character(get("scientific_name"))),
    common_name = trimws(as.character(get("common_name"))),
    confidence = suppressWarnings(as.numeric(get("confidence"))),
    stringsAsFactors = FALSE
  )

  # Raven: el inicio dentro de cada audio es "File Offset (s)"
  offset <- suppressWarnings(as.numeric(get("offset_s")))
  if (format == "raven" && any(!is.na(offset))) {
    duration <- out$end_s - out$start_s
    out$start_s <- ifelse(is.na(offset), out$start_s, offset)
    out$end_s <- out$start_s + duration
  }
  # La tabla de Raven de cada audio NO trae el nombre científico (solo el
  # común y el código de eBird): se traduce el código con la tabla del propio
  # BirdNET-Analyzer instalado. Si no está instalado, no se puede adivinar.
  missing_sci <- is.na(out$scientific_name) | out$scientific_name == ""
  if (format == "raven" && any(missing_sci)) {
    codes <- birdnet_code_map()
    code_col <- if ("Species Code" %in% names(raw)) as.character(raw[["Species Code"]]) else rep(NA, nrow(raw))
    from_code <- if (is.null(codes)) rep(NA_character_, nrow(raw)) else unname(codes[code_col])
    out$scientific_name[missing_sci] <- from_code[missing_sci]
    still <- is.na(out$scientific_name) | out$scientific_name == ""
    # Clases que no son especies (p. ej. "Human vocal"): su código es el propio nombre
    nonspecies <- still & out$common_name %in% c(birdnet_human_classes, birdnet_other_classes)
    out$scientific_name[nonspecies] <- out$common_name[nonspecies]
    if (any(is.na(out$scientific_name) | out$scientific_name == "")) {
      biosonora_abort("error.birdnet_raven_no_scientific", file = display_name)
    }
  }

  validate_birdnet_rows(out, display_name)
  out$category <- birdnet_category(out$scientific_name)
  attr(out, "format") <- format
  out
}

#' Reconoce el formato por los nombres de columna (NA si no es ninguno)
#' @noRd
detect_birdnet_format <- function(columns) {
  for (format in names(birdnet_formats)) {
    if (all(birdnet_formats[[format]]$required %in% columns)) return(format)
  }
  NA_character_
}

# Comprueba los valores y explica qué filas fallan
validate_birdnet_rows <- function(df, display_name) {
  if (nrow(df) == 0) return(invisible(TRUE))
  rows <- function(bad) paste(utils::head(which(bad) + 1, 5), collapse = ", ") # +1: cabecera
  bad_time <- is.na(df$start_s) | is.na(df$end_s) | df$start_s < 0 | df$end_s <= df$start_s
  if (any(bad_time)) {
    biosonora_abort("error.birdnet_bad_times", file = display_name, rows = rows(bad_time))
  }
  bad_conf <- is.na(df$confidence) | df$confidence < 0 | df$confidence > 1
  if (any(bad_conf)) {
    biosonora_abort("error.birdnet_bad_confidence", file = display_name, rows = rows(bad_conf))
  }
  bad_name <- is.na(df$scientific_name) | df$scientific_name == ""
  if (any(bad_name)) {
    biosonora_abort("error.birdnet_missing_species", file = display_name, rows = rows(bad_name))
  }
  invisible(TRUE)
}

#' Tipo de detección: "species", "human" (privada) u "other" (ruidos)
#' @noRd
birdnet_category <- function(scientific_name) {
  ifelse(scientific_name %in% birdnet_human_classes, "human",
         ifelse(scientific_name %in% birdnet_other_classes, "other", "species"))
}

#' Nombre del audio deducido del nombre del archivo de resultados
#'
#' "pista.BirdNET.results.csv" -> "pista"
#' "pista.BirdNET.selection.table.txt" -> "pista"
#' "Blue_Jay_Sample_BirdNET_Results.csv" -> "Blue_Jay_Sample"
#' @noRd
audio_stem_from_results <- function(results_name) {
  stem <- sub("\\.(csv|txt)$", "", basename(results_name), ignore.case = TRUE)
  stem <- sub("[._ -]+BirdNET([._ -].*)?$", "", stem, ignore.case = TRUE)
  sub("\\.(wav|flac|mp3)$", "", stem, ignore.case = TRUE)
}

#' Asocia cada detección a una grabación de la base de datos
#'
#' Orden de búsqueda: ruta completa del audio (columna File / Begin Path),
#' nombre del archivo, nombre sin extensión y, si el archivo no indica el
#' audio, el nombre deducido del archivo de resultados.
#'
#' @param detections Resultado de read_birdnet_file().
#' @param recordings data.frame con recording_id, file_path, file_name y
#'   original_name.
#' @param results_name Nombre del archivo de resultados.
#' @return El mismo data.frame con la columna recording_id (NA = sin asociar).
#' @noRd
match_detections_to_recordings <- function(detections, recordings, results_name) {
  strip_ext <- function(x) tolower(sub("\\.[^.]*$", "", basename(x)))
  rec_path <- tolower(normalizePath(recordings$file_path, winslash = "/", mustWork = FALSE))
  rec_names <- tolower(recordings$file_name)
  rec_stems <- strip_ext(recordings$file_name)
  orig_stems <- strip_ext(ifelse(is.na(recordings$original_name), "", recordings$original_name))

  find <- function(ref) {
    if (is.na(ref) || !nzchar(ref)) return(NA_integer_)
    ref_norm <- tolower(gsub("\\\\", "/", ref))
    hit <- which(rec_path == ref_norm)
    if (!length(hit)) hit <- which(rec_names == tolower(basename(ref_norm)))
    if (!length(hit)) hit <- which(rec_stems == strip_ext(ref_norm))
    if (!length(hit)) hit <- which(orig_stems == strip_ext(ref_norm))
    if (length(hit)) recordings$recording_id[hit[1]] else NA_integer_
  }

  refs <- unique(detections$file_ref)
  ids <- vapply(refs, find, integer(1))
  detections$recording_id <- unname(ids[match(detections$file_ref, refs)])
  no_ref <- is.na(detections$file_ref) | detections$file_ref == ""
  if (any(no_ref)) {
    detections$recording_id[no_ref] <- find(audio_stem_from_results(results_name))
  }
  detections
}

#' Tabla de códigos de eBird -> nombre científico del BirdNET-Analyzer instalado
#'
#' No se copia al proyecto (forma parte de BirdNET): se lee del entorno de
#' Python. Devuelve NULL si BirdNET no está instalado.
#' @noRd
birdnet_code_map <- function() {
  file <- birdnet_package_file("eBird_taxonomy_codes_2024E.json")
  if (is.null(file)) return(NULL)
  codes <- jsonlite::fromJSON(file, simplifyVector = TRUE)
  values <- unlist(codes)
  is_code <- !grepl("_", names(values)) # entradas código -> "Científico_Común"
  stats::setNames(sub("_.*$", "", values[is_code]), names(values)[is_code])
}
