# Detección del tipo de grabadora y extracción de metadatos comunes.
#
# Tipos de grabadora (recorder_type):
#   - "audiomoth": metadatos GUANO o comentario de AudioMoth.
#   - "manual"   : grabadora de mano (TASCAM u otra) con bloque bext (fecha fiable).
#   - "unknown"  : no se puede determinar; se pedirá al usuario.
#
# Origen de la fecha (datetime_source), de más a menos fiable:
#   "guano", "comment", "bext", "filename", "file_mtime".

#' Detecta el tipo de grabadora a partir de la cabecera de un WAV
#'
#' @param header Resultado de read_wav_header().
#' @return Lista con recorder_type y model.
#' @noRd
detect_recorder <- function(header) {
  guano <- header$guano
  info <- header$info
  text_or_empty <- function(x) if (is.null(x)) "" else x
  make_model <- paste(text_or_empty(guano[["Make"]]), text_or_empty(guano[["Model"]]))
  artist <- text_or_empty(info$IART)
  comment <- text_or_empty(info$ICMT)

  # Los clips exportados desde "AudioMoth Filter Playground" no son grabaciones
  # originales: no tienen fecha, así que no los tratamos como AudioMoth.
  is_playground <- grepl("Filter Playground", paste(artist, comment), fixed = TRUE)

  if (!is_playground && (grepl("AudioMoth", make_model, fixed = TRUE) ||
    grepl("^AudioMoth [0-9A-Fa-f]{16}", artist) ||
    grepl("by AudioMoth", comment, fixed = TRUE))) {
    model <- if (!is.null(guano[["Model"]])) guano[["Model"]] else "AudioMoth"
    return(list(recorder_type = "audiomoth", model = model))
  }

  bext <- header$bext
  if (!is.null(bext)) {
    origin <- paste(bext$originator, bext$originator_reference, bext$description)
    model <- if (grepl("TASCAM|DR-?[0-9]", origin, ignore.case = TRUE)) {
      m <- regmatches(origin, regexpr("DR-?[0-9]+[A-Z]*", origin, ignore.case = TRUE))
      if (length(m)) paste("TASCAM", toupper(m)) else "TASCAM"
    } else {
      NA_character_
    }
    return(list(recorder_type = "manual", model = model))
  }

  list(recorder_type = "unknown", model = NA_character_)
}

#' Extrae los metadatos comunes de una grabación
#'
#' @param path Ruta al WAV.
#' @param config Resultado de read_audiomoth_config() asociado, o NULL.
#' @param local_tz Zona horaria local (para fechas sin zona, p. ej. bext).
#' @return Lista con una fila de metadatos (ver `recording_columns`).
#' @noRd
extract_recording_metadata <- function(path, config = NULL, local_tz = "Europe/Madrid") {
  header <- read_wav_header(path)
  rec <- detect_recorder(header)
  file_name <- basename(path)
  fn <- parse_audiomoth_filename(file_name)

  meta <- list(
    file_path = normalizePath(path, winslash = "/", mustWork = FALSE),
    file_name = file_name,
    file_size = file.size(path),
    recorder_type = rec$recorder_type,
    model = rec$model,
    device_id = NA_character_,
    firmware = NA_character_,
    start_utc = as.POSIXct(NA, tz = "UTC"),
    datetime_source = NA_character_,
    datetime_reliable = FALSE,
    duration_s = header$duration_s,
    sample_rate = header$sample_rate,
    channels = header$channels,
    bits = header$bits,
    gain = NA_character_,
    battery_v = NA_real_,
    temperature_c = NA_real_,
    original_name = NA_character_,
    name_modified = FALSE,
    # Posición y tamaño del audio dentro del archivo (para la huella)
    data_offset = header$data_offset,
    data_size = header$data_size
  )

  if (rec$recorder_type == "audiomoth") {
    meta <- fill_audiomoth_metadata(meta, header, fn, config)
  } else if (rec$recorder_type == "manual") {
    meta$start_utc <- parse_bext_datetime(header$bext, local_tz)
    if (!is.na(meta$start_utc)) {
      meta$datetime_source <- "bext"
      meta$datetime_reliable <- TRUE
    }
  }

  # Último recurso: nombre de archivo (AAAAMMDD_HHMMSS, en hora local) o
  # fecha de modificación del archivo (poco fiable: cambia al copiar/convertir)
  if (is.na(meta$start_utc) && !is.null(fn)) {
    tz <- if (rec$recorder_type == "audiomoth") "UTC" else local_tz
    meta$start_utc <- as.POSIXct(fn$datetime_text, format = "%Y%m%d_%H%M%S", tz = tz)
    attr(meta$start_utc, "tzone") <- "UTC"
    meta$datetime_source <- "filename"
    meta$datetime_reliable <- FALSE
  }
  if (is.na(meta$start_utc)) {
    meta$start_utc <- as.POSIXct(file.mtime(path))
    attr(meta$start_utc, "tzone") <- "UTC"
    meta$datetime_source <- "file_mtime"
    meta$datetime_reliable <- FALSE
  }
  meta
}

# Rellena los campos de AudioMoth, priorizando GUANO > comentario > nombre > CONFIG.TXT
fill_audiomoth_metadata <- function(meta, header, fn, config) {
  guano <- header$guano
  cmt <- parse_audiomoth_comment(header$info$ICMT)

  first_valid <- function(...) {
    for (x in list(...)) if (!is.null(x) && length(x) == 1 && !is.na(x) && nzchar(x)) return(x)
    NA_character_
  }

  meta$device_id <- toupper(first_valid(
    guano[["Serial"]], cmt$device_id, fn$device_id, config$device_id
  ))
  meta$firmware <- first_valid(guano[["Firmware Version"]], config$firmware)
  meta$gain <- first_valid(cmt$gain, config$gain)
  meta$battery_v <- suppressWarnings(as.numeric(first_valid(
    guano[["OAD|Battery Voltage"]], cmt$battery_v
  )))
  meta$temperature_c <- suppressWarnings(as.numeric(first_valid(
    guano[["Temperature Int"]], cmt$temperature_c
  )))

  guano_time <- parse_guano_timestamp(guano[["Timestamp"]])
  if (!is.na(guano_time)) {
    meta$start_utc <- guano_time
    meta$datetime_source <- "guano"
    meta$datetime_reliable <- TRUE
  } else if (!is.null(cmt) && !is.na(cmt$datetime_utc)) {
    meta$start_utc <- cmt$datetime_utc
    meta$datetime_source <- "comment"
    meta$datetime_reliable <- TRUE
  }

  # Nombre original guardado por AudioMoth: sirve para detectar renombrados
  meta$original_name <- first_valid(guano[["Original Filename"]])
  meta$name_modified <- (!is.null(fn) && nzchar(fn$suffix)) ||
    (!is.na(meta$original_name) && !identical(meta$original_name, meta$file_name))
  meta
}

#' Fecha y hora de inicio de un bloque bext
#'
#' La norma indica "aaaa-mm-dd" y "hh:mm:ss", pero muchos programas usan otros
#' separadores ("hh-mm-ss", "aaaa:mm:dd"): nos quedamos solo con los dígitos.
#' La fecha bext no lleva zona horaria: se interpreta como hora local.
#' @noRd
parse_bext_datetime <- function(bext, local_tz) {
  if (is.null(bext)) return(as.POSIXct(NA, tz = "UTC"))
  d <- gsub("[^0-9]", "", bext$origination_date)
  t <- gsub("[^0-9]", "", bext$origination_time)
  if (nchar(d) != 8 || nchar(t) != 6) return(as.POSIXct(NA, tz = "UTC"))
  x <- as.POSIXct(paste(d, t), format = "%Y%m%d %H%M%S", tz = local_tz)
  attr(x, "tzone") <- "UTC"
  x
}
