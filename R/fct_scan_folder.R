# Recorrido de una carpeta de grabaciones.
#
# Para cada WAV: lee sus metadatos, lo asocia a su CONFIG.TXT (buscando en su
# carpeta y en las carpetas superiores, hasta la carpeta elegida) y calcula
# una "huella" para detectar copias duplicadas del mismo audio.

# Bytes de audio que se usan para la huella (inicio y final de los datos)
fingerprint_bytes <- 65536

#' Analiza una carpeta y devuelve los metadatos de todas sus grabaciones
#'
#' @param folder Carpeta a analizar (se incluyen subcarpetas).
#' @param local_tz Zona horaria local.
#' @param progress Función opcional `function(i, n)` para informar del avance.
#' @return data.frame con una fila por archivo (ver `empty_recordings()`).
#' @noRd
scan_folder <- function(folder, local_tz = "Europe/Madrid", progress = NULL) {
  if (!is.character(folder) || length(folder) != 1 || !nzchar(folder) || !dir.exists(folder)) {
    biosonora_abort("error.folder_not_found")
  }
  folder <- normalizePath(folder, winslash = "/")
  files <- list.files(folder, pattern = "\\.wav$", ignore.case = TRUE,
                      recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    biosonora_abort("error.no_wav_found", folder = basename(folder))
  }
  files <- sort(files)

  configs <- new.env()
  rows <- vector("list", length(files))
  for (i in seq_along(files)) {
    if (!is.null(progress)) progress(i, length(files))
    rows[[i]] <- scan_one_file(files[i], folder, configs, local_tz)
  }
  recordings <- do.call(rbind, rows)
  mark_duplicates(recordings)
}

# Lee un archivo; si falla, devuelve una fila con la clave del error
scan_one_file <- function(path, root, configs, local_tz) {
  config_path <- find_config_for(path, root)
  config <- NULL
  if (!is.na(config_path)) {
    if (is.null(configs[[config_path]])) {
      configs[[config_path]] <- tryCatch(read_audiomoth_config(config_path), error = function(e) list())
    }
    config <- configs[[config_path]]
  }

  row <- tryCatch({
    meta <- extract_recording_metadata(path, config, local_tz)
    if (meta$recorder_type != "audiomoth") config_path <- NA_character_
    meta$config_path <- config_path
    meta$fingerprint <- file_fingerprint(path, meta$data_offset, meta$data_size)
    meta$error_key <- NA_character_
    meta
  }, error = function(e) {
    key <- if (inherits(e, "biosonora_error")) e$key else "error.wav_unreadable"
    list(
      file_path = normalizePath(path, winslash = "/", mustWork = FALSE),
      file_name = basename(path), file_size = file.size(path),
      recorder_type = "unknown", error_key = key
    )
  })
  as_recording_row(row)
}

#' Busca el CONFIG.TXT de un archivo subiendo por las carpetas hasta `root`
#' @noRd
find_config_for <- function(path, root) {
  dir <- dirname(normalizePath(path, winslash = "/"))
  repeat {
    hit <- list.files(dir, pattern = "^config\\.txt$", ignore.case = TRUE, full.names = TRUE)
    if (length(hit)) return(normalizePath(hit[1], winslash = "/"))
    if (identical(dir, root) || !startsWith(dir, root) || dirname(dir) == dir) break
    dir <- dirname(dir)
  }
  NA_character_
}

#' Huella del audio: tamaño + primeros y últimos bytes de datos
#'
#' Leer 39 GB enteros para comparar sería muy lento; dos grabaciones distintas
#' casi nunca coinciden en tamaño y en 128 KB de audio.
#' @noRd
file_fingerprint <- function(path, data_offset, data_size) {
  con <- file(path, "rb")
  on.exit(close(con))
  n <- min(fingerprint_bytes, data_size)
  seek(con, data_offset)
  head_bytes <- readBin(con, "raw", n = n)
  seek(con, data_offset + data_size - n)
  tail_bytes <- readBin(con, "raw", n = n)
  rlang::hash(list(data_size, head_bytes, tail_bytes))
}

#' Marca las copias duplicadas
#'
#' En cada grupo con la misma huella se conserva como original el archivo con
#' nombre no modificado (o el de nombre más corto); el resto apunta a él.
#' @noRd
mark_duplicates <- function(recordings) {
  recordings$duplicate_of <- NA_character_
  ok <- !is.na(recordings$fingerprint)
  groups <- split(which(ok), recordings$fingerprint[ok])
  for (idx in groups[lengths(groups) > 1]) {
    ord <- idx[order(recordings$name_modified[idx], nchar(recordings$file_name[idx]))]
    recordings$duplicate_of[ord[-1]] <- recordings$file_path[ord[1]]
  }
  recordings
}

#' Estructura vacía de la tabla de grabaciones (columnas y tipos)
#' @noRd
empty_recordings <- function() {
  data.frame(
    file_path = character(), file_name = character(), file_size = numeric(),
    recorder_type = character(), model = character(), device_id = character(),
    firmware = character(),
    start_utc = as.POSIXct(character(), tz = "UTC"),
    datetime_source = character(), datetime_reliable = logical(),
    duration_s = numeric(), sample_rate = integer(), channels = integer(),
    bits = integer(), gain = character(), battery_v = numeric(),
    temperature_c = numeric(), original_name = character(),
    name_modified = logical(), config_path = character(),
    fingerprint = character(), error_key = character(),
    stringsAsFactors = FALSE
  )
}

# Convierte una lista de metadatos en una fila con todas las columnas
as_recording_row <- function(x) {
  row <- empty_recordings()[NA_integer_, , drop = FALSE]
  rownames(row) <- NULL
  for (col in intersect(names(x), names(row))) {
    value <- x[[col]]
    if (length(value) == 1 && !is.null(value)) row[[col]] <- value
  }
  row$name_modified[is.na(row$name_modified)] <- FALSE
  row$datetime_reliable[is.na(row$datetime_reliable)] <- FALSE
  row
}
