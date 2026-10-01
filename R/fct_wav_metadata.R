# Lectura de la cabecera y los metadatos de archivos WAV sin cargar el audio.
#
# Un WAV es un contenedor "RIFF" formado por bloques (chunks). Cada bloque
# tiene un identificador de 4 letras, un tamaño y su contenido:
#   - "fmt " : formato (canales, frecuencia de muestreo, bits)
#   - "data" : las muestras de audio (lo único que NO leemos aquí)
#   - "LIST" : metadatos INFO (AudioMoth guarda aquí su comentario ICMT)
#   - "guan" : metadatos GUANO (estándar bioacústico, usado por AudioMoth)
#   - "bext" : Broadcast WAV (grabadoras como TASCAM: fecha y hora de inicio)

# Tamaño máximo que leemos de un bloque de metadatos (evita leer basura enorme)
max_metadata_chunk <- 1024 * 1024

#' Lee la cabecera y los metadatos de un archivo WAV
#'
#' @param path Ruta al archivo WAV.
#' @return Lista con format, channels, sample_rate, bits, data_offset,
#'   data_size, n_samples, duration_s, info, guano, bext y chunks.
#' @noRd
read_wav_header <- function(path) {
  file_size <- file.size(path)
  if (is.na(file_size)) {
    biosonora_abort("error.file_not_found", file = basename(path))
  }
  if (file_size < 44) {
    biosonora_abort("error.wav_too_small", file = basename(path))
  }

  con <- file(path, "rb")
  on.exit(close(con))

  riff <- read_fourcc(con)
  readBin(con, "integer", size = 4, endian = "little") # tamaño RIFF (no fiable)
  wave <- read_fourcc(con)
  if (!identical(riff, "RIFF") || !identical(wave, "WAVE")) {
    biosonora_abort("error.wav_not_riff", file = basename(path))
  }

  out <- list(
    format = NA_integer_, channels = NA_integer_, sample_rate = NA_integer_,
    bits = NA_integer_, data_offset = NA_real_, data_size = NA_real_,
    info = list(), guano = list(), bext = NULL, chunks = character()
  )

  pos <- 12
  while (pos + 8 <= file_size) {
    seek(con, pos)
    id <- read_fourcc(con)
    size <- read_uint32(con)
    body_start <- pos + 8
    out$chunks <- c(out$chunks, id)

    if (identical(id, "data")) {
      # Grabaciones interrumpidas pueden declarar un tamaño mayor que el archivo
      size <- min(size, file_size - body_start)
      out$data_offset <- body_start
      out$data_size <- size
    } else if (size <= max_metadata_chunk) {
      body <- readBin(con, "raw", n = size)
      switch(id,
        "fmt " = out <- parse_fmt(out, body),
        "LIST" = out$info <- c(out$info, parse_list_info(body)),
        "guan" = out$guano <- parse_guano(body),
        "bext" = out$bext <- parse_bext(body),
        NULL
      )
    }
    # Los bloques de tamaño impar llevan un byte de relleno
    pos <- body_start + size + (size %% 2)
  }

  if (is.na(out$sample_rate) || is.na(out$data_offset)) {
    biosonora_abort("error.wav_incomplete", file = basename(path))
  }

  bytes_per_frame <- out$channels * ceiling(out$bits / 8)
  out$n_samples <- floor(out$data_size / bytes_per_frame)
  out$duration_s <- out$n_samples / out$sample_rate
  out
}

# ---- Funciones auxiliares de bajo nivel ----

# Identificador de bloque (4 caracteres). No se recortan espacios: "fmt " lleva uno.
read_fourcc <- function(con) {
  bytes <- readBin(con, "raw", n = 4)
  bytes[bytes == as.raw(0)] <- as.raw(32)
  if (length(bytes) < 4) return("")
  rawToChar(bytes)
}

read_uint32 <- function(con) {
  b <- as.numeric(readBin(con, "raw", n = 4))
  if (length(b) < 4) return(0)
  sum(b * 256^(0:3))
}

raw_uint <- function(bytes) {
  sum(as.numeric(bytes) * 256^(seq_along(bytes) - 1))
}

#' Convierte bytes en texto, quitando los ceros finales y caracteres inválidos
#' @noRd
raw_to_text <- function(bytes) {
  bytes <- bytes[bytes != as.raw(0)]
  if (length(bytes) == 0) return("")
  txt <- rawToChar(bytes)
  Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) {
    txt <- iconv(txt, from = "latin1", to = "UTF-8", sub = "")
  }
  trimws(txt)
}

# Bloque "fmt ": formato de audio
parse_fmt <- function(out, body) {
  if (length(body) < 16) return(out)
  format <- raw_uint(body[1:2])
  # WAVE_FORMAT_EXTENSIBLE (0xFFFE): el formato real está en el subformato
  if (format == 65534 && length(body) >= 26) {
    format <- raw_uint(body[25:26])
  }
  out$format <- as.integer(format)
  out$channels <- as.integer(raw_uint(body[3:4]))
  out$sample_rate <- as.integer(raw_uint(body[5:8]))
  out$bits <- as.integer(raw_uint(body[15:16]))
  out
}

#' Bloque LIST de tipo INFO: pares clave (4 letras) / texto
#' @noRd
parse_list_info <- function(body) {
  if (length(body) < 4 || !identical(rawToChar(body[1:4]), "INFO")) {
    return(list())
  }
  res <- list()
  i <- 5
  while (i + 7 <= length(body)) {
    key <- raw_to_text(body[i:(i + 3)])
    size <- raw_uint(body[(i + 4):(i + 7)])
    end <- min(i + 7 + size, length(body))
    value <- if (size > 0) raw_to_text(body[(i + 8):end]) else ""
    res[[key]] <- value
    i <- i + 8 + size + (size %% 2)
  }
  res
}

#' Bloque GUANO: texto con líneas "Clave:Valor"
#'
#' Solo se corta por los primeros dos puntos: los valores pueden contener ":"
#' (por ejemplo, Timestamp:2026-08-10T16:00:00Z).
#' @noRd
parse_guano <- function(body) {
  txt <- raw_to_text(body)
  lines <- strsplit(txt, "\r?\n")[[1]]
  lines <- lines[grepl(":", lines, fixed = TRUE)]
  if (length(lines) == 0) return(list())
  keys <- trimws(sub(":.*$", "", lines))
  values <- trimws(sub("^[^:]*:", "", lines))
  stats::setNames(as.list(values), keys)
}

#' Bloque bext (Broadcast WAV, norma EBU Tech 3285)
#' @noRd
parse_bext <- function(body) {
  if (length(body) < 346) return(NULL)
  list(
    description = raw_to_text(body[1:256]),
    originator = raw_to_text(body[257:288]),
    originator_reference = raw_to_text(body[289:320]),
    origination_date = raw_to_text(body[321:330]),
    origination_time = raw_to_text(body[331:338]),
    # Muestras transcurridas desde medianoche (64 bits: parte baja + alta)
    time_reference = raw_uint(body[339:342]) + raw_uint(body[343:346]) * 2^32,
    coding_history = if (length(body) > 602) raw_to_text(body[603:length(body)]) else ""
  )
}
