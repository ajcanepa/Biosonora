# Generador de archivos WAV sintéticos para los tests y para datos_ejemplo/.
# Crea un WAV con los bloques de metadatos que queramos (INFO, GUANO, bext),
# imitando la estructura real de AudioMoth y de grabadoras con BWF.

# Entero sin signo en little-endian
le_uint <- function(x, bytes) {
  out <- raw(bytes)
  for (i in seq_len(bytes)) {
    out[i] <- as.raw(x %% 256)
    x <- x %/% 256
  }
  out
}

wav_chunk <- function(id, body) {
  out <- c(charToRaw(id), le_uint(length(body), 4), body)
  if (length(body) %% 2 == 1) out <- c(out, as.raw(0)) # byte de relleno
  out
}

text_raw <- function(x, size = NULL) {
  b <- charToRaw(enc2utf8(x))
  if (!is.null(size)) b <- c(b, raw(max(0, size - length(b))))[seq_len(size)]
  b
}

#' Escribe un WAV sintético
#' @param tone_hz Frecuencia del tono (una señal simple, sin voces).
write_test_wav <- function(path, sample_rate = 8000, channels = 1, bits = 16,
                           duration_s = 0.5, tone_hz = 1000,
                           info = NULL, guano = NULL, bext = NULL) {
  n <- round(sample_rate * duration_s)
  t <- (seq_len(n) - 1) / sample_rate
  signal <- 0.5 * sin(2 * pi * tone_hz * t)
  bytes_per_sample <- bits / 8
  full <- 2^(bits - 1) - 1
  values <- round(signal * full)
  values[values < 0] <- values[values < 0] + 2^bits # complemento a dos
  # Cada columna = bytes de una muestra; se repite cada columna una vez por
  # canal para intercalarlos (mismo tono en todos los canales)
  frames <- matrix(unlist(lapply(values, le_uint, bytes = bytes_per_sample)),
                   nrow = bytes_per_sample)
  data <- as.vector(frames[, rep(seq_len(n), each = channels), drop = FALSE])

  fmt <- c(le_uint(1, 2), le_uint(channels, 2), le_uint(sample_rate, 4),
           le_uint(sample_rate * channels * bytes_per_sample, 4),
           le_uint(channels * bytes_per_sample, 2), le_uint(bits, 2))
  chunks <- wav_chunk("fmt ", fmt)

  if (!is.null(bext)) {
    body <- c(
      text_raw(bext$description %||% "", 256), text_raw(bext$originator %||% "", 32),
      text_raw(bext$originator_reference %||% "", 32), text_raw(bext$date %||% "", 10),
      text_raw(bext$time %||% "", 8), le_uint(0, 8), le_uint(1, 2), raw(64), raw(10), raw(180)
    )
    chunks <- c(chunks, wav_chunk("bext", body))
  }
  if (!is.null(info)) {
    sub <- unlist(lapply(names(info), function(k) {
      wav_chunk(k, c(text_raw(info[[k]]), as.raw(0)))
    }))
    chunks <- c(chunks, wav_chunk("LIST", c(charToRaw("INFO"), sub)))
  }
  chunks <- c(chunks, wav_chunk("data", data))
  if (!is.null(guano)) {
    txt <- paste0("GUANO|Version:1.0\n",
                  paste0(names(guano), ":", unlist(guano), collapse = "\n"), "\n")
    chunks <- c(chunks, wav_chunk("guan", text_raw(txt)))
  }
  riff <- c(charToRaw("RIFF"), le_uint(4 + length(chunks), 4), charToRaw("WAVE"), chunks)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeBin(riff, path)
  invisible(path)
}

# Comentario y GUANO como los escribe AudioMoth (firmware 1.12.1)
audiomoth_info <- function(device, time_hms, date_dmy, battery = "4.8", temp = "21.5") {
  list(
    ICMT = sprintf("Recorded at %s %s (UTC) by AudioMoth %s at medium gain while battery was %sV and temperature was %sC.",
                   time_hms, date_dmy, device, battery, temp),
    IART = paste("AudioMoth", device)
  )
}

audiomoth_guano <- function(device, timestamp, original, battery = "4.8", temp = "21.5") {
  list(
    "Make" = "Open Acoustic Devices", "Model" = "AudioMoth", "Serial" = device,
    "Firmware Version" = "AudioMoth-Firmware-Basic (1.12.1)",
    "Timestamp" = timestamp, "Original Filename" = original,
    "OAD|Recording Settings" = "8000 GAIN 2", "OAD|Battery Voltage" = battery,
    "Temperature Int" = temp
  )
}

# Ruta a los datos de ejemplo del proyecto (funciona en tests y en el script)
example_dir <- function(...) {
  root <- tryCatch(testthat::test_path("..", "..", "datos_ejemplo"), error = function(e) "datos_ejemplo")
  normalizePath(file.path(root, ...), winslash = "/", mustWork = FALSE)
}
