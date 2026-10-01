# Espectrograma y forma de onda, calculados bajo demanda.
#
# Un espectrograma muestra la energía del sonido (color) en cada frecuencia
# (eje vertical) a lo largo del tiempo (eje horizontal). Se calcula dividiendo
# el audio en ventanas cortas y aplicando a cada una la transformada de
# Fourier (FFT), que ya incluye R base (stats::mvfft).
#
# Para no perder sonidos breves (p. ej. llamadas de murciélago de pocos ms)
# se calculan todas las ventanas y luego se reducen al ancho de la imagen
# quedándonos con el máximo de cada grupo.

# Versión del algoritmo: si cambia, se invalidan las imágenes en caché
spectrogram_version <- 2L

#' Lee el audio de un WAV como vector numérico entre -1 y 1 (canales mezclados)
#' @noRd
read_audio_mono <- function(path) {
  w <- tryCatch(tuneR::readWave(path), error = function(e) {
    biosonora_abort("error.wav_unreadable", file = basename(path))
  })
  x <- if (w@stereo) (as.numeric(w@left) + as.numeric(w@right)) / 2 else as.numeric(w@left)
  full_scale <- if (w@pcm) 2^(w@bit - 1) else 1
  list(samples = x / full_scale, sample_rate = w@samp.rate)
}

#' Tamaño de ventana FFT adecuado a la frecuencia de muestreo (~10 ms)
#' @noRd
choose_fft_size <- function(sample_rate) {
  as.integer(min(4096, max(256, 2^round(log2(sample_rate * 0.0107)))))
}

#' Calcula el espectrograma (en dB) y los picos de la forma de onda
#'
#' @param audio Resultado de read_audio_mono().
#' @param max_columns Ancho máximo (ventanas de tiempo) del resultado.
#' @param max_rows Alto máximo (bandas de frecuencia) del resultado.
#' @param n_peaks Número de puntos de la forma de onda.
#' @return Lista con db (matriz frecuencia x tiempo, fila 1 = 0 Hz),
#'   freq_max (Hz), duration_s y peaks (vector min/max intercalados).
#' @noRd
compute_spectrogram <- function(audio, max_columns = 4000, max_rows = 512, n_peaks = 4000) {
  x <- audio$samples
  sr <- audio$sample_rate
  n_fft <- choose_fft_size(sr)
  hop <- n_fft %/% 2
  if (length(x) < n_fft) x <- c(x, rep(0, n_fft - length(x)))

  n_bins <- n_fft %/% 2
  window <- 0.5 - 0.5 * cos(2 * pi * (0:(n_fft - 1)) / (n_fft - 1)) # Hann

  # Factores enteros de agrupación: cada `col_factor` ventanas seguidas forman
  # una columna y cada `row_factor` bandas seguidas forman una fila
  n_frames <- (length(x) - n_fft) %/% hop + 1
  col_factor <- max(1L, ceiling(n_frames / max_columns))
  row_factor <- max(1L, ceiling(n_bins / max_rows))
  n_cols <- ceiling(n_frames / col_factor)
  starts <- 1 + (seq_len(n_cols * col_factor) - 1) * hop
  # Las últimas ventanas pueden salirse del audio: se rellena con ceros
  x <- c(x, rep(0, max(0, max(starts) + n_fft - 1 - length(x))))

  offsets <- 0:(n_fft - 1)
  block_cols <- max(1L, 4096L %/% col_factor) # columnas por bloque (memoria)
  pieces <- list()
  for (c0 in seq(1, n_cols, by = block_cols)) {
    cols <- c0:min(c0 + block_cols - 1, n_cols)
    idx <- ((cols[1] - 1) * col_factor + 1):(max(cols) * col_factor)
    frames <- matrix(x[outer(offsets, starts[idx], "+")], nrow = n_fft) * window
    spec <- Mod(stats::mvfft(frames)[seq_len(n_bins), , drop = FALSE])^2
    spec <- pool_max(spec, row_factor)            # agrupar filas
    spec <- t(pool_max(t(spec), col_factor))      # agrupar columnas
    pieces[[length(pieces) + 1]] <- spec
  }
  power <- do.call(cbind, pieces)

  list(
    db = 10 * log10(power + 1e-20),
    freq_max = sr / 2,
    duration_s = length(audio$samples) / sr,
    peaks = waveform_peaks(audio$samples, n_peaks)
  )
}

#' Máximo de cada grupo de `factor` filas consecutivas
#'
#' Vectorizado: compara a la vez la 1ª fila de todos los grupos, luego la 2ª...
#' Si el número de filas no es múltiplo del factor, el último grupo es menor.
#' @noRd
pool_max <- function(m, factor) {
  if (factor <= 1) return(m)
  n <- nrow(m)
  out <- m[seq(1, n, by = factor), , drop = FALSE]
  for (j in seq_len(factor - 1)) {
    rows <- seq(1 + j, n, by = factor)
    k <- seq_along(rows)
    out[k, ] <- pmax(out[k, , drop = FALSE], m[rows, , drop = FALSE])
  }
  out
}

#' Picos de la forma de onda: máximo y mínimo de cada tramo, intercalados
#' @noRd
waveform_peaks <- function(x, n) {
  # Cada columna de la matriz es un tramo de `k` muestras (se rellena el final
  # repitiendo la última muestra para completar la matriz)
  k <- max(1L, ceiling(length(x) / n))
  n_cols <- ceiling(length(x) / k)
  m <- matrix(c(x, rep(x[length(x)], n_cols * k - length(x))), nrow = k)
  mx <- apply(m, 2, max)
  mn <- apply(m, 2, min)
  scale <- max(abs(c(mx, mn)), 1e-9)
  round(as.vector(rbind(mx, mn)) / scale, 4)
}

#' Paletas de color del espectrograma
#'
#' viridis, magma y cividis son aptas para daltonismo (cividis está pensada
#' para que se vea casi igual con y sin daltonismo); "grey" es la alternativa
#' en escala de grises (sonido fuerte = oscuro).
#' @noRd
spectrogram_palette <- function(name = "viridis", n = 256) {
  switch(name,
    viridis = grDevices::hcl.colors(n, "viridis"),
    magma = grDevices::hcl.colors(n, "Inferno"),
    cividis = grDevices::hcl.colors(n, "Cividis"),
    grey = rev(grDevices::gray.colors(n, start = 0, end = 1)),
    biosonora_abort("error.invalid_palette")
  )
}

#' Dibuja el espectrograma como imagen PNG
#'
#' @param spec Resultado de compute_spectrogram().
#' @param file Ruta del PNG a crear.
#' @param fmin_hz,fmax_hz Rango de frecuencias a mostrar.
#' @param dyn_range_db Rango dinámico en dB (contraste): lo que esté más de
#'   este valor por debajo del máximo se pinta con el color más bajo.
#' @param palette Nombre de la paleta.
#' @noRd
render_spectrogram_png <- function(spec, file, fmin_hz = 0, fmax_hz = spec$freq_max,
                                   dyn_range_db = 80, palette = "viridis") {
  db <- spec$db
  n_rows <- nrow(db)
  freqs <- (seq_len(n_rows) - 0.5) * spec$freq_max / n_rows
  keep <- which(freqs >= fmin_hz & freqs <= fmax_hz)
  if (length(keep) < 2) keep <- seq_len(n_rows)
  db <- db[keep, , drop = FALSE]

  top <- max(db)
  v <- (db - (top - dyn_range_db)) / dyn_range_db
  v[v < 0] <- 0
  v[v > 1] <- 1
  colors <- spectrogram_palette(palette)
  idx <- 1 + round(v * (length(colors) - 1))
  img <- matrix(colors[idx], nrow = nrow(db))
  img <- img[nrow(img):1, , drop = FALSE] # frecuencias altas arriba

  grDevices::png(file, width = ncol(img), height = nrow(img))
  on.exit(grDevices::dev.off())
  # Sin márgenes: la imagen debe ocupar exactamente todo el PNG
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::rasterImage(grDevices::as.raster(img), 0, 0, 1, 1, interpolate = FALSE)
  invisible(file)
}

#' Clave única de caché para un archivo y unos parámetros
#' @noRd
cache_key <- function(path, ...) {
  info <- file.info(path)
  rlang::hash(list(normalizePath(path, winslash = "/"), info$size,
                   as.numeric(info$mtime), spectrogram_version, ...))
}

#' Borra los archivos más antiguos de la caché si supera el tamaño máximo
#' @noRd
prune_cache <- function(dir, max_mb = 500) {
  files <- list.files(dir, full.names = TRUE)
  if (length(files) == 0) return(invisible(0))
  info <- file.info(files)
  total <- sum(info$size, na.rm = TRUE) / 1024^2
  if (total <= max_mb) return(invisible(0))
  ord <- order(info$atime)
  excess <- cumsum(info$size[ord]) / 1024^2 <= (total - max_mb * 0.8)
  unlink(files[ord][excess])
  invisible(sum(excess))
}
