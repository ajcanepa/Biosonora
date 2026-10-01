# Preparación del audio para reproducirlo en el navegador.
#
# Velocidad lenta = "expansión temporal": se escribe el mismo audio con una
# frecuencia de muestreo menor en la cabecera. Así, a 0,1x todo dura 10 veces
# más y suena 10 veces más grave (un ultrasonido de 40 kHz se oye a 4 kHz).
# Funciona igual en todos los navegadores, sin depender de su control de
# velocidad.
#
# Los navegadores reproducen bien entre ~3 kHz y 96 kHz de frecuencia de
# muestreo. Si hace falta, se reduce (con un filtro paso bajo para evitar
# distorsión) o se aumenta el número de muestras.

playback_speeds <- c(1, 0.5, 0.25, 0.1)
playback_max_rate <- 96000
playback_min_rate <- 3000

#' Crea el WAV de reproducción para una velocidad dada
#'
#' @param path WAV original.
#' @param speed Velocidad (1, 0.5, 0.25 o 0.1).
#' @param out_file Ruta del WAV de salida (16 bits).
#' @return Lista con out_file, header_rate y play_duration_s.
#' @noRd
make_playback_audio <- function(path, speed, out_file) {
  if (!speed %in% playback_speeds) biosonora_abort("error.invalid_speed")
  w <- tryCatch(tuneR::readWave(path), error = function(e) {
    biosonora_abort("error.wav_unreadable", file = basename(path))
  })
  full_scale <- if (w@pcm) 2^(w@bit - 1) else 1
  channels <- list(as.numeric(w@left) / full_scale)
  if (w@stereo) channels[[2]] <- as.numeric(w@right) / full_scale
  sr <- w@samp.rate

  target <- sr * speed
  if (target > playback_max_rate) {
    k <- ceiling(target / playback_max_rate)
    channels <- lapply(channels, decimate, factor = k)
    sr <- sr / k
  } else if (target < playback_min_rate) {
    u <- ceiling(playback_min_rate / target)
    channels <- lapply(channels, upsample, factor = u)
    sr <- sr * u
  }
  header_rate <- as.integer(round(sr * speed))

  to_int16 <- function(x) as.integer(round(pmax(-1, pmin(1, x)) * 32767))
  wave <- if (length(channels) == 2) {
    tuneR::Wave(left = to_int16(channels[[1]]), right = to_int16(channels[[2]]),
                samp.rate = header_rate, bit = 16)
  } else {
    tuneR::Wave(left = to_int16(channels[[1]]), samp.rate = header_rate, bit = 16)
  }
  tuneR::writeWave(wave, out_file)
  list(
    out_file = out_file,
    header_rate = header_rate,
    play_duration_s = length(channels[[1]]) / header_rate
  )
}

#' Reduce la frecuencia de muestreo por un factor entero
#'
#' Primero se aplica un filtro paso bajo (FIR con ventana de Hamming) para que
#' las frecuencias que no caben no se "doblen" y aparezcan como ruido audible.
#' @noRd
decimate <- function(x, factor) {
  if (factor <= 1) return(x)
  taps <- 63
  n <- seq(-(taps - 1) / 2, (taps - 1) / 2)
  fc <- 0.45 / factor
  h <- ifelse(n == 0, 2 * fc, sin(2 * pi * fc * n) / (pi * n))
  h <- h * (0.54 - 0.46 * cos(2 * pi * (0:(taps - 1)) / (taps - 1)))
  h <- h / sum(h)
  y <- stats::filter(x, h, sides = 2)
  y[is.na(y)] <- 0
  as.numeric(y)[seq(1, length(y), by = factor)]
}

#' Aumenta el número de muestras por un factor entero (interpolación lineal)
#' @noRd
upsample <- function(x, factor) {
  if (factor <= 1) return(x)
  n <- length(x)
  stats::approx(seq_len(n), x, xout = seq(1, n, length.out = n * factor))$y
}
