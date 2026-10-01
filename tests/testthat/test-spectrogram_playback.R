# Espectrograma, forma de onda y audio de reproducción

test_that("el espectrograma muestra la energía en la frecuencia del tono", {
  f <- tempfile(fileext = ".wav")
  write_test_wav(f, sample_rate = 8000, duration_s = 1, tone_hz = 1000)
  spec <- compute_spectrogram(read_audio_mono(f), max_columns = 50)
  expect_equal(spec$freq_max, 4000)
  expect_equal(spec$duration_s, 1)
  expect_lte(ncol(spec$db), 50)
  # Fila con más energía (de media) = 1000 Hz
  peak_row <- which.max(rowMeans(spec$db))
  peak_hz <- (peak_row - 0.5) * spec$freq_max / nrow(spec$db)
  expect_lt(abs(peak_hz - 1000), 2 * spec$freq_max / nrow(spec$db))
  expect_length(spec$peaks, 2 * 4000)
})

test_that("pool_max toma el máximo de cada grupo de filas", {
  m <- matrix(c(1, 5, 2, 3, 9), ncol = 1)
  expect_equal(as.vector(pool_max(m, 2)), c(5, 3, 9))
  expect_identical(pool_max(m, 1), m)
})

test_that("el PNG tiene una columna por ventana y respeta el rango de frecuencias", {
  f <- tempfile(fileext = ".wav")
  write_test_wav(f, sample_rate = 8000, duration_s = 1, tone_hz = 1000)
  spec <- compute_spectrogram(read_audio_mono(f), max_columns = 40)
  png_file <- tempfile(fileext = ".png")
  render_spectrogram_png(spec, png_file, fmin_hz = 0, fmax_hz = 2000, palette = "grey")
  bytes <- readBin(png_file, "raw", 24)
  width <- sum(as.numeric(bytes[17:20]) * 256^(3:0))
  height <- sum(as.numeric(bytes[21:24]) * 256^(3:0))
  expect_equal(width, ncol(spec$db))
  expect_equal(height, sum((seq_len(nrow(spec$db)) - 0.5) * 4000 / nrow(spec$db) <= 2000))
  expect_error(spectrogram_palette("arcoiris"), class = "biosonora_error")
})

test_that("reproducción lenta = expansión temporal (misma muestra, menor frecuencia)", {
  f <- example_dir("tascam", "DR0000_0001.wav")
  out <- tempfile(fileext = ".wav")
  p <- make_playback_audio(f, 0.1, out)
  expect_equal(p$header_rate, 4800L)
  expect_equal(p$play_duration_s, 5) # 0,5 s x 10
  h <- read_wav_header(out)
  expect_equal(h$bits, 16L)
  expect_equal(h$channels, 2L)
})

test_that("ultrasonidos a 1x: se reduce la frecuencia de muestreo con filtro", {
  f <- tempfile(fileext = ".wav")
  write_test_wav(f, sample_rate = 250000, duration_s = 0.2, tone_hz = 40000)
  p <- make_playback_audio(f, 1, tempfile(fileext = ".wav"))
  expect_lte(p$header_rate, playback_max_rate)
  expect_equal(p$play_duration_s, 0.2, tolerance = 0.01)
  # A 0,1x cabe sin reducir: 25 kHz y el tono de 40 kHz se oye a 4 kHz
  q <- make_playback_audio(f, 0.1, tempfile(fileext = ".wav"))
  expect_equal(q$header_rate, 25000L)
})

test_that("muestreo muy bajo a 0,1x: se aumentan las muestras para el navegador", {
  f <- tempfile(fileext = ".wav")
  write_test_wav(f, sample_rate = 8000, duration_s = 0.2)
  p <- make_playback_audio(f, 0.1, tempfile(fileext = ".wav"))
  expect_gte(p$header_rate, playback_min_rate)
  expect_equal(p$play_duration_s, 2, tolerance = 0.01)
  expect_error(make_playback_audio(f, 3, tempfile()), class = "biosonora_error")
})

test_that("la caché borra lo más antiguo al superar el tamaño máximo", {
  dir <- tempfile()
  dir.create(dir)
  for (i in 1:5) writeBin(raw(300 * 1024), file.path(dir, paste0(i, ".png")))
  prune_cache(dir, max_mb = 1)
  total <- sum(file.size(list.files(dir, full.names = TRUE))) / 1024^2
  expect_lte(total, 1)
})
