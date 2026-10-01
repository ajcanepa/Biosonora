# Lectura de cabeceras y metadatos WAV

test_that("lee formato y metadatos de un WAV AudioMoth", {
  h <- read_wav_header(example_dir("audiomoth", "20260810", "24BBD608675B68F7_20260810_160000.WAV"))
  expect_equal(h$sample_rate, 8000L)
  expect_equal(h$channels, 1L)
  expect_equal(h$bits, 16L)
  expect_equal(h$duration_s, 0.5)
  expect_equal(h$chunks, c("fmt ", "LIST", "data", "guan"))
  expect_match(h$info$ICMT, "^Recorded at 16:00:00 10/08/2026 \\(UTC\\)")
  expect_equal(h$info$IART, "AudioMoth 24BBD608675B68F7")
  expect_equal(h$guano$Serial, "24BBD608675B68F7")
  expect_equal(h$guano$Timestamp, "2026-08-10T16:00:00Z") # el valor contiene ":"
  expect_equal(h$guano[["OAD|Battery Voltage"]], "4.8")
})

test_that("lee el bloque bext de una grabadora manual (estéreo, 24 bits)", {
  h <- read_wav_header(example_dir("tascam", "DR0000_0001.wav"))
  expect_equal(h$channels, 2L)
  expect_equal(h$bits, 24L)
  expect_equal(h$sample_rate, 48000L)
  expect_equal(h$duration_s, 0.5)
  expect_equal(h$bext$originator, "TASCAM DR-05X")
  expect_equal(h$bext$origination_date, "2025-04-10")
  expect_equal(h$bext$origination_time, "17:53:05")
})

test_that("un archivo que no es WAV da un error comprensible", {
  err <- tryCatch(read_wav_header(example_dir("otros", "no_es_audio.wav")), error = identity)
  expect_s3_class(err, "biosonora_error")
  expect_equal(err$key, "error.wav_not_riff")
  expect_equal(err$data$file, "no_es_audio.wav")
})

test_that("un archivo inexistente o vacío da un error comprensible", {
  err <- tryCatch(read_wav_header(tempfile(fileext = ".wav")), error = identity)
  expect_equal(err$key, "error.file_not_found")
  empty <- tempfile(fileext = ".wav")
  writeBin(raw(10), empty)
  err <- tryCatch(read_wav_header(empty), error = identity)
  expect_equal(err$key, "error.wav_too_small")
})

test_that("una grabación cortada no declara más audio del que hay", {
  src <- example_dir("audiomoth", "20260811", "24BBD608675B68F7_20260811_160000.WAV")
  bytes <- readBin(src, "raw", file.size(src))
  cut <- tempfile(fileext = ".wav")
  writeBin(bytes[1:(length(bytes) - 2000)], cut)
  h <- read_wav_header(cut)
  expect_lt(h$duration_s, 0.5)
  expect_equal(h$data_offset + h$data_size, file.size(cut))
})

test_that("parse_guano corta solo por los primeros dos puntos", {
  g <- parse_guano(charToRaw("GUANO|Version:1.0\nTimestamp:2026-08-10T16:00:00+02:00\nNota:a:b"))
  expect_equal(g$Timestamp, "2026-08-10T16:00:00+02:00")
  expect_equal(g$Nota, "a:b")
})
