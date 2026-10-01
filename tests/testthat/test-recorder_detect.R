# Detección de grabadora y metadatos comunes

local_time <- function(x, tz = "Europe/Madrid") format(x, "%Y-%m-%d %H:%M:%S", tz = tz)

test_that("AudioMoth: fecha desde GUANO, convertida de UTC a hora de Madrid", {
  m <- extract_recording_metadata(
    example_dir("audiomoth", "20260810", "24BBD608675B68F7_20260810_160000.WAV"))
  expect_equal(m$recorder_type, "audiomoth")
  expect_equal(m$model, "AudioMoth")
  expect_equal(m$device_id, "24BBD608675B68F7")
  expect_equal(m$datetime_source, "guano")
  expect_true(m$datetime_reliable)
  expect_equal(local_time(m$start_utc, "UTC"), "2026-08-10 16:00:00")
  expect_equal(local_time(m$start_utc), "2026-08-10 18:00:00") # verano: UTC+2
  expect_equal(m$battery_v, 4.8)
  expect_false(m$name_modified)
})

test_that("AudioMoth sin GUANO: fecha desde el comentario", {
  m <- extract_recording_metadata(
    example_dir("audiomoth", "20260811", "24BBD608675B68F7_20260811_160000.WAV"))
  expect_equal(m$datetime_source, "comment")
  expect_equal(m$battery_v, 4.6)
  expect_equal(m$temperature_c, 18)
})

test_that("AudioMoth renombrado: se detecta y se guarda el nombre original", {
  m <- extract_recording_metadata(
    example_dir("audiomoth", "20260810", "24BBD608675B68F7_20260810_160100 1.WAV"))
  expect_true(m$name_modified)
  expect_equal(m$original_name, "24BBD608675B68F7_20260810_160100.WAV")
  expect_equal(local_time(m$start_utc, "UTC"), "2026-08-10 16:01:00")
})

test_that("grabadora manual con bext: modelo TASCAM y hora local", {
  m <- extract_recording_metadata(example_dir("tascam", "DR0000_0001.wav"))
  expect_equal(m$recorder_type, "manual")
  expect_equal(m$model, "TASCAM DR-05X")
  expect_equal(m$datetime_source, "bext")
  expect_true(m$datetime_reliable)
  # bext no lleva zona: se interpreta como hora local de Madrid
  expect_equal(local_time(m$start_utc), "2025-04-10 17:53:05")
  expect_equal(local_time(m$start_utc, "UTC"), "2025-04-10 15:53:05")
  expect_equal(m$channels, 2L)
  expect_equal(m$bits, 24L)
})

test_that("bext de otro programa: grabadora manual sin modelo (se preguntará)", {
  f <- tempfile(fileext = ".wav")
  write_test_wav(f, bext = list(originator = "REAPER", date = "2025-04-10", time = "17-53-05"))
  m <- extract_recording_metadata(f)
  expect_equal(m$recorder_type, "manual")
  expect_true(is.na(m$model))
  expect_equal(local_time(m$start_utc), "2025-04-10 17:53:05") # separador "-" admitido
})

test_that("sin metadatos: grabadora desconocida y fecha poco fiable", {
  m <- extract_recording_metadata(example_dir("desconocida", "grabacion_sin_metadatos.wav"))
  expect_equal(m$recorder_type, "unknown")
  expect_equal(m$datetime_source, "file_mtime")
  expect_false(m$datetime_reliable)
})

test_that("los clips del AudioMoth Filter Playground no se toman por AudioMoth", {
  f <- tempfile(fileext = ".wav")
  write_test_wav(f, info = list(ICMT = "Audio clip exported from the AudioMoth Filter Playground.",
                                IART = "AudioMoth Filter Playground"))
  expect_equal(detect_recorder(read_wav_header(f))$recorder_type, "unknown")
})

test_that("nombre AAAAMMDD_HHMMSS sin metadatos: fecha desde el nombre (poco fiable)", {
  f <- file.path(tempdir(), "20250410_175305.wav")
  write_test_wav(f)
  m <- extract_recording_metadata(f)
  expect_equal(m$datetime_source, "filename")
  expect_false(m$datetime_reliable)
  expect_equal(local_time(m$start_utc), "2025-04-10 17:53:05")
})
