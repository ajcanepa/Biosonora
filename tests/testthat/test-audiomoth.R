# Lectores específicos de AudioMoth

test_that("lee el CONFIG.TXT real de AudioMoth", {
  cfg <- read_audiomoth_config(example_dir("audiomoth", "CONFIG.TXT"))
  expect_equal(cfg$device_id, "24BBD608675B68F7")
  expect_equal(cfg$firmware, "AudioMoth-Firmware-Basic (1.12.1)")
  expect_equal(cfg$sample_rate, 250000L)
  expect_equal(cfg$gain, "Medium")
  expect_equal(cfg$record_s, 55)
  expect_equal(cfg$sleep_s, 5)
  expect_equal(cfg$periods$start, "16:00")
  expect_equal(cfg$periods$end, "21:00")
  expect_equal(cfg$periods$utc_offset_h, 0)
  expect_equal(cfg$first_date, "2026-08-10")
  expect_equal(cfg$last_date, "2026-08-14")
  expect_equal(format(cfg$device_time_utc, "%Y-%m-%d %H:%M:%S"), "2026-08-09 10:33:01")
  # "-" significa "no aplica"
  expect_true(is.na(cfg$raw[["Filter"]]))
  # Se conservan todas las claves, también las desconocidas
  expect_true("Enable magnetic switch" %in% names(cfg$raw))
})

test_that("CONFIG.TXT con zona horaria distinta de UTC", {
  f <- tempfile(fileext = ".TXT")
  writeLines(c("Device ID : 0123456789ABCDEF",
               "Device time : 2026-08-09 12:33:01 (UTC+2)",
               "Recording period 1 : 18:00 - 23:00 (UTC+2)",
               "Campo nuevo : valor"), f)
  cfg <- read_audiomoth_config(f)
  expect_equal(cfg$utc_offset_h, 2)
  expect_equal(format(cfg$device_time_utc, "%H:%M"), "10:33")
  expect_equal(cfg$periods$utc_offset_h, 2)
  expect_equal(cfg$raw[["Campo nuevo"]], "valor")
})

test_that("interpreta los nombres de archivo de AudioMoth", {
  x <- parse_audiomoth_filename("2403750567C2A798_20260810_160000.WAV")
  expect_equal(x$device_id, "2403750567C2A798")
  expect_equal(x$datetime_text, "20260810_160000")
  expect_equal(x$suffix, "")
  y <- parse_audiomoth_filename("20260810_160000.wav")
  expect_true(is.na(y$device_id))
  z <- parse_audiomoth_filename("2403750567C2A798_20260810_172300 1.WAV")
  expect_equal(z$suffix, " 1")
  expect_null(parse_audiomoth_filename("Carbonero.wav"))
})

test_that("interpreta el comentario de AudioMoth", {
  x <- parse_audiomoth_comment(paste(
    "Recorded at 16:00:00 10/08/2026 (UTC) by AudioMoth 2403750567C2A798",
    "at medium gain while battery was 4.8V and temperature was 39.1C."))
  expect_equal(format(x$datetime_utc, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-08-10 16:00:00")
  expect_equal(x$device_id, "2403750567C2A798")
  expect_equal(x$gain, "medium")
  expect_equal(x$battery_v, 4.8)
  expect_equal(x$temperature_c, 39.1)

  # Hora local con desfase, batería fuera de rango y temperatura negativa
  y <- parse_audiomoth_comment(paste(
    "Recorded at 18:00:00 10/08/2026 (UTC+2) by AudioMoth 2403750567C2A798",
    "at high gain while battery was less than 2.5V and temperature was -3.5C."))
  expect_equal(format(y$datetime_utc, "%H:%M", tz = "UTC"), "16:00")
  expect_equal(y$battery_v, 2.5)
  expect_equal(y$temperature_c, -3.5)

  expect_null(parse_audiomoth_comment("Audio clip exported"))
})

test_that("interpreta marcas de tiempo GUANO y desfases UTC", {
  expect_equal(format(parse_guano_timestamp("2026-08-10T16:00:00Z"), "%H:%M", tz = "UTC"), "16:00")
  expect_equal(format(parse_guano_timestamp("2026-08-10T18:00:00+02:00"), "%H:%M", tz = "UTC"), "16:00")
  expect_true(is.na(parse_guano_timestamp("2026-08-10T16:00:00"))) # sin zona: no se adivina
  expect_equal(parse_utc_offset("(UTC)"), 0)
  expect_equal(parse_utc_offset("(UTC+2)"), 2)
  expect_equal(parse_utc_offset("(UTC-3:30)"), -3.5)
  expect_true(is.na(parse_utc_offset("sin zona")))
})
