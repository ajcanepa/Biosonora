# Análisis de carpetas y base de datos

test_that("analiza la carpeta de ejemplo completa", {
  recs <- scan_folder(example_dir())
  expect_equal(nrow(recs), 7)
  by_name <- stats::setNames(seq_len(nrow(recs)), recs$file_name)

  # Errores: el archivo que no es audio queda registrado, sin detener el análisis
  bad <- recs[by_name[["no_es_audio.wav"]], ]
  expect_equal(bad$error_key, "error.wav_not_riff")

  # Duplicados: la copia " 1" apunta al original
  dup <- recs[by_name[["24BBD608675B68F7_20260810_160000 1.WAV"]], ]
  expect_equal(basename(dup$duplicate_of), "24BBD608675B68F7_20260810_160000.WAV")
  expect_true(is.na(recs$duplicate_of[by_name[["24BBD608675B68F7_20260810_160100 1.WAV"]]]))

  # Asociación al CONFIG.TXT (que está en la carpeta superior)
  am <- recs[recs$recorder_type == "audiomoth", ]
  expect_true(all(basename(am$config_path) == "CONFIG.TXT"))
  expect_true(is.na(recs$config_path[by_name[["DR0000_0001.wav"]]]))
})

test_that("una carpeta inexistente o sin WAV da errores comprensibles", {
  err <- tryCatch(scan_folder(file.path(tempdir(), "no_existe")), error = identity)
  expect_equal(err$key, "error.folder_not_found")
  empty <- file.path(tempdir(), "carpeta_vacia")
  dir.create(empty, showWarnings = FALSE)
  err <- tryCatch(scan_folder(empty), error = identity)
  expect_equal(err$key, "error.no_wav_found")
})

test_that("la base de datos guarda, actualiza y respeta el trabajo del usuario", {
  db_file <- tempfile(fileext = ".sqlite")
  con <- db_connect(db_file)
  on.exit(DBI::dbDisconnect(con))

  recs <- scan_folder(example_dir())
  expect_equal(db_upsert_recordings(con, recs), 7)
  all <- db_get_recordings(con, "Europe/Madrid")
  expect_equal(nrow(all), 7)
  am <- all[all$file_name == "24BBD608675B68F7_20260810_160000.WAV", ]
  expect_equal(am$start_local, "2026-08-10 18:00:00")
  expect_equal(am$hour_local, 18L)
  expect_equal(am$review_status, "unreviewed")

  # El usuario trabaja: punto de muestreo, grabadora y revisión
  site <- db_save_site(con, "Río Arlanzón", 42.34106, -3.70184, 10)
  db_assign_site(con, am$recording_id, site, observer = "Voluntaria 1")
  unknown <- all$recording_id[all$recorder_type == "unknown" & is.na(all$error_key)]
  db_set_recorder(con, unknown, "manual", "Grabadora del móvil")
  db_set_review_status(con, am$recording_id, "reviewed", "Voluntaria 1")

  # Volver a analizar la carpeta no borra nada de lo anterior
  db_upsert_recordings(con, scan_folder(example_dir()))
  again <- db_get_recordings(con, "Europe/Madrid")
  expect_equal(nrow(again), 7)
  am2 <- again[again$recording_id == am$recording_id, ]
  expect_equal(am2$site_name, "Río Arlanzón")
  expect_equal(am2$lat, 42.34106)
  expect_equal(am2$observer, "Voluntaria 1")
  expect_equal(am2$review_status, "reviewed")
  expect_equal(again$model[again$recording_id == unknown], "Grabadora del móvil")
  expect_equal(again$recorder_type[again$recording_id == unknown], "manual")
})

test_that("se validan los puntos de muestreo y estados", {
  con <- db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  key_of <- function(expr) tryCatch(expr, error = function(e) e$key)
  expect_equal(key_of(db_save_site(con, "  ")), "error.site_name_required")
  expect_equal(key_of(db_save_site(con, "A", 95, 3)), "error.latitude_range")
  expect_equal(key_of(db_save_site(con, "A", 40, 200)), "error.longitude_range")
  expect_equal(key_of(db_save_site(con, "A", 40, NA)), "error.coordinates_incomplete")
  expect_equal(key_of(db_set_review_status(con, 1, "hecho")), "error.invalid_status")
  # Un punto sin coordenadas es válido; guardarlo de nuevo actualiza sus datos
  id1 <- db_save_site(con, "Charca", NA, NA)
  id2 <- db_save_site(con, "Charca", 42, -3)
  expect_equal(id1, id2)
  expect_equal(db_get_sites(con)$lat, 42)
})
