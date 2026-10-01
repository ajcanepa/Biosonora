# Importación de BirdNET, detecciones, validación y listado de especies

birdnet_file <- function(name) example_dir("birdnet", name)

# Base de datos temporal con las grabaciones de ejemplo
example_db <- function() {
  con <- db_connect(tempfile(fileext = ".sqlite"))
  db_upsert_recordings(con, scan_folder(example_dir()))
  con
}
recording_id_of <- function(con, file_name) {
  DBI::dbGetQuery(con, "SELECT recording_id FROM recordings WHERE file_name = ?",
                  params = list(file_name))$recording_id
}

test_that("lee el formato sencillo (archivo real de ejemplo)", {
  d <- read_birdnet_file(birdnet_file("Blue_Jay_Sample_BirdNET_Results.csv"))
  expect_equal(attr(d, "format"), "simple")
  expect_equal(nrow(d), 29)
  expect_equal(d$scientific_name[1], "Cyanocitta cristata")
  expect_equal(d$common_name[1], "Blue Jay")
  expect_equal(d$confidence[1], 0.9815)
  expect_true(all(is.na(d$file_ref)))
  expect_true(all(d$category == "species"))
})

test_that("lee el CSV de BirdNET-Analyzer con la columna File", {
  d <- read_birdnet_file(birdnet_file("24BBD608675B68F7_20260810_160000.BirdNET.results.csv"))
  expect_equal(attr(d, "format"), "analyzer_csv")
  expect_equal(nrow(d), 3)
  expect_equal(d$common_name[1], "Mirlo común") # tildes en UTF-8
  expect_match(d$file_ref[1], "24BBD608675B68F7_20260810_160000.WAV$")
})

test_that("lee la tabla de Raven: inicio real desde File Offset y voz humana", {
  d <- read_birdnet_file(birdnet_file("combinado.BirdNET.selection.table.txt"))
  expect_equal(attr(d, "format"), "raven")
  expect_equal(d$start_s, c(0, 3, 0.5))   # la 3ª: Begin Time 60.5 pero File Offset 0.5
  expect_equal(d$end_s, c(3, 6, 3.5))
  expect_equal(d$high_hz, c(4000, 4000, 4000))
  expect_equal(d$category, c("species", "human", "species"))
})

test_that("los archivos con formato desconocido o valores erróneos dan errores claros", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("especie,hora", "Turdus merula,10"), f)
  err <- tryCatch(read_birdnet_file(f, "mal.csv"), error = identity)
  expect_equal(err$key, "error.birdnet_unknown_format")
  expect_match(err$data$columns, "especie, hora")

  writeLines(c("start,end,scientific_name,common_name,confidence",
               "0,3,Turdus merula,Mirlo,1.5"), f)
  err <- tryCatch(read_birdnet_file(f, "conf.csv"), error = identity)
  expect_equal(err$key, "error.birdnet_bad_confidence")
  expect_equal(err$data$rows, "2")

  writeLines(c("start,end,scientific_name,common_name,confidence",
               "5,3,Turdus merula,Mirlo,0.5"), f)
  expect_equal(tryCatch(read_birdnet_file(f), error = function(e) e$key), "error.birdnet_bad_times")

  writeLines(character(), f)
  expect_equal(tryCatch(read_birdnet_file(f), error = function(e) e$key), "error.birdnet_empty")
})

test_that("deduce el audio a partir del nombre del archivo de resultados", {
  expect_equal(audio_stem_from_results("Blue_Jay_Sample_BirdNET_Results.csv"), "Blue_Jay_Sample")
  expect_equal(audio_stem_from_results("pista.BirdNET.selection.table.txt"), "pista")
  expect_equal(audio_stem_from_results("pista.WAV.BirdNET.results.csv"), "pista")
})

test_that("asocia las detecciones a sus grabaciones", {
  con <- example_db()
  on.exit(DBI::dbDisconnect(con))
  recs <- DBI::dbGetQuery(con, "SELECT recording_id, file_path, file_name, original_name FROM recordings")

  # Ruta de Windows en la columna File: se asocia por el nombre del archivo
  d <- read_birdnet_file(birdnet_file("24BBD608675B68F7_20260810_160000.BirdNET.results.csv"))
  d <- match_detections_to_recordings(d, recs, "x.csv")
  expect_true(all(d$recording_id == recording_id_of(con, "24BBD608675B68F7_20260810_160000.WAV")))

  # Tabla combinada: la última fila es de un audio que no está
  r <- read_birdnet_file(birdnet_file("combinado.BirdNET.selection.table.txt"))
  r <- match_detections_to_recordings(r, recs, "combinado.txt")
  expect_equal(r$recording_id[1:2], rep(recording_id_of(con, "24BBD608675B68F7_20260811_160000.WAV"), 2))
  expect_true(is.na(r$recording_id[3]))

  # Sin columna de audio: se deduce del nombre del archivo de resultados
  s <- read_birdnet_file(birdnet_file("Blue_Jay_Sample_BirdNET_Results.csv"))
  s <- match_detections_to_recordings(s, recs, "grabacion_sin_metadatos_BirdNET_Results.csv")
  expect_true(all(s$recording_id == recording_id_of(con, "grabacion_sin_metadatos.wav")))
})

test_that("importar dos veces no duplica y conserva la validación", {
  con <- example_db()
  on.exit(DBI::dbDisconnect(con))
  recs <- DBI::dbGetQuery(con, "SELECT recording_id, file_path, file_name, original_name FROM recordings")
  f <- birdnet_file("24BBD608675B68F7_20260810_160000.BirdNET.results.csv")
  d <- match_detections_to_recordings(read_birdnet_file(f), recs, basename(f))
  expect_equal(db_import_detections(con, d, basename(f), "analyzer_csv", user = "Ana"), 3)

  rec <- d$recording_id[1]
  det <- db_get_detections(con, rec)
  expect_equal(det$low_hz, rep(0, 3))          # banda por defecto de BirdNET
  expect_equal(det$high_hz, rep(15000, 3))
  db_validate_detection(con, det$detection_id[1], "correct", "Ana")

  expect_equal(db_import_detections(con, d, basename(f), "analyzer_csv"), 0)
  det2 <- db_get_detections(con, rec)
  expect_equal(nrow(det2), 3)
  expect_equal(det2$validation[det2$detection_id == det$detection_id[1]], "correct")

  # Umbral de confianza
  expect_equal(nrow(db_get_detections(con, rec, min_confidence = 0.5)), 2)
})

test_that("validar, corregir, anotar y borrar dejan rastro en el historial", {
  con <- example_db()
  on.exit(DBI::dbDisconnect(con))
  rec <- recording_id_of(con, "24BBD608675B68F7_20260810_160000.WAV")
  recs <- DBI::dbGetQuery(con, "SELECT recording_id, file_path, file_name, original_name FROM recordings")
  f <- birdnet_file("24BBD608675B68F7_20260810_160000.BirdNET.results.csv")
  db_import_detections(con, match_detections_to_recordings(read_birdnet_file(f), recs, "x"),
                       "x", "analyzer_csv")
  det <- db_get_detections(con, rec)
  robin <- det$detection_id[det$scientific_name == "Erithacus rubecula"]

  # Corregir la especie la deja validada como correcta
  db_correct_species(con, robin, "Phoenicurus ochruros", "Colirrojo tizón", "Luis")
  d <- db_get_detection(con, robin)
  expect_equal(d$species, "Phoenicurus ochruros")
  expect_equal(d$species_common, "Colirrojo tizón")
  expect_equal(d$scientific_name, "Erithacus rubecula") # el original se conserva
  expect_equal(d$validation, "correct")
  expect_equal(d$validated_by, "Luis")

  db_set_detection_notes(con, robin, "  Canto lejano  ", "Luis")
  expect_equal(db_get_detection(con, robin)$notes, "Canto lejano")

  h <- db_get_history(con, robin)
  expect_equal(h$action, c("species", "notes"))
  expect_equal(h$user, c("Luis", "Luis"))

  # Anotación manual: correcta, sin confianza, se puede borrar
  id <- db_add_manual_detection(con, rec, 0.1, 0.4, 500, 2500, "Parus major", "Carbonero común",
                                user = "Luis")
  m <- db_get_detection(con, id)
  expect_equal(m$source, "manual")
  expect_equal(m$validation, "correct")
  expect_true(is.na(m$confidence))
  db_delete_manual_detection(con, id, "Luis")
  expect_null(db_get_detection(con, id))
  expect_equal(db_get_history(con, id)$action, c("created", "deleted"))

  # Las detecciones de BirdNET no se borran
  key_of <- function(expr) tryCatch(expr, error = function(e) e$key)
  expect_equal(key_of(db_delete_manual_detection(con, robin)), "error.only_manual_delete")
  expect_equal(key_of(db_add_manual_detection(con, rec, 1, 1, 0, 1, "X")), "error.annotation_too_small")
  expect_equal(key_of(db_add_manual_detection(con, rec, 0, 1, 0, 1, " ")), "error.species_required")
  # Voz humana sin especie: se guarda como "Human vocal"
  hid <- db_add_manual_detection(con, rec, 0, 0.2, 0, 3000, "", category = "human")
  expect_equal(db_get_detection(con, hid)$scientific_name, "Human vocal")
})

test_that("el listado de especies resume, filtra y excluye lo que debe", {
  con <- example_db()
  on.exit(DBI::dbDisconnect(con))
  recs <- DBI::dbGetQuery(con, "SELECT recording_id, file_path, file_name, original_name FROM recordings")
  for (name in c("24BBD608675B68F7_20260810_160000.BirdNET.results.csv",
                 "combinado.BirdNET.selection.table.txt")) {
    d <- match_detections_to_recordings(read_birdnet_file(birdnet_file(name)), recs, name)
    db_import_detections(con, d, name, attr(d, "format"))
  }
  sp <- compute_species_list(con)
  # Voz humana excluida; la detección sin grabación no se importó
  expect_setequal(sp$scientific_name, c("Turdus merula", "Erithacus rubecula"))
  blackbird <- sp[sp$scientific_name == "Turdus merula", ]
  expect_equal(blackbird$n_detections, 3)
  expect_equal(blackbird$n_recordings, 2)
  expect_equal(blackbird$max_confidence, 0.955)
  expect_equal(blackbird$mean_confidence, mean(c(0.8712, 0.955, 0.61)))
  expect_equal(blackbird$status, "pending")

  # Umbral
  expect_equal(compute_species_list(con, list(min_confidence = 0.5))$scientific_name, "Turdus merula")

  # Una incorrecta desaparece; una correcta valida la especie
  det <- DBI::dbGetQuery(con, "SELECT detection_id, scientific_name FROM detections")
  db_validate_detection(con, det$detection_id[det$scientific_name == "Erithacus rubecula"], "incorrect")
  db_validate_detection(con, det$detection_id[det$scientific_name == "Turdus merula"][1], "correct")
  sp2 <- compute_species_list(con)
  expect_equal(sp2$scientific_name, "Turdus merula")
  expect_equal(sp2$status, "validated")
  expect_equal(sp2$n_correct, 1L)

  # Filtro por fecha (hora local) y por grabación
  expect_equal(nrow(compute_species_list(con, list(date_from = "2026-08-11"))), 1)
  expect_equal(compute_species_list(con, list(date_from = "2026-08-11"))$n_detections, 1)
  rec <- recording_id_of(con, "24BBD608675B68F7_20260810_160000.WAV")
  expect_equal(compute_species_list(con, list(recording_ids = rec))$n_recordings, 1)

  # Se guarda en la base de datos y se exporta en porcentaje
  id <- db_save_species_list(con, sp2, list(min_confidence = 0), "Ana")
  items <- DBI::dbGetQuery(con, "SELECT * FROM species_list_items WHERE list_id = ?", params = list(id))
  expect_equal(items$scientific_name, "Turdus merula")
  out <- species_list_for_export(sp2, "es")
  expect_equal(out$max_confidence_pct, 95.5)
  expect_equal(out$validation_status, "Validada")
  csv <- tempfile(fileext = ".csv")
  write_csv_utf8(out, csv)
  expect_equal(readBin(csv, "raw", 3), as.raw(c(0xEF, 0xBB, 0xBF))) # BOM para Excel
  back <- utils::read.csv(csv, encoding = "UTF-8", check.names = FALSE)
  expect_equal(back$common_name, "Mirlo común")
})
