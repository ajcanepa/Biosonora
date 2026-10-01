# Exportación a Observation.org, taxonomía y exportación completa

# Lee las celdas de texto de una hoja del ejemplo de Observation.org.
# Un .xlsx es un ZIP con XML dentro: así no hace falta ningún paquete extra.
read_example_sheet <- function(sheet_file) {
  xlsx <- testthat::test_path("..", "..", "Instrucciones", "observations-import-example.xlsx")
  skip_if_not(file.exists(xlsx), "No está el ejemplo de Observation.org")
  dir <- tempfile()
  utils::unzip(xlsx, files = paste0("xl/worksheets/", sheet_file), exdir = dir)
  xml <- paste(readLines(file.path(dir, "xl", "worksheets", sheet_file), warn = FALSE,
                         encoding = "UTF-8"), collapse = "")
  xml <- gsub("<c [^>]*/>", "", xml) # celdas vacías (<c ... />)
  cells <- regmatches(xml, gregexpr('<c r="([A-Z]+)([0-9]+)"[^>]*>(.*?)</c>', xml))[[1]]
  ref <- sub('^<c r="([A-Z]+[0-9]+)".*$', "\\1", cells)
  value <- ifelse(grepl("<t>", cells), sub("^.*<t>(.*)</t>.*$", "\\1", cells),
                  sub("^.*<v>(.*)</v>.*$", "\\1", cells))
  data.frame(col = gsub("[0-9]", "", ref), row = as.integer(gsub("[A-Z]", "", ref)),
             value = value, stringsAsFactors = FALSE)
}

# Base de datos de ejemplo: grabaciones, punto de muestreo y detecciones
export_db <- function() {
  con <- db_connect(tempfile(fileext = ".sqlite"))
  db_upsert_recordings(con, scan_folder(example_dir()))
  recs <- DBI::dbGetQuery(con, "SELECT recording_id, file_path, file_name, original_name FROM recordings")
  for (name in c("24BBD608675B68F7_20260810_160000.BirdNET.results.csv",
                 "combinado.BirdNET.selection.table.txt")) {
    d <- match_detections_to_recordings(read_birdnet_file(example_dir("birdnet", name)), recs, name)
    db_import_detections(con, d, name, attr(d, "format"))
  }
  site <- db_save_site(con, "Río Arlanzón", 42.341063, -3.701844, 25)
  am <- recs$recording_id[grepl("^24BBD", recs$file_name)]
  db_assign_site(con, am, site)
  con
}
det_id <- function(con, species, file_pattern) {
  DBI::dbGetQuery(con, "
    SELECT d.detection_id FROM detections d JOIN recordings r ON r.recording_id = d.recording_id
    WHERE d.scientific_name = ? AND r.file_name LIKE ? ORDER BY d.start_s",
    params = list(species, file_pattern))$detection_id
}

test_that("las columnas son exactamente las del ejemplo de Observation.org", {
  sheet <- read_example_sheet("sheet1.xml")
  header <- sheet$value[sheet$row == 1][order(match(sheet$col[sheet$row == 1], LETTERS))]
  expect_equal(observation_columns, header)
})

test_that("los valores fijos están en las tablas de valores permitidos para aves", {
  lookup <- read_example_sheet("sheet2.xml")
  wide <- reshape(lookup[lookup$row > 1, ], idvar = "row", timevar = "col", direction = "wide")
  names(wide) <- c("row", "column", "value", "group")
  cfg <- observation_org_config()
  allowed <- function(col) wide$value[wide$column == col & (is.na(wide$group) | wide$group == "Aves")]
  expect_true(cfg$method %in% allowed("method"))
  expect_true(cfg$counting_method %in% allowed("counting method"))
  expect_true(cfg$activity %in% allowed("activity"))
  expect_equal(cfg$notes, "ID mediante BirdNet APP")
})

test_that("por defecto solo se exportan las detecciones validadas, agrupadas por especie, punto y día", {
  con <- export_db()
  on.exit(DBI::dbDisconnect(con))
  # Nada validado todavía: nada que exportar
  expect_equal(nrow(build_observation_export(con)$rows), 0)

  # Validamos los dos mirlos del 10/08 y el del 11/08
  for (id in det_id(con, "Turdus merula", "%")) db_validate_detection(con, id, "correct", "Ana")
  r <- build_observation_export(con)
  expect_equal(nrow(r$rows), 2) # mismo punto, dos días
  expect_equal(names(r$rows), observation_columns)
  day1 <- r$rows[r$rows$date == "2026-08-10", ]
  expect_equal(day1$time, "18:00")              # 16:00 UTC -> 18:00 Madrid
  expect_equal(day1$`scientific name`, "Turdus merula")
  expect_equal(day1$lat, 42.341063)
  expect_equal(day1$lng, -3.701844)
  expect_equal(day1$accuracy, 25L)
  expect_equal(day1$number, 1L)
  expect_equal(day1$notes, "ID mediante BirdNet APP")
  expect_equal(day1$`is certain`, "True")
  expect_equal(day1$method, "Oído")
  expect_equal(day1$`counting method`, "Visto/Sin contar")
  expect_equal(day1$activity, "Presente")
  expect_equal(r$n_unverified, 0)
})

test_that("opcionalmente se incluyen no revisadas por encima del umbral (no verificadas)", {
  con <- export_db()
  on.exit(DBI::dbDisconnect(con))
  r <- build_observation_export(con, list(include_unverified = TRUE, min_confidence = 0.5))
  # Mirlo 10/08 (0,87 y 0,96), mirlo 11/08 (0,61) y petirrojo (0,43: fuera)
  expect_setequal(r$rows$`scientific name`, "Turdus merula")
  expect_true(all(r$rows$`is certain` == "False"))
  expect_equal(r$n_unverified, 3)
  # Nunca la voz humana (aunque supere el umbral)
  expect_false(any(grepl("Human", r$rows$`scientific name`)))
  # Ni las incorrectas ni las dudosas
  for (id in det_id(con, "Turdus merula", "%20260811%")) db_validate_detection(con, id, "doubtful")
  r2 <- build_observation_export(con, list(include_unverified = TRUE, min_confidence = 0.5))
  expect_equal(r2$rows$date, "2026-08-10")
})

test_that("sin coordenadas o sin correspondencia taxonómica no se puede exportar", {
  con <- export_db()
  on.exit(DBI::dbDisconnect(con))
  rec <- DBI::dbGetQuery(con, "SELECT recording_id FROM recordings WHERE file_name = 'DR0000_0001.wav'")$recording_id
  db_add_manual_detection(con, rec, 0, 0.3, 1000, 4000, "Especie inventada", user = "Ana")
  id <- db_add_manual_detection(con, rec, 0, 0.3, 1000, 4000, "Parus major", user = "Ana")
  r <- build_observation_export(con)
  expect_equal(r$missing_coords$file_name, "DR0000_0001.wav")
  expect_equal(r$unmatched$source_name, "Especie inventada")
  expect_equal(nrow(r$rows), 0)

  # Se resuelve: punto con coordenadas y equivalencia manual
  site <- db_save_site(con, "Parral", 42.3, -3.7, 10)
  db_assign_site(con, rec, site)
  db_set_taxon_map(con, "Especie inventada", "Parus major", "Ana")
  r2 <- build_observation_export(con)
  expect_equal(nrow(r2$missing_coords), 0)
  expect_equal(nrow(r2$unmatched), 0)
  expect_equal(r2$rows$`scientific name`, "Parus major") # ambas se agrupan en una
  expect_equal(r2$rows$time, "17:53")                     # bext: hora local
})

test_that("la taxonomía traduce nombres de Clements a IOC y respeta los del usuario", {
  con <- db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  tax <- resolve_taxonomy(con, c("Turdus merula", "Oressochen jubatus", "Nada nada"))
  expect_equal(tax$target_name, c("Turdus merula", "Neochen jubata", NA))
  expect_equal(tax$method, c("ioc", "clements", "none"))
  expect_equal(tax$spanish[1], "Mirlo común")
  db_set_taxon_map(con, "Nada nada", "Turdus merula", "Ana")
  expect_equal(resolve_taxonomy(con, "Nada nada")$method, "user")
  db_set_taxon_map(con, "Nada nada", NULL)
  expect_equal(resolve_taxonomy(con, "Nada nada")$method, "none")
})

test_that("el CSV generado tiene exactamente la estructura del ejemplo", {
  con <- export_db()
  on.exit(DBI::dbDisconnect(con))
  for (id in det_id(con, "Turdus merula", "%")) db_validate_detection(con, id, "correct", "Ana")
  file <- tempfile(fileext = ".csv")
  write_observation_csv(build_observation_export(con)$rows, file)

  raw <- readBin(file, "raw", 3)
  expect_false(identical(raw, as.raw(c(0xEF, 0xBB, 0xBF)))) # sin BOM
  lines <- readLines(file, encoding = "UTF-8")
  header <- strsplit(gsub('"', "", lines[1]), ",")[[1]]
  sheet <- read_example_sheet("sheet1.xml")
  expected <- sheet$value[sheet$row == 1][order(match(sheet$col[sheet$row == 1], LETTERS))]
  expect_equal(header, expected)

  back <- utils::read.csv(file, check.names = FALSE, encoding = "UTF-8", colClasses = "character")
  expect_equal(names(back), expected)
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", back$date)))   # como el ejemplo: 2026-09-30
  expect_true(all(grepl("^\\d{2}:\\d{2}$", back$time)))          # como el ejemplo: 11:03
  expect_true(all(grepl("^-?\\d+\\.\\d+$", back$lat)))           # punto decimal, sin notación científica
  expect_true(all(back$`is certain` %in% c("True", "False")))
  expect_equal(unique(back$method), "Oído")                      # tildes en UTF-8
  expect_equal(back$obscurity, rep("", nrow(back)))              # lo decide Observation.org
})

test_that("la exportación completa incluye todo menos la voz humana", {
  con <- export_db()
  on.exit(DBI::dbDisconnect(con))
  all <- build_detections_export(con, list(), "es")
  expect_equal(nrow(all), 4) # 5 importadas - 1 voz humana
  expect_false(any(grepl("Human", all$nombre_cientifico)))
  expect_true(all(c("archivo", "fecha", "hora", "punto", "latitud", "confianza_pct",
                    "validacion", "revisor") %in% names(all)))
  expect_equal(nrow(build_detections_export(con, list(only_validated = TRUE))), 0)
  expect_equal(names(build_detections_export(con, list(), "en"))[1], "file")
  xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(all, xlsx)
  expect_gt(file.size(xlsx), 1000)
})
