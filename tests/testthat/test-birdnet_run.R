# Ejecución de BirdNET en segundo plano

test_that("la semana del año sigue el criterio de BirdNET (4 por mes, 1-48)", {
  expect_equal(birdnet_week(as.Date(c("2026-01-01", "2026-01-31", "2026-08-10",
                                      "2026-12-31", "2024-02-29"))),
               c(1L, 4L, 30L, 48L, 8L))
})

test_that("los argumentos se pasan como vector (un dato del usuario nunca es una orden)", {
  params <- list(min_confidence = 0.25, overlap = 0.5, sensitivity = 1, locale = "es", threads = 2)
  rara <- "/tmp/mis grabaciones; rm -rf ~"
  args <- birdnet_args(rara, "/tmp/salida", params, lat = 42.34, lon = -3.7, week = 30)
  expect_equal(args[1:3], c("-m", "birdnet_analyzer.analyze", rara)) # un único elemento
  expect_equal(args[which(args == "--lat") + 1], "42.34")
  expect_equal(args[which(args == "--week") + 1], "30")
  expect_equal(args[which(args == "--min_conf") + 1], "0.25")
  expect_equal(args[which(args == "-l") + 1], "es")
  # Sin coordenadas ni semana: no se pasan esos filtros
  plain <- birdnet_args("/in", "/out", params)
  expect_false(any(c("--lat", "--lon", "--week") %in% plain))
})

test_that("las grabaciones se agrupan en lotes con los mismos parámetros", {
  recs <- data.frame(
    recording_id = 1:4, file_path = paste0("/a/", 1:4, ".wav"),
    start_utc = as.POSIXct(c("2026-08-10 16:00", "2026-08-10 17:00", "2026-08-25 16:00",
                             "2026-08-10 16:00"), tz = "UTC"),
    lat = c(42.3, 42.3, 42.3, NA), lon = c(-3.7, -3.7, -3.7, NA)
  )
  b <- birdnet_batches(recs)
  expect_length(b, 3) # (punto, semana 30), (punto, semana 32), (sin punto, semana 30)
  sizes <- unname(sort(vapply(b, function(x) length(x$recording_ids), 1L)))
  expect_equal(sizes, c(1L, 1L, 2L))
  expect_length(birdnet_batches(recs, use_location = FALSE, use_week = FALSE), 1)
})

test_that("la carpeta de entrada enlaza cada grabación con su identificador delante", {
  src <- example_dir("audiomoth", "20260810", "24BBD608675B68F7_20260810_160000.WAV")
  dir <- tempfile()
  links <- prepare_birdnet_input(list(recording_ids = 7L, file_paths = src), dir)
  expect_equal(links$link_name, "rec7__24BBD608675B68F7_20260810_160000.WAV")
  expect_true(file.exists(file.path(dir, links$link_name)))
})

test_that("la tabla de Raven de un audio (sin nombre científico) se traduce con los códigos de eBird", {
  f <- example_dir("birdnet", "24BBD608675B68F7_20260810_160000.BirdNET.selection.table.txt")
  skip_if(is.null(birdnet_code_map()), "BirdNET no está instalado")
  d <- read_birdnet_file(f)
  expect_equal(d$scientific_name, c("Delichon urbicum", "Motacilla alba"))
  expect_equal(d$common_name, c("Avión Común", "Lavandera Blanca"))
})

test_that("sin BirdNET instalado, la tabla de Raven sin nombres da un error claro", {
  withr_env <- Sys.getenv("BIOSONORA_BIRDNET_ENV")
  Sys.setenv(BIOSONORA_BIRDNET_ENV = tempfile())
  on.exit(Sys.setenv(BIOSONORA_BIRDNET_ENV = withr_env))
  f <- example_dir("birdnet", "24BBD608675B68F7_20260810_160000.BirdNET.selection.table.txt")
  err <- tryCatch(read_birdnet_file(f), error = identity)
  expect_equal(err$key, "error.birdnet_raven_no_scientific")
  expect_null(birdnet_python())
  expect_equal(tryCatch(new_birdnet_job(data.frame(), list()), error = function(e) e$key),
               "error.birdnet_not_installed")
})

test_that("un análisis real de BirdNET se ejecuta en segundo plano y se importa", {
  skip_if(is.null(birdnet_python()), "BirdNET no está instalado")
  skip_on_cran()
  con <- db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  wav <- file.path(tempfile(), "tono_6s.wav")
  write_test_wav(wav, sample_rate = 48000, duration_s = 6, tone_hz = 3000)
  db_upsert_recordings(con, scan_folder(dirname(wav)))
  recs <- db_get_recordings(con)
  job <- new_birdnet_job(recs, list(min_confidence = 0.1, overlap = 0, sensitivity = 1,
                                    locale = "es", threads = 2), use_location = FALSE)
  for (i in 1:120) { # como máximo 2 minutos
    poll_birdnet_job(job)
    if (!job$status %in% c("pending", "running")) break
    Sys.sleep(1)
  }
  expect_equal(job$status, "finished")
  expect_equal(job$done, 1L)
  expect_length(list.files(job$work_dir, "\\.BirdNET\\.results\\.csv$", recursive = TRUE), 1)
  n <- import_birdnet_job(con, job, "Test")
  expect_true(n >= 0) # un tono puro casi nunca da detecciones, pero no debe fallar
  expect_false(dir.exists(job$work_dir)) # los temporales se borran tras importar
})
