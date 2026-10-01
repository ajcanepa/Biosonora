# Subida segura de archivos y espacios de datos

upload_df <- function(paths, names = basename(paths)) {
  data.frame(name = names, size = file.size(paths), datapath = paths, stringsAsFactors = FALSE)
}

test_that("los nombres de archivo se sanean (sin rutas ni caracteres peligrosos)", {
  expect_equal(sanitize_filename("../../etc/passwd"), "passwd")
  expect_equal(sanitize_filename("C:\\Users\\x\\grabación ñ 1.WAV"), "grabacion n 1.WAV")
  expect_equal(sanitize_filename("a;rm -rf $HOME.wav"), "a_rm -rf _HOME.wav")
  expect_equal(sanitize_filename("..wav"), "wav")
  expect_equal(sanitize_filename(""), "archivo")
  expect_lte(nchar(sanitize_filename(strrep("x", 500))), 120)
})

test_that("se reconoce el tipo real del archivo, no solo la extensión", {
  expect_equal(sniff_file_type(example_dir("tascam", "DR0000_0001.wav")), "wav")
  expect_equal(sniff_file_type(example_dir("otros", "subida_prueba.zip")), "zip")
  expect_true(is.na(sniff_file_type(example_dir("otros", "no_es_audio.wav"))))
})

test_that("se guardan los WAV válidos y se rechazan los demás con su motivo", {
  dest <- tempfile()
  txt <- tempfile(fileext = ".exe"); writeLines("hola", txt)
  files <- upload_df(
    c(example_dir("tascam", "DR0000_0001.wav"), example_dir("otros", "no_es_audio.wav"), txt),
    c("../DR0000_0001.wav", "no_es_audio.wav", "programa.exe"))
  res <- store_uploads(files, dest)
  expect_equal(basename(res$stored), "DR0000_0001.wav")
  expect_true(startsWith(normalizePath(res$stored), normalizePath(dest))) # dentro del destino
  expect_equal(res$rejected$reason, c("upload.bad_content", "upload.bad_extension"))

  # Mismo nombre otra vez: no se sobrescribe
  res2 <- store_uploads(files[1, ], dest)
  expect_equal(basename(res2$stored), "DR0000_0001_2.wav")

  # Tamaño máximo
  res3 <- store_uploads(files[1, ], tempfile(), max_mb = 0.01)
  expect_equal(res3$rejected$reason, "upload.too_big")
})

test_that("un ZIP se extrae de forma segura y conserva las carpetas para asociar el CONFIG.TXT", {
  dest <- tempfile()
  res <- store_uploads(upload_df(example_dir("otros", "subida_prueba.zip")), dest)
  rel <- sub(paste0("^", normalizePath(dest), "/"), "", normalizePath(res$stored))
  expect_setequal(rel, c("tarjeta SD/20260810/24BBD608675B68F7_20260810_160000.WAV",
                         "tarjeta SD/CONFIG.TXT"))
  expect_false(file.exists(file.path(dirname(dest), "escapar.wav"))) # zip slip bloqueado
  reasons <- stats::setNames(res$rejected$reason, sub("^.*: ", "", res$rejected$name))
  expect_equal(reasons[["../escapar.wav"]], "upload.dangerous_path")
  expect_equal(reasons[["tarjeta SD/notas.docx"]], "upload.bad_extension")
  expect_equal(reasons[["tarjeta SD/falso.wav"]], "upload.bad_content")

  # La grabación extraída se asocia a su CONFIG.TXT al analizar la carpeta
  recs <- scan_folder(dest)
  expect_equal(nrow(recs), 1)
  expect_equal(basename(recs$config_path), "CONFIG.TXT")

  # Bomba ZIP: contenido descomprimido mayor que el máximo
  big <- store_uploads(upload_df(example_dir("otros", "subida_prueba.zip")), tempfile(), max_mb = 0.0001)
  expect_true("upload.zip_too_big" %in% big$rejected$reason | "upload.too_big" %in% big$rejected$reason)
})

test_that("en modo local hay un único espacio de datos; en servidor, uno por usuario", {
  expect_null(user_space(list(user = "ana")))         # modo local (configuración por defecto)
  base <- app_data_dir()
  expect_equal(app_data_dir("ana_1"), normalizePath(file.path(base, "usuarios", "ana_1")))
})
