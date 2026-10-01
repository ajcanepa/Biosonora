# Traducciones

test_that("español e inglés tienen exactamente las mismas claves", {
  es <- names(i18n_dictionary("es"))
  en <- names(i18n_dictionary("en"))
  expect_setequal(es, en)
  expect_false(any(i18n_dictionary("en") == ""))
})

test_that("todas las claves escritas en el código existen", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir))
  code <- unlist(lapply(list.files(r_dir, full.names = TRUE), readLines, encoding = "UTF-8"))
  # Cualquier texto "seccion.clave" cuya sección exista en los archivos de idioma
  sections <- unique(sub("\\..*$", "", names(i18n_dictionary("es"))))
  keys <- unlist(regmatches(code, gregexpr('"[a-z_]+\\.[a-z0-9_]+"', code)))
  keys <- keys[sub('^"([a-z_]+)\\..*$', "\\1", keys) %in% sections]
  keys <- unique(gsub('"', "", keys))
  all_keys <- names(i18n_dictionary("es"))
  missing <- setdiff(keys, all_keys)
  # Prefijos que el código completa con paste0() (p. ej. "welcome.step1" + "_title")
  is_prefix <- vapply(missing, function(k) any(startsWith(all_keys, k)), logical(1))
  expect_equal(missing[!is_prefix], character())
  # Claves que se construyen con paste0()
  built <- c(paste0("status.", review_statuses),
             paste0("recorder.", c("audiomoth", "manual", "unknown")),
             paste0("datetime_source.", c("guano", "comment", "bext", "filename", "file_mtime")))
  expect_equal(setdiff(built, names(i18n_dictionary("es"))), character())
})

test_that("tr rellena marcadores y avisa de claves inexistentes", {
  expect_equal(tr("load.progress", "es", i = 3, n = 10), "3 de 10 archivos")
  expect_equal(tr("load.progress", "en", i = 3, n = 10), "3 of 10 files")
  expect_equal(tr("no.existe"), "no.existe")
  expect_equal(tr("common.save", "fr"), "Guardar") # idioma no soportado -> español
})

test_that("los errores se convierten en mensajes traducidos", {
  e <- tryCatch(biosonora_abort("error.wav_not_riff", file = "x.wav"), error = identity)
  expect_equal(error_to_message(e, "es"), "«x.wav» no es un archivo WAV válido.")
  expect_equal(error_to_message(e, "en"), "“x.wav” is not a valid WAV file.")
  expect_message(msg <- error_to_message(simpleError("fallo interno"), "es"))
  expect_equal(msg, tr("error.unexpected", "es"))
})
