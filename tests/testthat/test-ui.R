# Tests de la interfaz con shinytest2: se abre la app en un navegador sin
# ventana (Chrome) y se simula lo que haría una persona.

skip_on_cran()
skip_if_not_installed("shinytest2")
skip_if(is.null(chromote::find_chrome()), "No hay Chrome para los tests de interfaz")

start_app <- function() {
  data_dir <- tempfile("biosonora_ui_")
  withr_old <- Sys.getenv("BIOSONORA_DATA_DIR")
  Sys.setenv(BIOSONORA_DATA_DIR = data_dir)
  app <- shinytest2::AppDriver$new(
    testthat::test_path("apps", "biosonora"), name = "biosonora",
    height = 1000, width = 1366, load_timeout = 60000, timeout = 30000
  )
  Sys.setenv(BIOSONORA_DATA_DIR = withr_old)
  # Sin guía de bienvenida (ya vista) para que no tape la pantalla
  app$run_js("localStorage.setItem('biosonora-welcome-seen', '1');")
  app$wait_for_js("document.querySelectorAll('#tracks-table .rt-tbody .rt-tr').length > 0", timeout = 60000)
  app
}

test_that("la app arranca, analiza la carpeta y muestra las grabaciones", {
  app <- start_app()
  on.exit(app$stop())
  expect_equal(app$get_js("document.title"), "Biosonora")
  # 7 grabaciones; la copia duplicada se oculta por defecto
  expect_match(app$get_text("#tracks-count"), "6 de 7")
  # La primera pista se abre sola en el visor
  app$wait_for_js("document.querySelector('#viewer-time').textContent !== '0:00 / 0:00'", timeout = 60000)
  expect_match(app$get_text("#viewer-title"), "\\.(wav|WAV)")
})

test_that("el cambio de idioma traduce la interfaz sin recargar", {
  app <- start_app()
  on.exit(app$stop())
  expect_equal(app$get_text("[data-i18n='load.scan_button']"), "Analizar carpeta")
  app$set_inputs(lang = "en")
  app$wait_for_js("document.querySelector(\"[data-i18n='load.scan_button']\").textContent === 'Scan folder'")
  expect_equal(app$get_js("document.documentElement.lang"), "en")
  app$wait_for_js("document.querySelector('#tracks-count').textContent.indexOf('of') >= 0")
  expect_match(app$get_text("#tracks-count"), "6 of 7")
})

test_that("importar BirdNET, validar con el teclado y ver el listado de especies", {
  app <- start_app()
  on.exit(app$stop())
  # El nombre del revisor no es un control normal: se envía como valor de Shiny
  app$run_js("Shiny.setInputValue('reviewer-name', 'Test');")
  app$upload_file(`birdnet-files` = example_dir("birdnet", "24BBD608675B68F7_20260810_160000.BirdNET.results.csv"))
  app$wait_for_idle()
  # Abrir la grabación de esas detecciones
  id <- app$get_js("Array.from(document.querySelectorAll('#tracks-table .rt-tbody .rt-tr')).findIndex(r => r.textContent.indexOf('20260810_160000.WAV') >= 0)")
  expect_gte(id, 0)
  app$run_js(sprintf("document.querySelectorAll('#tracks-table .rt-tbody .rt-tr')[%d].querySelectorAll('.rt-td')[2].click();", id))
  app$wait_for_js("document.querySelector('#detections-count') && document.querySelector('#detections-count').textContent.indexOf('3') >= 0", timeout = 60000)
  expect_match(app$get_text("#detections-count"), "3, 3 sin revisar")
  # Tecla V: la detección seleccionada pasa a correcta
  app$run_js("document.body.dispatchEvent(new KeyboardEvent('keydown', {key: 'v', bubbles: true}));")
  app$wait_for_js("document.querySelector('#detections-count').textContent.indexOf('2 sin revisar') >= 0")
  expect_match(app$get_text("#detections-count"), "2 sin revisar")
  # Pestaña de especies
  app$run_js("document.querySelector('a[data-value=\"species\"]').click();")
  app$wait_for_js("document.querySelectorAll('#species-table .rt-tbody .rt-tr').length > 0", timeout = 30000)
  expect_match(app$get_text("#species-table"), "Turdus merula")
})

test_that("la exportación a Observation.org avisa de lo que falta", {
  app <- start_app()
  on.exit(app$stop())
  app$upload_file(`birdnet-files` = example_dir("birdnet", "24BBD608675B68F7_20260810_160000.BirdNET.results.csv"))
  app$wait_for_idle()
  app$run_js("document.querySelector('a[data-value=\"export\"]').click();")
  app$wait_for_js("!!document.getElementById('export-check')")
  app$set_inputs(`export-include_unverified` = TRUE)
  app$click("export-check")
  app$wait_for_js("document.getElementById('export-obs_result').innerText.indexOf('punto de muestreo') >= 0")
  # Sin punto de muestreo con coordenadas no se puede descargar
  expect_match(app$get_text("#export-obs_result"), "no tienen punto de muestreo")
})

test_that("subir un ZIP añade sus grabaciones y avisa de lo rechazado", {
  app <- start_app()
  on.exit(app$stop())
  app$upload_file(`load-upload` = example_dir("otros", "subida_prueba.zip"))
  app$wait_for_js("document.querySelector('#tracks-count').textContent.indexOf('de 8') >= 0", timeout = 60000)
  expect_match(app$get_text("#tracks-count"), "de 8")
  notes <- app$get_js("Array.from(document.querySelectorAll('.shiny-notification')).map(n => n.textContent).join(' | ')")
  expect_match(notes, "no se han guardado")
  expect_match(notes, "ruta no permitida")
})
