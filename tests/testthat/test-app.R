# Tests mínimos de la Fase 0: la app se construye y la configuración se lee

test_that("app_ui devuelve una interfaz de Shiny", {
  ui <- app_ui(request = NULL)
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("run_app crea un objeto de aplicación Shiny", {
  app <- run_app()
  expect_s3_class(app, "shiny.appobj")
})

test_that("la configuración por defecto es modo local con zona Europe/Madrid", {
  expect_equal(get_golem_config("run_mode", config = "default"), "local")
  expect_equal(get_golem_config("local_timezone", config = "default"), "Europe/Madrid")
  expect_equal(get_golem_config("run_mode", config = "production"), "server")
})

test_that("los valores fijos de Observation.org están configurados", {
  oo <- get_golem_config("observation_org", config = "default")
  expect_equal(oo$method, "Oído")
  expect_equal(oo$counting_method, "Visto/Sin contar")
  expect_equal(oo$notes_template, "ID mediante {classifier}")
})
