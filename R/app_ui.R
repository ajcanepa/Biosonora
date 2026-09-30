#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Recursos externos (CSS, JS, favicon)
    golem_add_external_resources(),
    # Página base con Bootstrap 5 (bslib). Los módulos se añadirán en la Fase 1.
    # "Biosonora" es el nombre propio de la app, no necesita traducción.
    bslib::page_navbar(
      title = "Biosonora",
      theme = bslib::bs_theme(version = 5)
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Biosonora"
    )
  )
}
