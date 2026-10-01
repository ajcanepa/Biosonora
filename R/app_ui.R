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
    bslib::page_navbar(
      # "Biosonora" es el nombre propio de la app, no necesita traducción
      title = tags$span(icon("feather"), "Biosonora"),
      window_title = "Biosonora",
      id = "main_nav",
      theme = biosonora_theme(),
      fillable = FALSE,
      bslib::nav_panel(
        title = i18n("nav.review"),
        value = "review",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = 290,
            mod_load_ui("load"),
            tags$hr(),
            mod_track_list_filters_ui("tracks")
          ),
          mod_viewer_ui("viewer"),
          mod_track_list_ui("tracks")
        )
      ),
      bslib::nav_spacer(),
      bslib::nav_item(text_size_selector()),
      bslib::nav_item(
        tags$label(`for` = "lang", class = "visually-hidden", i18n("nav.language")),
        selectInput("lang", NULL, choices = c("Español" = "es", "English" = "en"),
                    selected = default_language, width = "120px", selectize = FALSE)
      ),
      bslib::nav_item(bslib::input_dark_mode(id = "dark_mode"))
    )
  )
}

#' Selector de tamaño de texto (A− / A / A+)
#'
#' Cambia el tamaño de letra de toda la página; se recuerda en el navegador.
#' @noRd
text_size_selector <- function() {
  sizes <- list(c("small", "A\u2212", "nav.text_smaller"),
                c("normal", "A", "nav.text_normal"),
                c("large", "A+", "nav.text_larger"))
  div(
    class = "btn-group btn-group-sm bs-text-size", role = "group",
    `aria-label` = tr("nav.text_size"), `data-i18n-aria-label` = "nav.text_size",
    lapply(sizes, function(s) {
      tags$button(
        type = "button", class = "btn btn-outline-secondary", `data-size` = s[1],
        title = tr(s[3]), `data-i18n-title` = s[3],
        `aria-label` = tr(s[3]), `data-i18n-aria-label` = s[3],
        s[2]
      )
    })
  )
}

#' Tema visual: Bootstrap 5 con paleta inspirada en la naturaleza
#' @noRd
biosonora_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = "#2E6B4F",   # verde bosque
    secondary = "#5F6F65", # gris musgo
    success = "#3C8D5A",
    info = "#2F7A9E",      # azul agua
    warning = "#B8741A",   # ocre
    danger = "#B23A3A",
    # El tamaño real lo fija el selector A− / A / A+ (text_size.js) cambiando
    # el tamaño de letra de la página: todo se escala en proporción
    "font-size-base" = "1rem",
    "h5-font-size" = "1.05rem",
    "spacer" = "0.85rem",
    "card-spacer-y" = "0.75rem",
    "card-spacer-x" = "0.9rem"
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
  # Librerías JavaScript de terceros: fuera de www/ para cargarlas en orden
  add_resource_path(
    "vendor",
    app_sys("app/vendor")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Biosonora"
    ),
    # wavesurfer.js (BSD-3): forma de onda, cursor y línea de tiempo
    tags$script(src = "vendor/wavesurfer/wavesurfer.min.js"),
    tags$script(src = "vendor/wavesurfer/timeline.min.js")
  )
}
