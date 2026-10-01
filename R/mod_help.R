#' Ayuda: guía de bienvenida y explicaciones contextuales
#'
#' La guía aparece la primera vez que se abre la app en un navegador (se
#' recuerda con help.js) y se puede volver a abrir con el botón «Ayuda».
#'
#' @param id Identificador del módulo.
#' @noRd
mod_help_ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("open"), i18n("help.button"), icon = icon("circle-question"),
               class = "btn-outline-secondary btn-sm bs-help-button",
               `data-first-visit-input` = ns("first_visit"))
}

# Pasos de la guía: icono y clave de traducción
welcome_steps <- list(
  list(icon = "folder-open", key = "welcome.step1"),
  list(icon = "filter", key = "welcome.step2"),
  list(icon = "wave-square", key = "welcome.step3"),
  list(icon = "feather", key = "welcome.step4"),
  list(icon = "file-export", key = "welcome.step5")
)

#' @param lang reactive con el idioma.
#' @noRd
mod_help_server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    step <- reactiveVal(1)

    show_welcome <- function() {
      step(1)
      l <- lang()
      showModal(modalDialog(
        title = tagList(icon("feather"), tr("welcome.title", l)),
        uiOutput(ns("step")),
        footer = uiOutput(ns("footer")),
        size = "l", easyClose = TRUE
      ))
    }

    observeEvent(input$open, show_welcome())
    observeEvent(input$first_visit, if (isTRUE(input$first_visit)) show_welcome())

    output$step <- renderUI({
      l <- lang()
      s <- welcome_steps[[step()]]
      div(
        class = "bs-welcome",
        if (step() == 1) p(class = "lead", tr("welcome.intro", l)),
        div(class = "d-flex gap-3 align-items-start",
            div(class = "bs-welcome-icon", icon(s$icon)),
            div(h5(tr(paste0(s$key, "_title"), l)), p(tr(paste0(s$key, "_text"), l)))),
        p(class = "text-muted small mb-0",
          tr("welcome.progress", l, step = step(), total = length(welcome_steps)))
      )
    })

    output$footer <- renderUI({
      l <- lang()
      last <- step() == length(welcome_steps)
      tagList(
        if (step() > 1) actionButton(ns("back"), tr("welcome.back", l), class = "btn-outline-secondary"),
        if (!last) actionButton(ns("next_step"), tr("welcome.next", l), class = "btn-primary"),
        if (last) modalButton(tr("welcome.start", l))
      )
    })

    observeEvent(input$next_step, step(min(step() + 1, length(welcome_steps))))
    observeEvent(input$back, step(max(step() - 1, 1)))
  })
}

#' Icono «?» con una explicación al pasar el ratón o al enfocarlo con el teclado
#'
#' @param key Clave de traducción del texto de ayuda.
#' @noRd
help_icon <- function(key, lang = default_language) {
  bslib::tooltip(
    tags$span(class = "bs-help-icon", tabindex = "0", role = "img",
              `aria-label` = tr(key, lang), `data-i18n-aria-label` = key,
              icon("circle-question")),
    i18n(key, lang),
    placement = "right"
  )
}
