#' Módulo del revisor: nombre de la persona que valida y anota
#'
#' En modo local cada persona escribe su nombre una vez y el navegador lo
#' recuerda (reviewer.js). En modo servidor (fase 6) saldrá de su cuenta.
#'
#' @param id Identificador del módulo.
#' @noRd
mod_reviewer_ui <- function(id) {
  ns <- NS(id)
  tags$button(
    id = ns("open"), type = "button",
    class = "btn btn-outline-secondary btn-sm bs-reviewer action-button", # action-button: Shiny lo escucha
    `data-reviewer-input` = ns("name"), # (no usar data-input-id: Shiny lo toma como id del botón)
    icon("user"), tags$span(class = "bs-reviewer-name", i18n("reviewer.anonymous"))
  )
}

#' @return Lista con `name` (reactive con el nombre, "" si no hay) y
#'   `require(lang)` (devuelve el nombre o pide escribirlo y devuelve NULL).
#' @noRd
mod_reviewer_server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    name <- reactive(trimws(input$name %||% ""))

    ask <- function(reason_key = NULL) {
      l <- lang()
      showModal(modalDialog(
        title = tr("reviewer.title", l),
        if (!is.null(reason_key)) p(class = "text-muted", tr(reason_key, l)),
        p(tr("reviewer.help", l)),
        textInput(ns("new_name"), tr("reviewer.label", l), value = isolate(name())),
        footer = tagList(
          modalButton(tr("common.cancel", l)),
          actionButton(ns("save"), tr("common.save", l), class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    }

    observeEvent(input$open, ask())

    observeEvent(input$save, {
      new_name <- trimws(input$new_name %||% "")
      if (!nzchar(new_name)) {
        showNotification(tr("reviewer.required", lang()), type = "warning")
        return()
      }
      session$sendCustomMessage("biosonora-set-reviewer", list(id = ns("name"), name = new_name))
      removeModal()
    })

    list(
      name = name,
      # Devuelve el nombre; si falta, lo pide y devuelve NULL (la acción se
      # repite después de escribirlo)
      require = function() {
        n <- isolate(name())
        if (nzchar(n)) return(n)
        ask("reviewer.needed")
        NULL
      }
    )
  })
}
