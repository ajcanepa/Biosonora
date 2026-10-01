#' Módulo de carga: elegir una carpeta y analizar sus grabaciones
#'
#' En modo local se escribe (o se elige) la ruta de una carpeta del ordenador.
#' La subida de archivos para el modo servidor llegará en la fase 6.
#'
#' @param id Identificador del módulo.
#' @noRd
mod_load_ui <- function(id) {
  ns <- NS(id)
  initial_folder <- golem::get_golem_options("folder") %||% ""

  if (run_mode() != "local") {
    return(tagList(
      h5(i18n("load.title")),
      p(class = "text-muted small", i18n("load.server_pending"))
    ))
  }

  browse_available <- capabilities("tcltk") && nzchar(Sys.getenv("DISPLAY", "x"))
  tagList(
    h5(class = "mb-1", i18n("load.title")),
    p(class = "text-muted small mb-2", i18n("load.help")),
    i18n_attr(
      textInput(ns("folder"), label = tagList(i18n("load.folder_label"), help_icon("help.folder")),
                value = initial_folder, width = "100%"),
      "placeholder", "load.folder_placeholder"
    ),
    div(
      class = "d-flex gap-2 flex-wrap",
      if (browse_available) {
        actionButton(ns("browse"), i18n("load.browse"), icon = icon("folder-open"),
                     class = "btn-outline-secondary")
      },
      actionButton(ns("scan"), i18n("load.scan_button"), icon = icon("magnifying-glass"),
                   class = "btn-primary")
    )
  )
}

#' @param con Conexión a la base de datos.
#' @param lang reactive con el idioma actual.
#' @param data_changed reactiveVal que se incrementa cuando cambian los datos.
#' @noRd
mod_load_server <- function(id, con, lang, data_changed) {
  moduleServer(id, function(input, output, session) {

    # Diálogo del sistema para elegir carpeta (solo en modo local)
    observeEvent(input$browse, {
      folder <- tryCatch(tcltk::tk_choose.dir(caption = tr("load.browse", lang())),
                         error = function(e) NA)
      if (length(folder) == 1 && !is.na(folder) && nzchar(folder)) {
        updateTextInput(session, "folder", value = folder)
      }
    })

    run_scan <- function() {
      folder <- path.expand(trimws(input$folder %||% ""))
      recordings <- tryCatch(
        withProgress(message = tr("load.scanning", lang()), value = 0, {
          recs <- scan_folder(folder, local_timezone(), progress = function(i, n) {
            if (i %% 25 == 0 || i == n) {
              setProgress(i / n, detail = tr("load.progress", lang(), i = i, n = n))
            }
          })
          db_upsert_recordings(con, recs)
          recs
        }),
        error = function(e) {
          showNotification(error_to_message(e, lang()), type = "error", duration = 12)
          NULL
        }
      )
      if (is.null(recordings)) return()
      data_changed(data_changed() + 1)
      show_scan_summary(recordings, lang())
    }

    observeEvent(input$scan, run_scan())

    # Si la app se abre con una carpeta indicada, se analiza al arrancar
    observe({
      if (nzchar(isolate(input$folder %||% ""))) run_scan()
    }) |> bindEvent(session$clientData$url_hostname, once = TRUE)
  })
}

# Resumen comprensible de lo encontrado en la carpeta
show_scan_summary <- function(recordings, lang) {
  n <- nrow(recordings)
  count <- function(type) sum(recordings$recorder_type == type, na.rm = TRUE)
  errors <- sum(!is.na(recordings$error_key))
  duplicates <- sum(!is.na(recordings$duplicate_of))
  renamed <- sum(recordings$name_modified, na.rm = TRUE)
  unreliable <- sum(!recordings$datetime_reliable & is.na(recordings$error_key))

  lines <- list(tags$strong(tr("load.summary_found", lang, n = n)),
                tags$br(),
                tr("load.summary_types", lang, audiomoth = count("audiomoth"),
                   manual = count("manual"), unknown = count("unknown")))
  add <- function(key, value) {
    if (value > 0) lines <<- c(lines, list(tags$br(), tr(key, lang, n = value)))
  }
  add("load.summary_duplicates", duplicates)
  add("load.summary_renamed", renamed)
  add("load.summary_unreliable", unreliable)
  add("load.summary_errors", errors)
  if (count("unknown") > 0 || count("manual") > 0) {
    lines <- c(lines, list(tags$br(), tags$em(tr("load.summary_unknown_hint", lang))))
  }
  showNotification(tagList(lines), type = if (errors > 0) "warning" else "message",
                   duration = 15)
}
