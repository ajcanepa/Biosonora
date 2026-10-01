#' Módulo de carga: elegir una carpeta y analizar sus grabaciones
#'
#' Modo local: se escribe (o se elige) la ruta de una carpeta del ordenador.
#' Modo servidor: se elige una de las carpetas autorizadas en la configuración.
#' En los dos modos se pueden subir WAV sueltos, un ZIP o el CONFIG.TXT.
#'
#' @param id Identificador del módulo.
#' @noRd
mod_load_ui <- function(id) {
  ns <- NS(id)
  upload <- tagList(
    tags$label(class = "form-label mt-2", i18n("upload.label"), help_icon("help.upload")),
    i18n_attr(
      fileInput(ns("upload"), label = NULL, multiple = TRUE, accept = c(".wav", ".WAV", ".zip", ".txt"),
                buttonLabel = i18n("upload.choose"), placeholder = tr("birdnet.no_file"), width = "100%"),
      "placeholder", "birdnet.no_file"
    )
  )

  if (run_mode() != "local") {
    # Modo servidor: solo carpetas autorizadas por el administrador
    folders <- server_folders()
    return(tagList(
      h5(class = "mb-1", i18n("load.title")),
      p(class = "text-muted small mb-2", i18n("load.help_server")),
      if (nrow(folders)) tagList(
        selectInput(ns("server_folder"), i18n("load.server_folder"),
                    choices = stats::setNames(seq_len(nrow(folders)), folders$name), width = "100%"),
        actionButton(ns("scan_server"), i18n("load.scan_button"), icon = icon("magnifying-glass"),
                     class = "btn-primary")
      ),
      upload
    ))
  }

  initial_folder <- golem::get_golem_options("folder") %||% ""
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
    ),
    upload
  )
}

#' @param con Conexión a la base de datos.
#' @param lang reactive con el idioma.
#' @param data_changed reactiveVal que se incrementa cuando cambian los datos.
#' @param data_dir Carpeta de datos del usuario (para guardar las subidas).
#' @noRd
mod_load_server <- function(id, con, lang, data_changed, data_dir = app_data_dir()) {
  moduleServer(id, function(input, output, session) {

    # Diálogo del sistema para elegir carpeta (solo en modo local)
    observeEvent(input$browse, {
      folder <- tryCatch(tcltk::tk_choose.dir(caption = tr("load.browse", lang())),
                         error = function(e) NA)
      if (length(folder) == 1 && !is.na(folder) && nzchar(folder)) {
        updateTextInput(session, "folder", value = folder)
      }
    })

    run_scan <- function(folder) {
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

    observeEvent(input$scan, {
      if (run_mode() != "local") return() # en servidor no se aceptan rutas escritas
      run_scan(path.expand(trimws(input$folder %||% "")))
    })

    # Modo servidor: la ruta sale de la configuración, nunca del navegador
    observeEvent(input$scan_server, {
      folders <- server_folders()
      k <- suppressWarnings(as.integer(input$server_folder))
      if (is.na(k) || k < 1 || k > nrow(folders)) return()
      run_scan(folders$path[k])
    })

    # Subida de WAV, ZIP o CONFIG.TXT: se validan y se guardan en una carpeta nueva
    observeEvent(input$upload, {
      l <- lang()
      dest <- file.path(data_dir, "subidas", format(Sys.time(), "%Y%m%d_%H%M%S"))
      res <- tryCatch(store_uploads(input$upload, dest), error = function(e) {
        showNotification(error_to_message(e, l), type = "error")
        NULL
      })
      if (is.null(res)) return()
      if (nrow(res$rejected)) {
        showNotification(tagList(
          tags$strong(tr("upload.rejected_title", l, n = nrow(res$rejected))),
          lapply(utils::head(seq_len(nrow(res$rejected)), 10), function(i) {
            tags$div(paste0(res$rejected$name[i], ": ", tr(res$rejected$reason[i], l)))
          })
        ), type = "warning", duration = 20)
      }
      if (!any(grepl("\\.wav$", res$stored, ignore.case = TRUE))) {
        unlink(dest, recursive = TRUE)
        showNotification(tr("upload.nothing_stored", l), type = "error")
        return()
      }
      run_scan(dest)
    })

    # Si la app se abre con una carpeta indicada (modo local), se analiza al arrancar
    observe({
      if (run_mode() == "local" && nzchar(isolate(input$folder %||% ""))) {
        run_scan(path.expand(trimws(isolate(input$folder))))
      }
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
