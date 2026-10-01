#' Módulo de importación de resultados de BirdNET
#'
#' Se suben uno o varios archivos (CSV de BirdNET-Analyzer, tabla de Raven o
#' tabla sencilla). Cada detección se asocia a su grabación; si no se puede
#' saber a cuál pertenece un archivo, se pregunta al usuario.
#'
#' @param id Identificador del módulo.
#' @noRd
mod_birdnet_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5(class = "mb-1", i18n("birdnet.title")),
    mod_birdnet_run_ui("birdnet_run"),
    p(class = "text-muted small mb-2", i18n("birdnet.help")),
    i18n_attr(
      fileInput(ns("files"), label = NULL, multiple = TRUE, accept = c(".csv", ".txt"),
                buttonLabel = i18n("birdnet.choose"), placeholder = tr("birdnet.no_file"),
                width = "100%"),
      "placeholder", "birdnet.no_file"
    )
  )
}

#' @param con Conexión a la base de datos.
#' @param lang reactive con el idioma.
#' @param current reactive con la grabación abierta (se propone por defecto).
#' @param reviewer Resultado de mod_reviewer_server().
#' @param data_changed reactiveVal de cambios de datos.
#' @noRd
mod_birdnet_import_server <- function(id, con, lang, current, reviewer, data_changed) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pending <- reactiveVal(list()) # archivos leídos sin grabación asociada

    observeEvent(input$files, {
      files <- input$files
      l <- lang()
      recordings <- DBI::dbGetQuery(con, "SELECT recording_id, file_path, file_name, original_name FROM recordings")
      imported <- 0L
      matched_files <- 0L
      unmatched <- list()
      errors <- character()

      for (i in seq_len(nrow(files))) {
        name <- files$name[i]
        result <- tryCatch({
          det <- read_birdnet_file(files$datapath[i], display_name = name)
          det <- match_detections_to_recordings(det, recordings, name)
          n_new <- db_import_detections(con, det, name, attr(det, "format"),
                                        user = reviewer$name())
          if (any(!is.na(det$recording_id))) matched_files <- matched_files + 1L
          imported <- imported + n_new
          if (any(is.na(det$recording_id))) {
            unmatched[[name]] <- list(detections = det[is.na(det$recording_id), ],
                                      format = attr(det, "format"))
          }
          TRUE
        }, error = function(e) {
          errors <<- c(errors, error_to_message(e, l))
          FALSE
        })
      }

      if (length(errors)) {
        showNotification(tagList(lapply(errors, function(x) tags$div(x))), type = "error",
                         duration = 20)
      }
      if (imported > 0) {
        data_changed(data_changed() + 1)
        showNotification(tr("birdnet.imported", l, n = imported, files = matched_files),
                         type = "message", duration = 8)
      }
      if (length(unmatched)) {
        pending(unmatched)
        ask_recordings(unmatched, recordings, l)
      }
    })

    # Pregunta a qué grabación corresponde cada archivo sin asociar
    ask_recordings <- function(unmatched, recordings, l) {
      showModal(modalDialog(
        title = tr("birdnet.unmatched_title", l),
        p(tr("birdnet.unmatched_help", l)),
        lapply(seq_along(unmatched), function(k) {
          name <- names(unmatched)[k]
          det <- unmatched[[k]]$detections
          selectizeInput(ns(paste0("rec_", k)),
                         label = tr("birdnet.unmatched_file", l, file = name, n = nrow(det),
                                    duration = format_duration(max(det$end_s))),
                         choices = NULL, width = "100%")
        }),
        footer = tagList(
          modalButton(tr("common.cancel", l)),
          actionButton(ns("assign"), tr("birdnet.assign", l), class = "btn-primary")
        ),
        size = "l", easyClose = FALSE
      ))
      # Las opciones se buscan en el servidor: con cientos de grabaciones el
      # navegador no tiene que cargarlas todas
      choices <- c(stats::setNames("", tr("birdnet.skip", l)),
                   stats::setNames(as.character(recordings$recording_id), recordings$file_name))
      selected <- as.character(isolate(current()) %||% "")
      for (k in seq_along(unmatched)) {
        updateSelectizeInput(session, paste0("rec_", k), choices = choices,
                             selected = selected, server = TRUE)
      }
    }

    observeEvent(input$assign, {
      unmatched <- pending()
      l <- lang()
      imported <- 0L
      warnings <- character()
      for (k in seq_along(unmatched)) {
        rec_id <- input[[paste0("rec_", k)]]
        if (is.null(rec_id) || !nzchar(rec_id)) next
        det <- unmatched[[k]]$detections
        rec <- db_get_recording(con, rec_id)
        # Aviso si las detecciones pasan del final de la grabación elegida
        if (!is.null(rec) && max(det$end_s) > rec$duration_s + 1) {
          warnings <- c(warnings, tr("birdnet.beyond_duration", l, file = names(unmatched)[k],
                                     recording = rec$file_name))
        }
        det$recording_id <- as.integer(rec_id)
        imported <- imported + db_import_detections(con, det, names(unmatched)[k],
                                                    unmatched[[k]]$format, user = reviewer$name())
      }
      removeModal()
      pending(list())
      if (length(warnings)) {
        showNotification(tagList(lapply(warnings, tags$div)), type = "warning", duration = 15)
      }
      if (imported > 0) {
        data_changed(data_changed() + 1)
        showNotification(tr("birdnet.imported_manual", l, n = imported), type = "message")
      }
    })
  })
}
