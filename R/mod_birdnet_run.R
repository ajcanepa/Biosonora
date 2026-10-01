#' Módulo para ejecutar BirdNET desde la app (en segundo plano)
#'
#' Un botón abre una ventana con los parámetros. BirdNET se ejecuta como un
#' proceso aparte: la app sigue funcionando, se ve el progreso y se puede
#' cancelar. Al terminar, los resultados se importan solos.
#'
#' @param id Identificador del módulo.
#' @noRd
mod_birdnet_run_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("open"), i18n("birdnet_run.button"), icon = icon("play"),
                 class = "btn-outline-primary w-100 mb-2"),
    uiOutput(ns("progress"))
  )
}

#' @param con Conexión a la base de datos.
#' @param lang reactive con el idioma.
#' @param tracks Resultado de mod_track_list_server() (pista abierta, visibles y marcadas).
#' @param reviewer Resultado de mod_reviewer_server().
#' @param data_changed reactiveVal de cambios de datos.
#' @noRd
mod_birdnet_run_server <- function(id, con, lang, tracks, reviewer, data_changed) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tz <- local_timezone()
    job <- reactiveVal(NULL)
    tick <- reactiveVal(0) # avisa a la interfaz de que el trabajo ha avanzado

    # Cancelar el proceso si se cierra la sesión
    session$onSessionEnded(function() {
      j <- isolate(job())
      if (!is.null(j)) cancel_birdnet_job(j)
    })

    observeEvent(input$open, {
      l <- lang()
      if (!is.null(job()) && job()$status %in% c("pending", "running")) {
        showNotification(tr("birdnet_run.already_running", l), type = "warning")
        return()
      }
      if (is.null(birdnet_python())) {
        showModal(modalDialog(
          title = tr("birdnet_run.not_installed_title", l),
          p(tr("birdnet_run.not_installed_help", l)),
          tags$pre(class = "small", birdnet_install_instructions()),
          easyClose = TRUE, footer = modalButton(tr("common.close", l))
        ))
        return()
      }
      n_visible <- length(tracks$visible_ids())
      n_selected <- length(tracks$selected_ids())
      showModal(modalDialog(
        title = tr("birdnet_run.title", l),
        radioButtons(ns("scope"), tr("birdnet_run.scope", l),
                     choiceNames = list(tr("birdnet_run.scope_current", l),
                                        tr("birdnet_run.scope_selected", l, n = n_selected),
                                        tr("birdnet_run.scope_visible", l, n = n_visible)),
                     choiceValues = c("current", "selected", "visible"),
                     selected = if (n_selected > 0) "selected" else "current"),
        sliderInput(ns("min_conf"), tr("birdnet_run.min_confidence", l), min = 5, max = 95,
                    value = 25, step = 5, post = " %", ticks = FALSE),
        checkboxInput(ns("use_location"), tr("birdnet_run.use_location", l), TRUE),
        checkboxInput(ns("use_week"), tr("birdnet_run.use_week", l), TRUE),
        div(class = "d-flex gap-3 flex-wrap",
          sliderInput(ns("overlap"), tr("birdnet_run.overlap", l), min = 0, max = 2.5,
                      value = 0, step = 0.5, post = " s", ticks = FALSE),
          sliderInput(ns("sensitivity"), tr("birdnet_run.sensitivity", l), min = 0.5, max = 1.5,
                      value = 1, step = 0.25, ticks = FALSE)
        ),
        selectInput(ns("locale"), tr("birdnet_run.locale", l),
                    choices = c("Español" = "es", "English" = "en_uk"),
                    selected = if (l == "en") "en_uk" else "es"),
        p(class = "text-muted small", tr("birdnet_run.help", l)),
        footer = tagList(modalButton(tr("common.cancel", l)),
                         actionButton(ns("start"), tr("birdnet_run.start", l), class = "btn-primary")),
        easyClose = TRUE
      ))
    })

    observeEvent(input$start, {
      l <- lang()
      ids <- switch(input$scope,
        current = tracks$current(),
        selected = tracks$selected_ids(),
        visible = tracks$visible_ids()
      )
      if (length(ids) == 0) {
        showNotification(tr("birdnet_run.nothing_to_analyze", l), type = "warning")
        return()
      }
      recs <- db_get_recordings(con, tz)
      recs <- recs[recs$recording_id %in% ids & is.na(recs$error_key), , drop = FALSE]
      params <- list(min_confidence = input$min_conf / 100, overlap = input$overlap,
                     sensitivity = input$sensitivity, locale = input$locale,
                     threads = max(1L, parallel::detectCores() - 1L))
      new_job <- tryCatch(
        new_birdnet_job(recs, params, isTRUE(input$use_location), isTRUE(input$use_week), tz),
        error = function(e) {
          showNotification(error_to_message(e, l), type = "error")
          NULL
        })
      if (is.null(new_job)) return()
      removeModal()
      if (isTRUE(input$use_location) && any(is.na(recs$lat))) {
        showNotification(tr("birdnet_run.no_location_warning", l, n = sum(is.na(recs$lat))),
                         type = "warning", duration = 12)
      }
      job(new_job)
      tick(tick() + 1)
    })

    # Consulta el progreso cada segundo mientras BirdNET trabaja (sin bloquear)
    observe({
      j <- job()
      if (is.null(j) || !j$status %in% c("pending", "running")) return()
      invalidateLater(1000)
      poll_birdnet_job(j)
      isolate(tick(tick() + 1))
      if (j$status == "finished") finish(j)
      if (j$status == "failed") {
        showNotification(tr("birdnet_run.failed", lang()), type = "error", duration = 20)
      }
    })

    finish <- function(j) {
      n <- tryCatch(import_birdnet_job(con, j, isolate(reviewer$name())), error = function(e) {
        showNotification(error_to_message(e, isolate(lang())), type = "error")
        NA
      })
      if (is.na(n)) return()
      data_changed(isolate(data_changed()) + 1)
      showNotification(tr("birdnet_run.finished", isolate(lang()), n = n, files = j$total),
                       type = "message", duration = 12)
    }

    observeEvent(input$cancel, {
      j <- job()
      if (is.null(j)) return()
      cancel_birdnet_job(j)
      tick(tick() + 1)
      showNotification(tr("birdnet_run.cancelled", lang()), type = "warning")
    })

    output$progress <- renderUI({
      tick()
      j <- job()
      l <- lang()
      if (is.null(j) || !j$status %in% c("pending", "running")) return(NULL)
      pct <- round(100 * j$done / max(1, j$total))
      elapsed <- as.numeric(difftime(Sys.time(), j$started_at, units = "secs"))
      div(
        class = "small mb-2",
        div(class = "d-flex justify-content-between",
            tags$span(icon("spinner", class = "fa-spin"), " ",
                      tr("birdnet_run.progress", l, done = j$done, total = j$total)),
            tags$span(format_duration(elapsed))),
        div(class = "progress my-1", role = "progressbar", `aria-valuenow` = pct,
            `aria-valuemin` = 0, `aria-valuemax` = 100,
            `aria-label` = tr("birdnet_run.progress", l, done = j$done, total = j$total),
            div(class = "progress-bar", style = sprintf("width: %d%%", pct))),
        actionButton(ns("cancel"), tr("birdnet_run.cancel", l), icon = icon("stop"),
                     class = "btn-outline-danger btn-sm")
      )
    })
  })
}

#' Instrucciones para instalar el entorno de BirdNET (para la ventana de ayuda)
#' @noRd
birdnet_install_instructions <- function() {
  paste(
    "# 1. Instalar uv (una vez): https://docs.astral.sh/uv/getting-started/installation/",
    "curl -LsSf https://astral.sh/uv/install.sh | sh",
    "",
    "# 2. Desde la carpeta python/ del proyecto Biosonora:",
    sprintf("UV_PROJECT_ENVIRONMENT=\"%s\" uv sync --frozen", birdnet_env_dir()),
    sep = "\n"
  )
}
