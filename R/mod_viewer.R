#' Módulo visor: espectrograma, forma de onda y reproducción de una pista
#'
#' El espectrograma se calcula en R (imagen PNG guardada en caché) y se
#' muestra dentro de wavesurfer.js, que dibuja la forma de onda, el cursor de
#' reproducción, la línea de tiempo y permite hacer clic para saltar.
#'
#' @param id Identificador del módulo.
#' @noRd
mod_viewer_ui <- function(id) {
  ns <- NS(id)
  speed_names <- lapply(playback_speeds, function(s) paste0(format(s), "x"))
  bslib::card(
    full_screen = TRUE,
    class = "bs-viewer-card",
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
      uiOutput(ns("title"), inline = TRUE),
      uiOutput(ns("review_button"), inline = TRUE)
    ),
    bslib::card_body(
      # Barra de controles
      div(
        class = "bs-toolbar d-flex flex-wrap align-items-end gap-2 mb-2",
        div(
          class = "btn-group", role = "group",
          icon_button(actionButton(ns("prev"), NULL, icon = icon("backward-step"),
                                   class = "btn-outline-secondary"), "viewer.previous"),
          icon_button(tags$button(id = ns("play"), type = "button",
                                  class = "btn btn-primary bs-play-btn",
                                  icon("play")), "viewer.play_pause"),
          icon_button(actionButton(ns("next_track"), NULL, icon = icon("forward-step"),
                                   class = "btn-outline-secondary"), "viewer.next")
        ),
        tags$span(id = ns("time"), class = "bs-time", "0:00 / 0:00"),
        div(class = "bs-control",
            radioButtons(ns("speed"), tagList(i18n("viewer.speed"), help_icon("help.speed")), inline = TRUE,
                         choiceNames = speed_names, choiceValues = playback_speeds,
                         selected = 1)),
        div(
          class = "bs-control",
          tags$label(class = "form-label d-block", i18n("viewer.zoom")),
          div(class = "btn-group", role = "group",
              icon_button(tags$button(id = ns("zoom_out"), type = "button",
                                      class = "btn btn-outline-secondary bs-zoom-out",
                                      icon("magnifying-glass-minus")), "viewer.zoom_out"),
              icon_button(tags$button(id = ns("zoom_in"), type = "button",
                                      class = "btn btn-outline-secondary bs-zoom-in",
                                      icon("magnifying-glass-plus")), "viewer.zoom_in"))
        ),
        div(class = "bs-control bs-slider",
            sliderInput(ns("freq"), tagList(i18n("viewer.freq_range"), help_icon("help.freq_range")), min = 0, max = 24,
                        value = c(0, 24), step = 0.5, post = " kHz", ticks = FALSE)),
        div(class = "bs-control bs-slider-sm",
            sliderInput(ns("contrast"), tagList(i18n("viewer.contrast"), help_icon("help.contrast")), min = 30, max = 120,
                        value = 70, step = 5, post = " dB", ticks = FALSE)),
        div(class = "bs-control",
            radioButtons(ns("palette"), tagList(i18n("viewer.palette"), help_icon("help.spectro_palette")),
                         inline = TRUE,
                         choiceNames = list("Viridis", "Magma", "Cividis", i18n("viewer.palette_grey")),
                         choiceValues = c("viridis", "magma", "cividis", "grey"))),
        bslib::popover(
          tags$button(type = "button", class = "btn btn-outline-info btn-sm align-self-center",
                      icon("keyboard"), i18n("viewer.shortcuts")),
          shortcuts_help(),
          title = i18n("viewer.shortcuts")
        )
      ),
      # Zona del espectrograma
      div(
        id = ns("player"), class = "bs-player", role = "region",
        `aria-label` = tr("viewer.player_region"), `data-i18n-aria-label` = "viewer.player_region",
        div(class = "bs-freq-axis", `aria-hidden` = "true"),
        div(class = "bs-wave"),
        div(class = "bs-player-empty", i18n("viewer.empty")),
        div(class = "bs-player-loading d-none", icon("spinner", class = "fa-spin"),
            i18n("viewer.loading"))
      ),
      uiOutput(ns("metadata"))
    )
  )
}

# Tabla de atajos de teclado
shortcuts_help <- function() {
  keys <- list(
    c("Espacio / Space", "viewer.key_play"),
    c("← →", "viewer.key_seek"),
    c("+ −", "viewer.key_zoom"),
    c("N / P", "viewer.key_next_prev"),
    c("1 2 3 4", "viewer.key_speed"),
    c("R", "viewer.key_reviewed"),
    c("V / X / D", "viewer.key_validate"),
    c("F", "viewer.key_fragment"),
    c("\u2191 \u2193", "viewer.key_detections"),
    c("A", "viewer.key_annotate"),
    c("Esc", "viewer.key_escape")
  )
  tags$table(
    class = "table table-sm mb-0",
    tags$tbody(lapply(keys, function(k) tags$tr(tags$td(tags$kbd(k[1])), tags$td(i18n(k[2])))))
  )
}

#' @param con Conexión a la base de datos.
#' @param lang reactive con el idioma.
#' @param current reactive con el recording_id a mostrar.
#' @param media Lista con dir (carpeta temporal de la sesión) y url (prefijo web).
#' @param data_changed reactiveVal de cambios de datos.
#' @param key_event reactive con las teclas enviadas desde el navegador.
#' @param reviewer Resultado de mod_reviewer_server().
#' @return Lista con `nav` (reactiveVal: +1 siguiente, -1 anterior).
#' @noRd
mod_viewer_server <- function(id, con, lang, current, media, data_changed, key_event, reviewer) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    player_id <- ns("player")
    nav <- reactiveVal(NULL)
    spec_state <- reactiveVal(NULL) # espectrograma calculado de la pista actual
    cache_dir <- media$cache_dir

    recording <- reactive({
      req(current())
      data_changed()
      db_get_recording(con, current())
    })

    # ---- Al abrir una pista ----
    observeEvent(current(), {
      rec <- isolate(recording())
      req(rec)
      if (rec$review_status == "unreviewed") {
        db_set_review_status(con, rec$recording_id, "in_review", isolate(reviewer$name()))
        data_changed(data_changed() + 1)
      }
      if (!is.na(rec$error_key)) {
        spec_state(NULL)
        showNotification(tr(rec$error_key, lang(), file = rec$file_name), type = "error")
        return()
      }
      session$sendCustomMessage("biosonora-viewer-loading", list(id = player_id))
      spec <- tryCatch({
        audio <- read_audio_mono(rec$file_path)
        s <- compute_spectrogram(audio)
        s$recording_id <- rec$recording_id
        s$file_path <- rec$file_path
        s
      }, error = function(e) {
        showNotification(error_to_message(e, lang()), type = "error", duration = 10)
        NULL
      })
      if (is.null(spec)) {
        session$sendCustomMessage("biosonora-viewer-error", list(id = player_id))
        return()
      }
      nyquist <- spec$freq_max / 1000
      step <- if (nyquist > 50) 1 else 0.5
      freezeReactiveValue(input, "freq")
      updateSliderInput(session, "freq", min = 0, max = nyquist, value = c(0, nyquist), step = step)
      spec_state(spec)
      load_player(spec, rec, as.numeric(isolate(input$speed)), keep_position = FALSE,
                  params = list(freq = c(0, nyquist), contrast = isolate(input$contrast),
                                palette = isolate(input$palette)))
    })

    # ---- Imagen del espectrograma (se regenera al cambiar rango, contraste o paleta) ----
    spectro_params <- reactive({
      list(freq = input$freq, contrast = input$contrast, palette = input$palette)
    }) |> debounce(400)

    spectrogram_url <- function(spec, params) {
      fmin <- params$freq[1] * 1000
      fmax <- min(params$freq[2] * 1000, spec$freq_max)
      if (fmax <= fmin) fmax <- spec$freq_max
      key <- cache_key(spec$file_path, fmin, fmax, params$contrast, params$palette)
      file <- file.path(cache_dir, paste0(key, ".png"))
      if (!file.exists(file)) {
        render_spectrogram_png(spec, file, fmin, fmax, params$contrast, params$palette)
        prune_cache(cache_dir)
      }
      list(url = paste0(media$cache_url, "/", basename(file)), fmin = fmin, fmax = fmax)
    }

    observeEvent(spectro_params(), {
      spec <- spec_state()
      req(spec)
      img <- spectrogram_url(spec, spectro_params())
      session$sendCustomMessage("biosonora-viewer-spectro", list(
        id = player_id, spectro_url = img$url, fmin = img$fmin, fmax = img$fmax
      ))
    }, ignoreInit = TRUE)

    # ---- Reproducción ----
    # params: rango de frecuencias, contraste y paleta (NULL = los de los controles)
    load_player <- function(spec, rec, speed, keep_position, params = NULL) {
      play <- tryCatch({
        file <- file.path(media$dir, sprintf("rec%d_%s.wav", rec$recording_id, format(speed)))
        # El WAV de reproducción se reutiliza si ya existe en esta sesión
        info <- if (file.exists(paste0(file, ".rds"))) {
          readRDS(paste0(file, ".rds"))
        } else {
          p <- make_playback_audio(rec$file_path, speed, file)
          saveRDS(p, paste0(file, ".rds"))
          p
        }
        info$url <- paste0(media$url, "/", basename(file))
        info
      }, error = function(e) {
        showNotification(error_to_message(e, lang()), type = "error")
        NULL
      })
      if (is.null(play)) return()
      if (is.null(params)) {
        params <- isolate(list(freq = input$freq, contrast = input$contrast,
                               palette = input$palette))
      }
      img <- spectrogram_url(spec, params)
      session$sendCustomMessage("biosonora-viewer-load", list(
        id = player_id,
        recording_id = rec$recording_id,
        audio_url = play$url,
        peaks = spec$peaks,
        play_duration = play$play_duration_s,
        orig_duration = spec$duration_s,
        speed = speed,
        spectro_url = img$url,
        fmin = img$fmin,
        fmax = img$fmax,
        keep_position = keep_position
      ))
    }

    observeEvent(input$speed, {
      spec <- spec_state()
      req(spec)
      rec <- isolate(recording())
      req(rec)
      load_player(spec, rec, as.numeric(input$speed), keep_position = TRUE)
    }, ignoreInit = TRUE)

    # ---- Navegación ----
    observeEvent(input$prev, nav(list(step = -1, t = Sys.time())))
    observeEvent(input$next_track, nav(list(step = 1, t = Sys.time())))

    # Atajos de teclado que necesitan al servidor (el resto se resuelve en JS)
    observeEvent(key_event(), {
      key <- key_event()$key
      if (key %in% c("n", "N")) nav(list(step = 1, t = Sys.time()))
      if (key %in% c("p", "P")) nav(list(step = -1, t = Sys.time()))
      if (key %in% c("1", "2", "3", "4")) {
        updateRadioButtons(session, "speed", selected = playback_speeds[as.integer(key)])
      }
      if (key %in% c("r", "R")) toggle_reviewed()
    })

    # ---- Estado de revisión ----
    toggle_reviewed <- function() {
      rec <- isolate(recording())
      if (is.null(rec)) return()
      user <- reviewer$require()
      if (is.null(user)) return()
      new_status <- if (rec$review_status == "reviewed") "in_review" else "reviewed"
      db_set_review_status(con, rec$recording_id, new_status, user)
      data_changed(data_changed() + 1)
    }
    observeEvent(input$toggle_reviewed, toggle_reviewed())

    output$review_button <- renderUI({
      rec <- recording()
      l <- lang()
      reviewed <- rec$review_status == "reviewed"
      actionButton(
        ns("toggle_reviewed"),
        tr(if (reviewed) "viewer.mark_unreviewed" else "viewer.mark_reviewed", l),
        icon = icon(if (reviewed) "rotate-left" else "check"),
        class = if (reviewed) "btn-outline-secondary btn-sm" else "btn-success btn-sm"
      )
    })

    output$title <- renderUI({
      rec <- recording()
      l <- lang()
      tz <- local_timezone()
      status <- rec$review_status
      tags$span(
        class = "d-flex align-items-center gap-2 flex-wrap",
        icon("wave-square"),
        tags$strong(rec$file_name),
        tags$span(class = "text-muted",
                  format(rec$start_utc, tz = tz, format = "%Y-%m-%d %H:%M:%S"),
                  paste0("(", tz, ")")),
        tags$span(class = paste0("badge bs-status-badge bs-status-", status),
                  tr(paste0("status.", status), l))
      )
    })

    output$metadata <- renderUI({
      rec <- recording()
      metadata_panel(rec, lang())
    })

    nav
  })
}

#' Panel con los metadatos de la pista
#' @noRd
metadata_panel <- function(rec, lang) {
  tz <- local_timezone()
  value <- function(x, suffix = "") if (is.null(x) || is.na(x) || x == "") "—" else paste0(x, suffix)
  items <- list(
    c("meta.recorder", value(tr(paste0("recorder.", rec$recorder_type), lang))),
    c("meta.model", value(rec$model)),
    c("meta.device_id", value(rec$device_id)),
    c("meta.local_time", paste(format(rec$start_utc, tz = tz, format = "%Y-%m-%d %H:%M:%S"), tz)),
    c("meta.utc_time", paste(format(rec$start_utc, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"), "UTC")),
    c("meta.datetime_source", tr(paste0("datetime_source.", rec$datetime_source), lang)),
    c("meta.duration", format_duration(rec$duration_s)),
    c("meta.sample_rate", value(rec$sample_rate, " Hz")),
    c("meta.channels", value(rec$channels)),
    c("meta.bits", value(rec$bits)),
    c("meta.gain", value(rec$gain)),
    c("meta.battery", value(rec$battery_v, " V")),
    c("meta.temperature", value(rec$temperature_c, " °C")),
    c("meta.site", value(rec$site_name)),
    c("meta.coordinates", if (is.na(rec$lat)) "—" else sprintf("%.5f, %.5f", rec$lat, rec$lon)),
    c("meta.observer", value(rec$observer)),
    c("meta.file_path", rec$file_path)
  )
  warnings <- list()
  if (!isTRUE(as.logical(rec$datetime_reliable))) {
    warnings <- c(warnings, list(div(class = "alert alert-warning py-1 px-2 mb-1 small",
                                     icon("triangle-exclamation"), tr("meta.warn_unreliable_date", lang))))
  }
  if (isTRUE(as.logical(rec$name_modified))) {
    warnings <- c(warnings, list(div(class = "alert alert-info py-1 px-2 mb-1 small",
                                     icon("pen"), tr("meta.warn_renamed", lang, original = value(rec$original_name)))))
  }
  if (rec$recorder_type == "unknown") {
    warnings <- c(warnings, list(div(class = "alert alert-warning py-1 px-2 mb-1 small",
                                     icon("circle-question"), tr("meta.warn_unknown_recorder", lang))))
  }
  tags$details(
    class = "bs-metadata mt-2",
    open = if (length(warnings)) NA else NULL,
    tags$summary(tr("meta.title", lang)),
    warnings,
    tags$dl(
      class = "row small mb-0",
      lapply(items, function(it) tagList(
        tags$dt(class = "col-sm-4 col-lg-2", tr(it[1], lang)),
        tags$dd(class = "col-sm-8 col-lg-4 text-break", it[2])
      ))
    )
  )
}
