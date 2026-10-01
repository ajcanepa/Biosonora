#' Módulo de lista de pistas: filtros, tabla y acciones sobre varias pistas
#'
#' Tiene dos partes de interfaz con el mismo id: los filtros (en la barra
#' lateral) y la tabla (en la zona principal).
#'
#' @param id Identificador del módulo.
#' @noRd
mod_track_list_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5(class = "mb-2", i18n("filters.title")),
    dateRangeInput(ns("dates"), i18n("filters.dates"), start = NULL, end = NULL,
                   weekstart = 1, separator = "–", width = "100%"),
    sliderInput(ns("hours"), i18n("filters.hours"), min = 0, max = 23,
                value = c(0, 23), step = 1, ticks = FALSE, width = "100%"),
    selectizeInput(ns("sites"), i18n("filters.sites"), choices = NULL,
                   multiple = TRUE, width = "100%"),
    selectizeInput(ns("recorders"), i18n("filters.recorders"), choices = NULL,
                   multiple = TRUE, width = "100%"),
    checkboxGroupInput(
      ns("status"), i18n("filters.status"),
      choiceNames = lapply(paste0("status.", review_statuses), i18n),
      choiceValues = review_statuses, selected = review_statuses
    ),
    radioButtons(
      ns("sort"), i18n("filters.sort"),
      choiceNames = list(i18n("filters.sort_date_asc"), i18n("filters.sort_date_desc"),
                         i18n("filters.sort_name"), i18n("filters.sort_status")),
      choiceValues = c("date_asc", "date_desc", "name", "status")
    ),
    checkboxInput(ns("show_duplicates"), i18n("filters.show_duplicates"), value = FALSE)
  )
}

#' @noRd
mod_track_list_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
      tags$span(icon("list"), i18n("list.title"), textOutput(ns("count"), inline = TRUE)),
      div(
        class = "d-flex gap-2 flex-wrap",
        actionButton(ns("assign_site"), i18n("list.assign_site"), icon = icon("location-dot"),
                     class = "btn-outline-primary btn-sm"),
        actionButton(ns("set_recorder"), i18n("list.set_recorder"), icon = icon("microphone"),
                     class = "btn-outline-primary btn-sm")
      )
    ),
    bslib::card_body(
      p(class = "text-muted small mb-1", i18n("list.help")),
      reactable::reactableOutput(ns("table"))
    )
  )
}

#' @param con Conexión a la base de datos.
#' @param lang reactive con el idioma.
#' @param data_changed reactiveVal que se incrementa cuando cambian los datos.
#' @return Lista con `current` (reactive con el recording_id abierto) y
#'   `move(step)` (abre la pista siguiente/anterior de la lista filtrada).
#' @noRd
mod_track_list_server <- function(id, con, lang, data_changed) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tz <- local_timezone()
    current <- reactiveVal(NULL)
    page_size <- 15

    all_recordings <- reactive({
      data_changed()
      db_get_recordings(con, tz)
    })

    # ---- Actualizar las opciones de los filtros cuando cambian los datos ----
    last_range <- reactiveVal(NULL)
    observe({
      recs <- all_recordings()
      dates <- recs$date_local[!is.na(recs$date_local)]
      if (length(dates)) {
        rng <- range(dates)
        if (!identical(rng, last_range())) {
          updateDateRangeInput(session, "dates", start = rng[1], end = rng[2],
                               min = rng[1], max = rng[2])
          last_range(rng)
        }
      }
      no_site <- tr("filters.no_site", lang())
      sites <- sort(unique(stats::na.omit(recs$site_name)))
      updateSelectizeInput(session, "sites", choices = c(stats::setNames("", no_site), sites),
                           selected = isolate(input$sites),
                           options = list(placeholder = tr("filters.all", lang())))
      recorders <- sort(unique(recorder_label(recs, lang())))
      updateSelectizeInput(session, "recorders", choices = recorders,
                           selected = isolate(input$recorders),
                           options = list(placeholder = tr("filters.all", lang())))
    })

    # ---- Filtrar y ordenar ----
    visible <- reactive({
      recs <- all_recordings()
      if (nrow(recs) == 0) return(recs)
      keep <- rep(TRUE, nrow(recs))
      if (!isTRUE(input$show_duplicates)) keep <- keep & is.na(recs$duplicate_of)
      if (length(input$dates) == 2 && !any(is.na(input$dates))) {
        keep <- keep & !is.na(recs$date_local) &
          recs$date_local >= input$dates[1] & recs$date_local <= input$dates[2]
      }
      if (length(input$hours) == 2) {
        keep <- keep & !is.na(recs$hour_local) &
          recs$hour_local >= input$hours[1] & recs$hour_local <= input$hours[2]
      }
      if (length(input$sites)) {
        site_key <- ifelse(is.na(recs$site_name), "", recs$site_name)
        keep <- keep & site_key %in% input$sites
      }
      if (length(input$recorders)) {
        keep <- keep & recorder_label(recs, lang()) %in% input$recorders
      }
      keep <- keep & recs$review_status %in% (input$status %||% character())
      recs <- recs[keep, , drop = FALSE]

      ord <- switch(input$sort %||% "date_asc",
        date_asc = order(recs$start_utc, recs$file_name),
        date_desc = order(recs$start_utc, recs$file_name, decreasing = TRUE),
        name = order(recs$file_name),
        status = order(match(recs$review_status, review_statuses), recs$start_utc)
      )
      recs[ord, , drop = FALSE]
    })

    output$count <- renderText({
      paste0(" (", tr("list.count", lang(), shown = nrow(visible()),
                      total = nrow(all_recordings())), ")")
    })

    # ---- Tabla ----
    # La tabla se dibuja de nuevo solo al cambiar de idioma (cabeceras). Los
    # cambios de datos se envían con updateReactable() para no perder la
    # página ni la selección.
    table_data <- reactive(build_table_data(visible(), lang()))
    shown_data <- NULL # datos que muestra la tabla en este momento

    output$table <- reactable::renderReactable({
      l <- lang()
      shown_data <<- isolate(table_data())
      track_table(shown_data, l, ns("open"))
    })

    # Identificadores de las filas marcadas con la casilla
    selected_ids <- function() {
      sel <- reactable::getReactableState("table", "selected")
      if (is.null(sel) || length(sel) == 0 || is.null(shown_data)) return(integer())
      shown_data$recording_id[sel[sel <= nrow(shown_data)]]
    }

    observeEvent(table_data(), {
      old_ids <- selected_ids()
      page <- reactable::getReactableState("table", "page") %||% 1
      new <- table_data()
      shown_data <<- new
      selected <- which(new$recording_id %in% old_ids)
      max_page <- max(1, ceiling(nrow(new) / page_size))
      reactable::updateReactable("table", data = new,
                                 selected = if (length(selected)) selected else NA,
                                 page = min(page, max_page))
    }, ignoreInit = TRUE)

    # Clic en una fila: abrir la pista en el visor
    observeEvent(input$open, {
      current(as.integer(input$open$id))
    })

    # Resaltar la pista abierta en la tabla
    observeEvent(current(), {
      session$sendCustomMessage("biosonora-current-track", list(id = current()))
    })

    # Al cargar datos por primera vez, abrir la primera pista visible
    observeEvent(visible(), {
      if (is.null(current()) && nrow(visible()) > 0) current(visible()$recording_id[1])
    })

    move <- function(step) {
      ids <- visible()$recording_id
      if (length(ids) == 0) return(invisible())
      pos <- match(current(), ids)
      new_pos <- if (is.na(pos)) 1 else pos + step
      if (new_pos < 1 || new_pos > length(ids)) {
        showNotification(tr(if (step > 0) "list.no_next" else "list.no_previous", lang()),
                         duration = 3)
        return(invisible())
      }
      current(ids[new_pos])
      reactable::updateReactable("table", page = ceiling(new_pos / page_size))
    }

    # ---- Asignar punto de muestreo a las pistas marcadas ----
    observeEvent(input$assign_site, {
      ids <- selected_ids()
      if (length(ids) == 0) {
        showNotification(tr("list.select_first", lang()), type = "warning")
        return()
      }
      l <- lang()
      sites <- db_get_sites(con)
      showModal(modalDialog(
        title = tr("site.modal_title", l, n = length(ids)),
        p(class = "text-muted", tr("site.modal_help", l)),
        selectizeInput(ns("site_name"), tr("site.name", l), choices = sites$name,
                       options = list(create = TRUE, placeholder = tr("site.name_placeholder", l))),
        div(class = "d-flex gap-2",
          numericInput(ns("site_lat"), tr("site.lat", l), value = NA, min = -90, max = 90, step = 0.00001),
          numericInput(ns("site_lon"), tr("site.lon", l), value = NA, min = -180, max = 180, step = 0.00001)
        ),
        numericInput(ns("site_accuracy"), tr("site.accuracy", l), value = 10, min = 0, step = 1),
        textInput(ns("observer"), tr("site.observer", l)),
        footer = tagList(
          modalButton(tr("common.cancel", l)),
          actionButton(ns("save_site"), tr("common.save", l), class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    # Al elegir un punto existente, rellenar sus coordenadas
    observeEvent(input$site_name, {
      sites <- db_get_sites(con)
      s <- sites[sites$name == input$site_name, ]
      if (nrow(s) == 1) {
        updateNumericInput(session, "site_lat", value = s$lat)
        updateNumericInput(session, "site_lon", value = s$lon)
        updateNumericInput(session, "site_accuracy", value = s$accuracy_m)
      }
    })

    observeEvent(input$save_site, {
      ids <- selected_ids()
      ok <- tryCatch({
        site_id <- db_save_site(con, input$site_name %||% "", input$site_lat,
                                input$site_lon, input$site_accuracy)
        observer <- trimws(input$observer %||% "")
        db_assign_site(con, ids, site_id, if (nzchar(observer)) observer else NULL)
        TRUE
      }, error = function(e) {
        showNotification(error_to_message(e, lang()), type = "error", duration = 8)
        FALSE
      })
      if (!ok) return()
      removeModal()
      data_changed(data_changed() + 1)
      showNotification(tr("site.saved", lang(), n = length(ids)), type = "message")
    })

    # ---- Indicar tipo y modelo de grabadora ----
    observeEvent(input$set_recorder, {
      ids <- selected_ids()
      if (length(ids) == 0) {
        showNotification(tr("list.select_first", lang()), type = "warning")
        return()
      }
      l <- lang()
      showModal(modalDialog(
        title = tr("recorder.modal_title", l, n = length(ids)),
        p(class = "text-muted", tr("recorder.modal_help", l)),
        radioButtons(ns("recorder_type"), tr("recorder.type", l),
                     choiceNames = lapply(c("audiomoth", "manual", "unknown"),
                                          function(t) tr(paste0("recorder.", t), l)),
                     choiceValues = c("audiomoth", "manual", "unknown"),
                     selected = "manual"),
        textInput(ns("recorder_model"), tr("recorder.model", l),
                  placeholder = tr("recorder.model_placeholder", l)),
        footer = tagList(
          modalButton(tr("common.cancel", l)),
          actionButton(ns("save_recorder"), tr("common.save", l), class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$save_recorder, {
      ids <- selected_ids()
      model <- trimws(input$recorder_model %||% "")
      ok <- tryCatch({
        db_set_recorder(con, ids, input$recorder_type, if (nzchar(model)) model else NA_character_)
        TRUE
      }, error = function(e) {
        showNotification(error_to_message(e, lang()), type = "error")
        FALSE
      })
      if (!ok) return()
      removeModal()
      data_changed(data_changed() + 1)
      showNotification(tr("recorder.saved", lang(), n = length(ids)), type = "message")
    })

    list(current = current, move = move)
  })
}

#' Texto que identifica la grabadora de cada pista (modelo + identificador)
#' @noRd
recorder_label <- function(recs, lang) {
  type <- vapply(recs$recorder_type, function(t) tr(paste0("recorder.", t %||% "unknown"), lang), "")
  model <- ifelse(is.na(recs$model), type, recs$model)
  ifelse(is.na(recs$device_id), model, paste(model, recs$device_id))
}

#' Prepara los datos de la tabla (con el HTML de estado y avisos ya generado)
#' @noRd
build_table_data <- function(recs, lang) {
  if (nrow(recs) == 0) {
    return(data.frame(recording_id = integer(), status = character(), file = character(),
                      date = character(), time = character(), duration = character(),
                      recorder = character(), rate = character(), site = character()))
  }
  status_icon <- c(unreviewed = "○", in_review = "◐", reviewed = "●")
  status_html <- sprintf(
    '<span class="bs-status bs-status-%s"><span aria-hidden="true">%s</span> %s</span>',
    recs$review_status, status_icon[recs$review_status],
    vapply(paste0("status.", recs$review_status), tr, "", lang = lang)
  )

  flags <- function(cond, key, symbol) {
    ifelse(cond, sprintf(' <span class="bs-flag" title="%s">%s</span>',
                         htmltools::htmlEscape(tr(key, lang), attribute = TRUE), symbol), "")
  }
  file_html <- paste0(
    with_tooltip(recs$file_name),
    flags(!recs$datetime_reliable, "list.flag_unreliable_date", "⚠"),
    flags(recs$name_modified, "list.flag_renamed", "✎"),
    flags(!is.na(recs$duplicate_of), "list.flag_duplicate", "⧉"),
    flags(!is.na(recs$error_key), "list.flag_error", "✖")
  )

  data.frame(
    recording_id = recs$recording_id,
    status = status_html,
    file = file_html,
    date = substr(recs$start_local, 1, 10),
    time = substr(recs$start_local, 12, 19),
    duration = format_duration(recs$duration_s),
    recorder = with_tooltip(recorder_label(recs, lang)),
    rate = ifelse(is.na(recs$sample_rate), "", paste(recs$sample_rate / 1000, "kHz")),
    site = with_tooltip(ifelse(is.na(recs$site_name), "", recs$site_name)),
    stringsAsFactors = FALSE
  )
}

#' Texto (escapado) con el contenido completo al pasar el ratón, porque las
#' celdas largas se cortan con "…"
#' @noRd
with_tooltip <- function(x) {
  sprintf('<span title="%s">%s</span>', htmltools::htmlEscape(x, attribute = TRUE),
          htmltools::htmlEscape(x))
}

#' Tabla de pistas (reactable)
#' @param open_input_id Id del input de Shiny que recibe el clic en una fila.
#' @noRd
track_table <- function(data, lang, open_input_id) {
  tz <- local_timezone()
  col <- function(key, ...) reactable::colDef(name = tr(key, lang), ...)
  reactable::reactable(
    data,
    selection = "multiple",
    onClick = reactable::JS(sprintf(
      "function(rowInfo, column) {
         if (column.id === '.selection') return;
         Shiny.setInputValue('%s', {id: rowInfo.original.recording_id, t: Date.now()},
                             {priority: 'event'});
       }", open_input_id)),
    rowClass = reactable::JS(
      "function(rowInfo) {
         return rowInfo && rowInfo.original.recording_id === window.biosonoraCurrentTrack
           ? 'bs-current-row' : '';
       }"),
    sortable = FALSE,
    # Las cabeceras pueden ocupar dos líneas; las celdas, una (con "…")
    defaultColDef = reactable::colDef(headerStyle = list(whiteSpace = "normal")),
    searchable = TRUE,
    highlight = TRUE,
    compact = TRUE,
    striped = TRUE,
    wrap = FALSE, # una línea por fila; el texto largo se corta con "…"
    defaultPageSize = 15,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(15, 30, 60, 120),
    columns = list(
      recording_id = reactable::colDef(show = FALSE),
      status = col("list.col_status", html = TRUE, minWidth = 115),
      file = col("list.col_file", html = TRUE, minWidth = 205),
      date = col("list.col_date", minWidth = 100),
      # La zona horaria se indica siempre junto a la hora
      time = reactable::colDef(
        name = tr("list.col_time", lang), minWidth = 90,
        header = function(value) htmltools::tagList(value, htmltools::tags$br(),
                                                    htmltools::tags$small(
                                                      class = "text-muted",
                                                      # Espacio invisible tras la barra: permite partir "Europe/Madrid"
                                                      gsub("/", "/\u200B", tz, fixed = TRUE)
                                                    ))
      ),
      duration = col("list.col_duration", minWidth = 80),
      recorder = col("list.col_recorder", html = TRUE, minWidth = 160),
      rate = col("list.col_rate", minWidth = 80),
      site = col("list.col_site", html = TRUE, minWidth = 90)
    ),
    language = reactable::reactableLang(
      searchPlaceholder = tr("list.search", lang),
      noData = tr("list.no_data", lang),
      pageInfo = tr("list.page_info", lang),
      pagePrevious = tr("list.page_previous", lang),
      pageNext = tr("list.page_next", lang),
      pageSizeOptions = tr("list.page_size", lang),
      selectAllRowsLabel = tr("list.select_all", lang),
      selectRowLabel = tr("list.select_row", lang)
    )
  )
}
