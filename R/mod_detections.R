#' Módulo de detecciones: revisar y anotar la grabación abierta
#'
#' Muestra las detecciones (BirdNET y manuales) de la pista abierta, permite
#' validarlas, corregir la especie, añadir notas, marcar voz humana y crear
#' anotaciones dibujando un rectángulo sobre el espectrograma. Envía los
#' rectángulos al visor (viewer.js).
#'
#' @param id Identificador del módulo.
#' @noRd
mod_detections_ui <- function(id) {
  ns <- NS(id)
  btn <- function(input_id, icon_name, key, class, shortcut = NULL) {
    label <- if (is.null(shortcut)) i18n(key) else tagList(i18n(key), tags$kbd(class = "ms-1", shortcut))
    actionButton(ns(input_id), label, icon = icon(icon_name), class = paste("btn-sm", class))
  }
  bslib::card(
    class = "bs-detections-card",
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
      tags$span(icon("feather"), i18n("detections.title"), textOutput(ns("count"), inline = TRUE)),
      btn("annotate", "pen-to-square", "detections.new_annotation", "btn-primary", "A")
    ),
    bslib::card_body(
      div(
        class = "d-flex flex-wrap gap-1 mb-2 bs-detection-actions",
        btn("correct", "check", "detections.correct", "btn-outline-success", "V"),
        btn("incorrect", "xmark", "detections.incorrect", "btn-outline-danger", "X"),
        btn("doubtful", "question", "detections.doubtful", "btn-outline-warning", "D"),
        btn("play", "play", "detections.play_fragment", "btn-outline-secondary", "F"),
        btn("species", "pen", "detections.change_species", "btn-outline-secondary"),
        btn("notes", "note-sticky", "detections.notes", "btn-outline-secondary"),
        btn("human", "user-shield", "detections.toggle_human", "btn-outline-secondary"),
        btn("delete", "trash", "detections.delete", "btn-outline-secondary")
      ),
      uiOutput(ns("selected_info")),
      reactable::reactableOutput(ns("table"))
    )
  )
}

#' @param con Conexión a la base de datos.
#' @param lang reactive con el idioma.
#' @param current reactive con la grabación abierta.
#' @param min_confidence reactive con el umbral de confianza (0-1).
#' @param reviewer Resultado de mod_reviewer_server().
#' @param data_changed reactiveVal de cambios de datos.
#' @param player_id Id del visor (para enviarle los rectángulos).
#' @param events Lista de reactives con eventos del navegador: key, box_click,
#'   new_box.
#' @noRd
mod_detections_server <- function(id, con, lang, current, min_confidence, reviewer,
                                  data_changed, player_id, events) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected <- reactiveVal(NULL)
    new_box <- reactiveVal(NULL)

    detections <- reactive({
      req(current())
      data_changed()
      db_get_detections(con, current(), min_confidence())
    })

    # Al cambiar de grabación, seleccionar la primera detección sin revisar
    observeEvent(current(), {
      det <- isolate(detections())
      pending <- det$detection_id[det$validation == "pending"]
      selected(if (length(pending)) pending[1] else if (nrow(det)) det$detection_id[1] else NULL)
    })

    selected_detection <- reactive({
      id <- selected()
      det <- detections()
      if (is.null(id) || !id %in% det$detection_id) return(NULL)
      as.list(det[det$detection_id == id, ])
    })

    output$count <- renderText({
      det <- detections()
      paste0(" (", tr("detections.count", lang(), n = nrow(det),
                      pending = sum(det$validation == "pending")), ")")
    })

    # ---- Rectángulos sobre el espectrograma ----
    observe({
      det <- detections()
      l <- lang()
      sel <- selected()
      items <- lapply(seq_len(nrow(det)), function(i) {
        d <- det[i, ]
        list(
          id = d$detection_id, start = d$start_s, end = d$end_s,
          low = d$low_hz, high = d$high_hz,
          label = detection_label(d, l), state = d$validation,
          category = d$category, source = d$source,
          selected = identical(as.integer(sel), as.integer(d$detection_id))
        )
      })
      session$sendCustomMessage("biosonora-viewer-detections",
                                list(id = player_id, items = items))
    })

    # ---- Tabla ----
    output$table <- reactable::renderReactable({
      det <- detections()
      detections_table(det, lang(), ns("row_click"), selected())
    })

    observeEvent(input$row_click, selected(as.integer(input$row_click$id)))
    observeEvent(events$box_click(), selected(as.integer(events$box_click()$id)))

    output$selected_info <- renderUI({
      d <- selected_detection()
      l <- lang()
      if (is.null(d)) {
        return(p(class = "text-muted small mb-2",
                 if (nrow(detections()) == 0) i18n("detections.none") else i18n("detections.select_hint")))
      }
      history <- db_get_history(con, d$detection_id)
      div(
        class = "bs-selected-detection small mb-2",
        tags$strong(detection_label(d, l, with_confidence = FALSE)), " ",
        tags$em(d$species), " · ",
        sprintf("%s–%s", format_duration_precise(d$start_s), format_duration_precise(d$end_s)),
        if (!is.na(d$notes)) tagList(" · ", icon("note-sticky"), " ", d$notes),
        if (nrow(history)) {
          last <- history[nrow(history), ]
          tags$span(class = "text-muted ms-2",
                    tr("detections.last_change", l, user = last$user %||% "—",
                       date = format(parse_utc(last$at), tz = local_timezone(), "%Y-%m-%d %H:%M")))
        }
      )
    })

    # ---- Acciones ----
    with_selection <- function(fn) {
      d <- isolate(selected_detection())
      if (is.null(d)) {
        showNotification(tr("detections.select_first", lang()), type = "warning")
        return(invisible())
      }
      user <- reviewer$require()
      if (is.null(user)) return(invisible())
      tryCatch(fn(d, user), error = function(e) {
        showNotification(error_to_message(e, lang()), type = "error")
      })
    }

    validate <- function(state) {
      with_selection(function(d, user) {
        db_validate_detection(con, d$detection_id, state, user)
        data_changed(data_changed() + 1)
        select_next_pending(d$detection_id)
      })
    }

    # Tras validar, pasar a la siguiente detección sin revisar
    select_next_pending <- function(from_id) {
      det <- isolate(detections())
      pos <- match(from_id, det$detection_id)
      after <- det[seq_len(nrow(det)) > pos & det$validation == "pending", ]
      if (nrow(after)) selected(after$detection_id[1])
    }

    observeEvent(input$correct, validate("correct"))
    observeEvent(input$incorrect, validate("incorrect"))
    observeEvent(input$doubtful, validate("doubtful"))

    play_fragment <- function() {
      d <- isolate(selected_detection())
      if (is.null(d)) return()
      session$sendCustomMessage("biosonora-viewer-play-range",
                                list(id = player_id, start = d$start_s, end = d$end_s))
    }
    observeEvent(input$play, play_fragment())

    move_selection <- function(step) {
      det <- isolate(detections())
      if (nrow(det) == 0) return()
      pos <- match(isolate(selected()), det$detection_id)
      new_pos <- if (is.na(pos)) 1 else min(max(1, pos + step), nrow(det))
      selected(det$detection_id[new_pos])
    }

    observeEvent(events$key(), {
      key <- events$key()$key
      switch(tolower(key),
        v = validate("correct"),
        x = validate("incorrect"),
        d = validate("doubtful"),
        f = play_fragment(),
        a = start_annotation(),
        arrowdown = move_selection(1),
        arrowup = move_selection(-1),
        NULL
      )
    })

    # ---- Corregir especie ----
    species_choices <- function() {
      known <- db_known_species(con)
      labels <- ifelse(is.na(known$common_name), known$scientific_name,
                       paste0(known$scientific_name, " — ", known$common_name))
      stats::setNames(known$scientific_name, labels)
    }

    species_inputs <- function(l, selected_name = NULL, common = NULL) {
      tagList(
        selectizeInput(ns("species_name"), tr("detections.scientific_name", l),
                       choices = species_choices(), selected = selected_name,
                       options = list(create = TRUE, placeholder = tr("detections.species_placeholder", l))),
        textInput(ns("species_common"), tr("detections.common_name", l), value = common %||% "")
      )
    }

    # Al elegir una especie conocida, rellenar su nombre común
    observeEvent(input$species_name, {
      known <- db_known_species(con)
      common <- known$common_name[known$scientific_name == input$species_name]
      if (length(common) && !is.na(common[1])) updateTextInput(session, "species_common", value = common[1])
    })

    observeEvent(input$species, {
      d <- isolate(selected_detection())
      if (is.null(d)) {
        showNotification(tr("detections.select_first", lang()), type = "warning")
        return()
      }
      l <- lang()
      showModal(modalDialog(
        title = tr("detections.change_species", l),
        p(class = "text-muted", tr("detections.original_species", l,
                                   species = paste(d$scientific_name, d$common_name %||% ""))),
        species_inputs(l, d$species, d$species_common),
        footer = tagList(modalButton(tr("common.cancel", l)),
                         actionButton(ns("save_species"), tr("common.save", l), class = "btn-primary")),
        easyClose = TRUE
      ))
    })

    observeEvent(input$save_species, {
      with_selection(function(d, user) {
        db_correct_species(con, d$detection_id, input$species_name,
                           trimws(input$species_common %||% ""), user)
        removeModal()
        data_changed(data_changed() + 1)
      })
    })

    # ---- Notas ----
    observeEvent(input$notes, {
      d <- isolate(selected_detection())
      if (is.null(d)) {
        showNotification(tr("detections.select_first", lang()), type = "warning")
        return()
      }
      l <- lang()
      showModal(modalDialog(
        title = tr("detections.notes", l),
        textAreaInput(ns("notes_text"), NULL, value = if (is.na(d$notes)) "" else d$notes,
                      rows = 4, width = "100%"),
        footer = tagList(modalButton(tr("common.cancel", l)),
                         actionButton(ns("save_notes"), tr("common.save", l), class = "btn-primary")),
        easyClose = TRUE
      ))
    })

    observeEvent(input$save_notes, {
      with_selection(function(d, user) {
        db_set_detection_notes(con, d$detection_id, input$notes_text, user)
        removeModal()
        data_changed(data_changed() + 1)
      })
    })

    # ---- Voz humana (privada) ----
    observeEvent(input$human, {
      with_selection(function(d, user) {
        new_category <- if (d$category == "human") "species" else "human"
        db_set_detection_category(con, d$detection_id, new_category, user)
        data_changed(data_changed() + 1)
        showNotification(tr(if (new_category == "human") "detections.marked_human"
                            else "detections.unmarked_human", lang()), type = "message")
      })
    })

    # ---- Borrar (solo anotaciones manuales) ----
    observeEvent(input$delete, {
      with_selection(function(d, user) {
        db_delete_manual_detection(con, d$detection_id, user)
        selected(NULL)
        data_changed(data_changed() + 1)
      })
    })

    # ---- Nueva anotación: el usuario dibuja un rectángulo ----
    start_annotation <- function() {
      if (is.null(isolate(current()))) return()
      if (is.null(reviewer$require())) return()
      session$sendCustomMessage("biosonora-viewer-draw", list(id = player_id))
      showNotification(tr("detections.draw_hint", lang()), duration = 6, id = ns("draw_hint"))
    }
    observeEvent(input$annotate, start_annotation())

    observeEvent(events$new_box(), {
      box <- events$new_box()
      new_box(box)
      removeNotification(ns("draw_hint"))
      l <- lang()
      showModal(modalDialog(
        title = tr("detections.new_annotation", l),
        p(class = "text-muted small",
          tr("detections.box_summary", l,
             start = format_duration_precise(box$start_s), end = format_duration_precise(box$end_s),
             low = round(box$low_hz / 1000, 1), high = round(box$high_hz / 1000, 1))),
        checkboxInput(ns("box_human"), tr("detections.is_human", l), value = FALSE),
        species_inputs(l),
        textAreaInput(ns("box_notes"), tr("detections.notes", l), rows = 2, width = "100%"),
        footer = tagList(modalButton(tr("common.cancel", l)),
                         actionButton(ns("save_box"), tr("common.save", l), class = "btn-primary")),
        easyClose = FALSE
      ))
    })

    observeEvent(input$save_box, {
      box <- new_box()
      user <- reviewer$require()
      if (is.null(box) || is.null(user)) return()
      ok <- tryCatch({
        human <- isTRUE(input$box_human)
        id <- db_add_manual_detection(
          con, isolate(current()), box$start_s, box$end_s, box$low_hz, box$high_hz,
          scientific_name = if (human) "Human vocal" else input$species_name,
          common_name = if (human) NA_character_ else trimws(input$species_common %||% ""),
          category = if (human) "human" else "species",
          notes = input$box_notes, user = user)
        selected(id)
        TRUE
      }, error = function(e) {
        showNotification(error_to_message(e, lang()), type = "error")
        FALSE
      })
      if (!ok) return()
      removeModal()
      new_box(NULL)
      data_changed(data_changed() + 1)
    })
  })
}

#' Texto de una detección: nombre común (o científico), confianza y estado
#' @noRd
detection_label <- function(d, lang, with_confidence = TRUE) {
  name <- if (d$category == "human") tr("detections.human_voice", lang)
          else if (!is.na(d$species_common) && nzchar(d$species_common)) d$species_common
          else d$species
  conf <- if (with_confidence && !is.na(d$confidence)) sprintf(" %d%%", round(100 * d$confidence)) else ""
  paste0(validation_symbol(d$validation), " ", name, conf)
}

#' Símbolo de cada estado de validación (la información nunca va solo en el color)
#' @noRd
validation_symbol <- function(state) {
  unname(c(pending = "○", correct = "✓", incorrect = "✗", doubtful = "?")[state])
}

#' Tiempo con décimas: 1:03.5
#' @noRd
format_duration_precise <- function(seconds) {
  sprintf("%d:%04.1f", as.integer(seconds %/% 60), seconds %% 60)
}

#' Tabla de detecciones de la grabación abierta
#' @noRd
detections_table <- function(det, lang, click_input_id, selected_id) {
  if (nrow(det) == 0) {
    return(reactable::reactable(
      data.frame(x = character()), columns = list(x = reactable::colDef(name = "")),
      language = reactable::reactableLang(noData = tr("detections.none", lang))))
  }
  data <- data.frame(
    detection_id = det$detection_id,
    state = sprintf('<span class="bs-val bs-val-%s">%s %s</span>', det$validation,
                    validation_symbol(det$validation),
                    vapply(paste0("validation.", det$validation), tr, "", lang = lang)),
    species = ifelse(det$category == "human",
                     sprintf('<span class="bs-human">%s</span>', tr("detections.human_voice", lang)),
                     sprintf("%s <em class=\"text-muted\">%s</em>%s",
                             htmltools::htmlEscape(ifelse(is.na(det$species_common), "", det$species_common)),
                             htmltools::htmlEscape(det$species),
                             ifelse(is.na(det$corrected_scientific_name), "",
                                    sprintf(' <span class="badge text-bg-light" title="%s">✎</span>',
                                            htmltools::htmlEscape(tr("detections.corrected_from", lang,
                                                                     species = det$scientific_name), attribute = TRUE))))),
    confidence = ifelse(is.na(det$confidence), "—", sprintf("%d%%", round(100 * det$confidence))),
    time = sprintf("%s–%s", format_duration_precise(det$start_s), format_duration_precise(det$end_s)),
    source = vapply(paste0("source.", det$source), tr, "", lang = lang),
    reviewer = ifelse(is.na(det$validated_by), "", det$validated_by),
    stringsAsFactors = FALSE
  )
  col <- function(key, ...) reactable::colDef(name = tr(key, lang), ...)
  reactable::reactable(
    data,
    pagination = FALSE, height = if (nrow(data) > 8) 300 else "auto",
    compact = TRUE, highlight = TRUE, sortable = FALSE, wrap = FALSE,
    onClick = reactable::JS(sprintf(
      "function(rowInfo) { Shiny.setInputValue('%s', {id: rowInfo.original.detection_id, t: Date.now()}, {priority: 'event'}); }",
      click_input_id)),
    rowClass = function(index) {
      if (!is.null(selected_id) && identical(as.integer(data$detection_id[index]), as.integer(selected_id))) "bs-current-row"
    },
    columns = list(
      detection_id = reactable::colDef(show = FALSE),
      state = col("detections.col_state", html = TRUE, minWidth = 110),
      species = col("detections.col_species", html = TRUE, minWidth = 240),
      confidence = col("detections.col_confidence", minWidth = 80, align = "right"),
      time = col("detections.col_time", minWidth = 110),
      source = col("detections.col_source", minWidth = 80),
      reviewer = col("detections.col_reviewer", minWidth = 100)
    ),
    language = reactable::reactableLang(noData = tr("detections.none", lang))
  )
}
