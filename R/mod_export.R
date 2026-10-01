#' Módulo de exportación
#'
#' Tres apartados: CSV para Observation.org (con comprobaciones y vista
#' previa), equivalencias taxonómicas y exportación completa de detecciones.
#'
#' @param id Identificador del módulo.
#' @noRd
mod_export_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(12, 12, 12),
    # ---- Observation.org ----
    bslib::card(
      bslib::card_header(icon("binoculars"), i18n("export.obs_title")),
      bslib::card_body(
        p(class = "text-muted small mb-2", i18n("export.obs_help")),
        div(
          class = "d-flex flex-wrap gap-3 align-items-end",
          dateRangeInput(ns("obs_dates"), i18n("filters.dates"), start = NULL, end = NULL,
                         weekstart = 1, separator = "–"),
          selectizeInput(ns("obs_sites"), i18n("filters.sites"), choices = NULL, multiple = TRUE),
          div(
            checkboxInput(ns("include_unverified"), i18n("export.include_unverified"), FALSE),
            conditionalPanel(
              sprintf("input['%s']", ns("include_unverified")),
              sliderInput(ns("unverified_conf"), i18n("export.unverified_threshold"), min = 50,
                          max = 100, value = 80, step = 5, post = " %", ticks = FALSE)
            )
          ),
          actionButton(ns("check"), i18n("export.check"), icon = icon("list-check"),
                       class = "btn-primary")
        ),
        uiOutput(ns("obs_result"))
      )
    ),
    # ---- Equivalencias taxonómicas ----
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        tags$span(icon("sitemap"), i18n("taxonomy.title")),
        actionButton(ns("edit_taxon"), i18n("taxonomy.edit"), icon = icon("pen"),
                     class = "btn-outline-primary btn-sm")
      ),
      bslib::card_body(
        p(class = "text-muted small mb-2", i18n("taxonomy.help")),
        reactable::reactableOutput(ns("taxonomy"))
      )
    ),
    # ---- Exportación completa ----
    bslib::card(
      bslib::card_header(icon("table"), i18n("export.all_title")),
      bslib::card_body(
        p(class = "text-muted small mb-2", i18n("export.all_help")),
        div(
          class = "d-flex flex-wrap gap-3 align-items-end",
          sliderInput(ns("all_conf"), i18n("filters.min_confidence"), min = 0, max = 100,
                      value = 10, step = 5, post = " %", ticks = FALSE),
          checkboxInput(ns("only_validated"), i18n("export.only_validated"), FALSE),
          downloadButton(ns("download_all_csv"), i18n("export.download_csv"), class = "btn-outline-primary"),
          downloadButton(ns("download_all_xlsx"), i18n("export.download_xlsx"), class = "btn-outline-primary")
        )
      )
    )
  )
}

#' @param con Conexión a la base de datos.
#' @param lang reactive con el idioma.
#' @param reviewer Resultado de mod_reviewer_server().
#' @param data_changed reactiveVal de cambios de datos.
#' @noRd
mod_export_server <- function(id, con, lang, reviewer, data_changed) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tz <- local_timezone()
    taxonomy_changed <- reactiveVal(0)
    result <- reactiveVal(NULL)

    # Opciones de los filtros
    observe({
      data_changed()
      sites <- db_get_sites(con)
      updateSelectizeInput(session, "obs_sites", choices = stats::setNames(sites$site_id, sites$name),
                           selected = isolate(input$obs_sites),
                           options = list(placeholder = tr("filters.all", lang())))
    })
    # Rango de fechas inicial: todas las fechas con detecciones
    last_range <- reactiveVal(NULL)
    observe({
      data_changed()
      dates <- DBI::dbGetQuery(con, "
        SELECT MIN(r.start_utc) AS a, MAX(r.start_utc) AS b FROM recordings r
        JOIN detections d ON d.recording_id = r.recording_id")
      if (is.na(dates$a)) return()
      rng <- as.Date(format(parse_utc(c(dates$a, dates$b)), tz = tz, "%Y-%m-%d"))
      if (!identical(rng, last_range())) {
        updateDateRangeInput(session, "obs_dates", start = rng[1], end = rng[2])
        last_range(rng)
      }
    })

    options <- function() {
      dates <- input$obs_dates
      list(
        include_unverified = isTRUE(input$include_unverified),
        min_confidence = (input$unverified_conf %||% 80) / 100,
        date_from = if (length(dates) == 2 && !is.na(dates[1])) format(dates[1]) else NULL,
        date_to = if (length(dates) == 2 && !is.na(dates[2])) format(dates[2]) else NULL,
        site_ids = as.integer(input$obs_sites)
      )
    }

    # ---- Comprobar y previsualizar ----
    observeEvent(input$check, {
      result(tryCatch(build_observation_export(con, options(), tz), error = function(e) {
        showNotification(error_to_message(e, lang()), type = "error")
        NULL
      }))
    })
    # Si cambian datos o equivalencias, la vista previa deja de ser válida
    observeEvent(list(data_changed(), taxonomy_changed()), result(NULL), ignoreInit = TRUE)

    output$obs_result <- renderUI({
      r <- result()
      l <- lang()
      if (is.null(r)) return(p(class = "text-muted small mt-2", i18n("export.press_check")))
      blocking <- nrow(r$unmatched) > 0 || nrow(r$missing_coords) > 0
      problems <- tagList(
        if (nrow(r$missing_coords)) div(
          class = "alert alert-danger py-2 small",
          icon("location-dot"), tags$strong(tr("export.missing_coords", l, n = nrow(r$missing_coords))),
          tags$br(), paste(utils::head(r$missing_coords$file_name, 8), collapse = ", "),
          if (nrow(r$missing_coords) > 8) " …"
        ),
        if (nrow(r$unmatched)) div(
          class = "alert alert-danger py-2 small",
          icon("sitemap"), tags$strong(tr("export.unmatched", l, n = nrow(r$unmatched))),
          tags$br(), tags$em(paste(r$unmatched$source_name, collapse = ", "))
        ),
        if (nrow(r$unreliable)) div(
          class = "alert alert-warning py-2 small",
          icon("triangle-exclamation"), tr("export.unreliable_dates", l, n = nrow(r$unreliable))
        ),
        if (r$n_unverified > 0) div(
          class = "alert alert-warning py-2 small",
          icon("triangle-exclamation"), tr("export.unverified_warning", l, n = r$n_unverified)
        )
      )
      tagList(
        div(class = "mt-2", problems),
        p(class = "small mb-1",
          tr("export.summary", l, rows = nrow(r$rows), detections = r$n_detections)),
        if (nrow(r$rows)) reactable::reactable(
          r$rows, compact = TRUE, striped = TRUE, defaultPageSize = 10, wrap = FALSE,
          defaultColDef = reactable::colDef(minWidth = 90, headerStyle = list(whiteSpace = "normal")),
          columns = list(`scientific name` = reactable::colDef(minWidth = 170),
                         notes = reactable::colDef(minWidth = 170))
        ),
        div(class = "mt-2",
            if (blocking) p(class = "text-danger small", icon("ban"), tr("export.blocked", l))
            else if (nrow(r$rows)) downloadButton(ns("download_obs"), tr("export.download_obs", l),
                                                  class = "btn-success"))
      )
    })

    output$download_obs <- downloadHandler(
      filename = function() paste0("observation_org_", format(Sys.Date()), ".csv"),
      content = function(file) {
        # Se recalcula al descargar para exportar siempre los datos actuales
        r <- build_observation_export(con, options(), tz)
        write_observation_csv(r$rows, file)
      }
    )

    # ---- Equivalencias taxonómicas ----
    taxonomy <- reactive({
      data_changed()
      taxonomy_changed()
      names <- DBI::dbGetQuery(con, "
        SELECT DISTINCT COALESCE(corrected_scientific_name, scientific_name) AS name
        FROM detections WHERE category = 'species' AND validation != 'incorrect'")$name
      tax <- resolve_taxonomy(con, names)
      tax[order(tax$method != "none", tax$source_name), , drop = FALSE]
    })

    output$taxonomy <- reactable::renderReactable({
      tax <- taxonomy()
      l <- lang()
      data <- data.frame(
        source = tax$source_name,
        target = ifelse(is.na(tax$target_name), sprintf('<span class="text-danger">✖ %s</span>',
                                                        tr("taxonomy.none", l)),
                        sprintf("<em>%s</em>", htmltools::htmlEscape(tax$target_name))),
        common = ifelse(is.na(tax$spanish), "", if (l == "es") tax$spanish else tax$english),
        method = vapply(paste0("taxonomy.method_", tax$method), tr, "", lang = l),
        stringsAsFactors = FALSE
      )
      reactable::reactable(
        data, compact = TRUE, striped = TRUE, searchable = TRUE, defaultPageSize = 10,
        selection = "single", onClick = "select", wrap = FALSE,
        columns = list(
          source = reactable::colDef(name = tr("taxonomy.col_source", l), style = list(fontStyle = "italic")),
          target = reactable::colDef(name = tr("taxonomy.col_target", l), html = TRUE),
          common = reactable::colDef(name = tr("taxonomy.col_common", l)),
          method = reactable::colDef(name = tr("taxonomy.col_method", l))
        ),
        language = reactable::reactableLang(searchPlaceholder = tr("list.search", l),
                                            noData = tr("taxonomy.empty", l))
      )
    })

    observeEvent(input$edit_taxon, {
      sel <- reactable::getReactableState("taxonomy", "selected")
      tax <- taxonomy()
      l <- lang()
      if (is.null(sel) || !length(sel)) {
        showNotification(tr("taxonomy.select_first", l), type = "warning")
        return()
      }
      row <- tax[sel, ]
      showModal(modalDialog(
        title = tr("taxonomy.edit_title", l, name = row$source_name),
        p(class = "text-muted small", tr("taxonomy.edit_help", l)),
        selectizeInput(ns("taxon_target"), tr("taxonomy.col_target", l), choices = NULL,
                       width = "100%"),
        footer = tagList(
          actionButton(ns("taxon_reset"), tr("taxonomy.reset", l), class = "btn-outline-secondary"),
          modalButton(tr("common.cancel", l)),
          actionButton(ns("taxon_save"), tr("common.save", l), class = "btn-primary")
        ),
        easyClose = TRUE
      ))
      ioc <- ioc_species()
      labels <- paste0(ioc$ioc_name, " — ", if (l == "es") ioc$spanish else ioc$english)
      updateSelectizeInput(session, "taxon_target", choices = stats::setNames(ioc$ioc_name, labels),
                           selected = row$target_name %||% "", server = TRUE)
    })

    save_taxon <- function(target) {
      sel <- reactable::getReactableState("taxonomy", "selected")
      row <- taxonomy()[sel, ]
      user <- reviewer$require()
      if (is.null(user)) return()
      db_set_taxon_map(con, row$source_name, target, user)
      removeModal()
      taxonomy_changed(taxonomy_changed() + 1)
    }
    observeEvent(input$taxon_save, save_taxon(input$taxon_target))
    observeEvent(input$taxon_reset, save_taxon(NULL))

    # ---- Exportación completa ----
    all_options <- function() list(min_confidence = (input$all_conf %||% 10) / 100,
                                   only_validated = isTRUE(input$only_validated))
    output$download_all_csv <- downloadHandler(
      filename = function() paste0("biosonora_detecciones_", format(Sys.Date()), ".csv"),
      content = function(file) {
        write_csv_utf8(build_detections_export(con, all_options(), lang(), tz), file)
      }
    )
    output$download_all_xlsx <- downloadHandler(
      filename = function() paste0("biosonora_detecciones_", format(Sys.Date()), ".xlsx"),
      content = function(file) {
        writexl::write_xlsx(build_detections_export(con, all_options(), lang(), tz), file)
      }
    )
  })
}
