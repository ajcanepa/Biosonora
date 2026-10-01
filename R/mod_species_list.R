#' Módulo del listado de especies detectadas
#'
#' Filtros: umbral de confianza, puntos de muestreo, fechas y grabaciones.
#' Al exportar a CSV, el listado también se guarda en la base de datos.
#'
#' @param id Identificador del módulo.
#' @noRd
mod_species_list_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 290,
      h5(class = "mb-1", i18n("species.filters")),
      sliderInput(ns("min_conf"), tagList(i18n("species.min_confidence"), help_icon("help.min_confidence")),
                  min = 0, max = 100,
                  value = 50, step = 5, post = " %", ticks = FALSE, width = "100%"),
      selectizeInput(ns("sites"), i18n("filters.sites"), choices = NULL, multiple = TRUE,
                     width = "100%"),
      dateRangeInput(ns("dates"), i18n("filters.dates"), start = NULL, end = NULL,
                     weekstart = 1, separator = "–", width = "100%"),
      selectizeInput(ns("recordings"), i18n("species.recordings"), choices = NULL,
                     multiple = TRUE, width = "100%"),
      p(class = "text-muted small", i18n("species.help"))
    ),
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        tags$span(icon("list-check"), i18n("species.title"), textOutput(ns("count"), inline = TRUE)),
        downloadButton(ns("export"), i18n("species.export"), class = "btn-primary btn-sm")
      ),
      bslib::card_body(
        uiOutput(ns("saved_info")),
        reactable::reactableOutput(ns("table"))
      )
    )
  )
}

#' @param con Conexión a la base de datos.
#' @param lang reactive con el idioma.
#' @param reviewer Resultado de mod_reviewer_server().
#' @param data_changed reactiveVal de cambios de datos.
#' @noRd
mod_species_list_server <- function(id, con, lang, reviewer, data_changed) {
  moduleServer(id, function(input, output, session) {
    tz <- local_timezone()
    saved_changed <- reactiveVal(0)

    # Rango de fechas: todas las fechas con detecciones (si no, el control
    # tomaría por defecto el día de hoy y el listado saldría vacío)
    last_range <- reactiveVal(NULL)
    observe({
      data_changed()
      dates <- DBI::dbGetQuery(con, "
        SELECT DISTINCT r.start_utc FROM recordings r
        JOIN detections d ON d.recording_id = r.recording_id")$start_utc
      dates <- as.Date(format(parse_utc(dates), tz = tz, "%Y-%m-%d"))
      dates <- dates[!is.na(dates)]
      if (length(dates) == 0) return()
      rng <- range(dates)
      if (!identical(rng, last_range())) {
        updateDateRangeInput(session, "dates", start = rng[1], end = rng[2])
        last_range(rng)
      }
    })

    # Opciones de los filtros
    observe({
      data_changed()
      l <- lang()
      sites <- db_get_sites(con)
      updateSelectizeInput(session, "sites", choices = stats::setNames(sites$site_id, sites$name),
                           selected = isolate(input$sites),
                           options = list(placeholder = tr("filters.all", l)))
      recs <- DBI::dbGetQuery(con, "
        SELECT DISTINCT r.recording_id, r.file_name FROM recordings r
        JOIN detections d ON d.recording_id = r.recording_id ORDER BY r.file_name")
      updateSelectizeInput(session, "recordings",
                           choices = stats::setNames(recs$recording_id, recs$file_name),
                           selected = isolate(input$recordings), server = TRUE,
                           options = list(placeholder = tr("filters.all", l)))
    })

    filters <- reactive({
      dates <- input$dates
      list(
        min_confidence = (input$min_conf %||% 50) / 100,
        site_ids = as.integer(input$sites),
        recording_ids = as.integer(input$recordings),
        date_from = if (length(dates) == 2 && !is.na(dates[1])) format(dates[1]) else NULL,
        date_to = if (length(dates) == 2 && !is.na(dates[2])) format(dates[2]) else NULL
      )
    }) |> debounce(300)

    species <- reactive({
      data_changed()
      compute_species_list(con, filters(), tz)
    })

    output$count <- renderText(paste0(" (", nrow(species()), ")"))

    output$table <- reactable::renderReactable({
      species_table(species(), lang())
    })

    output$saved_info <- renderUI({
      saved_changed()
      last <- DBI::dbGetQuery(con, "
        SELECT generated_at, generated_by, (SELECT COUNT(*) FROM species_lists) AS n
        FROM species_lists ORDER BY list_id DESC LIMIT 1")
      if (nrow(last) == 0) return(NULL)
      p(class = "text-muted small mb-2",
        tr("species.saved_info", lang(), n = last$n,
           date = format(parse_utc(last$generated_at), tz = tz, "%Y-%m-%d %H:%M"),
           user = last$generated_by %||% "—"))
    })

    output$export <- downloadHandler(
      filename = function() paste0("biosonora_especies_", format(Sys.Date()), ".csv"),
      content = function(file) {
        sp <- species()
        db_save_species_list(con, sp, filters(), reviewer$name())
        saved_changed(saved_changed() + 1)
        write_csv_utf8(species_list_for_export(sp, lang()), file)
      }
    )
  })
}

#' Escribe un CSV en UTF-8 con marca BOM (para que Excel lea bien las tildes)
#' @noRd
write_csv_utf8 <- function(df, file) {
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), file)
  # La conexión convierte el texto a UTF-8 en cualquier sistema (también Windows)
  con <- file(file, open = "a", encoding = "UTF-8")
  on.exit(close(con))
  utils::write.csv(df, con, row.names = FALSE, na = "")
}

#' Tabla del listado de especies
#' @noRd
species_table <- function(sp, lang) {
  status_symbol <- c(validated = "✓", doubtful = "?", pending = "○")
  data <- data.frame(
    status = sprintf('<span class="bs-val bs-species-%s">%s %s</span>', sp$status,
                     status_symbol[sp$status],
                     vapply(paste0("species_status.", sp$status), tr, "", lang = lang)),
    common_name = ifelse(is.na(sp$common_name), "", sp$common_name),
    scientific_name = sp$scientific_name,
    max_confidence = sp$max_confidence,
    mean_confidence = sp$mean_confidence,
    n_detections = sp$n_detections,
    n_recordings = sp$n_recordings,
    validation = sprintf("%d ✓ · %d ? · %d ○", sp$n_correct, sp$n_doubtful, sp$n_pending),
    stringsAsFactors = FALSE
  )
  pct <- function(value) if (is.na(value)) "—" else sprintf("%.0f%%", 100 * value)
  col <- function(key, ...) reactable::colDef(name = tr(key, lang), ...)
  reactable::reactable(
    data, compact = TRUE, striped = TRUE, highlight = TRUE, searchable = TRUE,
    defaultPageSize = 25, wrap = FALSE,
    defaultColDef = reactable::colDef(headerStyle = list(whiteSpace = "normal")),
    columns = list(
      status = col("species.col_status", html = TRUE, minWidth = 120),
      common_name = col("species.col_common", minWidth = 160),
      scientific_name = col("species.col_scientific", minWidth = 170,
                            style = list(fontStyle = "italic")),
      max_confidence = col("species.col_max", cell = pct, minWidth = 90, align = "right"),
      mean_confidence = col("species.col_mean", cell = pct, minWidth = 90, align = "right"),
      n_detections = col("species.col_detections", minWidth = 90, align = "right"),
      n_recordings = col("species.col_recordings", minWidth = 90, align = "right"),
      validation = col("species.col_validation", minWidth = 140, sortable = FALSE)
    ),
    language = reactable::reactableLang(
      searchPlaceholder = tr("list.search", lang), noData = tr("species.none", lang),
      pageInfo = tr("list.page_info", lang), pagePrevious = tr("list.page_previous", lang),
      pageNext = tr("list.page_next", lang))
  )
}
