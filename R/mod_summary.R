#' Módulo de resúmenes gráficos
#'
#' Especies por punto de muestreo, actividad por hora del día y curva de
#' acumulación de especies, con filtros y descarga en PNG.
#'
#' @param id Identificador del módulo.
#' @noRd
mod_summary_ui <- function(id) {
  ns <- NS(id)
  plot_card <- function(output_id, title_key, help_key) {
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span(i18n(title_key), help_icon(help_key)),
        downloadButton(ns(paste0("png_", output_id)), "PNG", class = "btn-outline-secondary btn-sm",
                       `aria-label` = tr("summary.download_png"))
      ),
      # "plot_" delante: el id no puede coincidir con el de un filtro (p. ej. "sites")
      bslib::card_body(plotOutput(ns(paste0("plot_", output_id)), height = "340px"))
    )
  }
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 290,
      h5(class = "mb-1", i18n("summary.filters")),
      sliderInput(ns("min_conf"), label = tagList(i18n("filters.min_confidence"),
                                                  help_icon("help.min_confidence")),
                  min = 0, max = 100, value = 50, step = 5, post = " %", ticks = FALSE,
                  width = "100%"),
      checkboxInput(ns("only_validated"), i18n("export.only_validated"), FALSE),
      selectizeInput(ns("sites"), i18n("filters.sites"), choices = NULL, multiple = TRUE,
                     width = "100%"),
      dateRangeInput(ns("dates"), i18n("filters.dates"), start = NULL, end = NULL,
                     weekstart = 1, separator = "–", width = "100%"),
      radioButtons(ns("palette"), label = tagList(i18n("summary.palette"), help_icon("help.chart_palette")),
                   choiceNames = list("Okabe-Ito", "Viridis", "Cividis"),
                   choiceValues = c("okabe_ito", "viridis", "cividis"))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6, 12),
      plot_card("sites", "summary.sites_title", "help.sites_plot"),
      plot_card("accumulation", "summary.accumulation_title", "help.accumulation_plot"),
      plot_card("hours", "summary.hours_title", "help.hours_plot")
    )
  )
}

#' @param con Conexión a la base de datos.
#' @param lang reactive con el idioma.
#' @param dark reactive: TRUE en modo oscuro.
#' @param data_changed reactiveVal de cambios de datos.
#' @noRd
mod_summary_server <- function(id, con, lang, dark, data_changed) {
  moduleServer(id, function(input, output, session) {
    tz <- local_timezone()

    observe({
      data_changed()
      sites <- db_get_sites(con)
      updateSelectizeInput(session, "sites", choices = stats::setNames(sites$site_id, sites$name),
                           selected = isolate(input$sites),
                           options = list(placeholder = tr("filters.all", lang())))
    })
    last_range <- reactiveVal(NULL)
    observe({
      data_changed()
      dates <- DBI::dbGetQuery(con, "
        SELECT MIN(r.start_utc) AS a, MAX(r.start_utc) AS b FROM recordings r
        JOIN detections d ON d.recording_id = r.recording_id")
      if (is.na(dates$a)) return()
      rng <- as.Date(format(parse_utc(c(dates$a, dates$b)), tz = tz, "%Y-%m-%d"))
      if (!identical(rng, last_range())) {
        updateDateRangeInput(session, "dates", start = rng[1], end = rng[2])
        last_range(rng)
      }
    })

    detections <- reactive({
      data_changed()
      dates <- input$dates
      summary_detections(con, list(
        min_confidence = (input$min_conf %||% 50) / 100,
        only_validated = isTRUE(input$only_validated),
        site_ids = as.integer(input$sites),
        date_from = if (length(dates) == 2 && !is.na(dates[1])) format(dates[1]) else NULL,
        date_to = if (length(dates) == 2 && !is.na(dates[2])) format(dates[2]) else NULL
      ), tz)
    }) |> debounce(300)

    tables <- list(
      sites = reactive(species_by_site(detections(), tr("filters.no_site", lang()))),
      hours = reactive(activity_by_hour(detections(), other_label = tr("summary.other_species", lang()))),
      accumulation = reactive(species_accumulation(detections()))
    )
    plotters <- list(sites = plot_species_by_site, hours = plot_activity_by_hour,
                     accumulation = plot_species_accumulation)

    make_plot <- function(type) {
      plotters[[type]](tables[[type]](), lang(), input$palette %||% "okabe_ito", isTRUE(dark()))
    }

    for (type in names(plotters)) local({
      t <- type
      output[[paste0("plot_", t)]] <- renderPlot(make_plot(t), bg = "transparent", res = 96,
                                alt = reactive(plot_alt_text(t, tables[[t]](), lang())))
      output[[paste0("png_", t)]] <- downloadHandler(
        filename = function() paste0("biosonora_", t, "_", format(Sys.Date()), ".png"),
        content = function(file) {
          # El PNG descargado siempre con fondo blanco (para informes)
          p <- plotters[[t]](tables[[t]](), lang(), input$palette %||% "okabe_ito", FALSE) +
            ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
          ggplot2::ggsave(file, p, width = 9, height = 5, dpi = 150, bg = "white")
        }
      )
    })
  })
}
