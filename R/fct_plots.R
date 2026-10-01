# Gráficos resumen con ggplot2.
#
# Accesibilidad: paletas aptas para daltonismo, números escritos sobre las
# barras (la información no depende solo del color) y un texto alternativo
# para lectores de pantalla (plot_alt_text()).

#' Tema común: limpio, letra grande y adaptado a modo claro u oscuro
#' @noRd
biosonora_plot_theme <- function(dark = FALSE, base_size = 14) {
  fg <- if (dark) "#e6ece8" else "#1f2a24"
  grid <- if (dark) "#3a4740" else "#e3e8e5"
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = fg),
      axis.text = ggplot2::element_text(colour = fg),
      panel.grid.major = ggplot2::element_line(colour = grid),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.position = "bottom",
      plot.title.position = "plot"
    )
}

#' Especies por punto de muestreo (barras horizontales con el número escrito)
#' @noRd
plot_species_by_site <- function(df, lang = "es", palette = "okabe_ito", dark = FALSE) {
  if (nrow(df) == 0) return(empty_plot(lang, dark))
  df$site <- factor(df$site, levels = rev(df$site))
  # Okabe-Ito: azul (#0072B2); viridis / cividis: tono intermedio
  fill <- if (palette == "okabe_ito") chart_palette(palette, 8)[5] else chart_palette(palette, 3, dark)[2]
  ggplot2::ggplot(df, ggplot2::aes(x = .data$n_species, y = .data$site)) +
    ggplot2::geom_col(fill = fill, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = .data$n_species), hjust = -0.3,
                       colour = if (dark) "#e6ece8" else "#1f2a24", size = 4.5) +
    ggplot2::scale_x_continuous(breaks = integer_breaks, expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(x = tr("summary.axis_species", lang), y = NULL) +
    biosonora_plot_theme(dark)
}

#' Actividad por hora del día (barras apiladas por especie)
#' @noRd
plot_activity_by_hour <- function(df, lang = "es", palette = "okabe_ito", dark = FALSE) {
  if (nrow(df) == 0) return(empty_plot(lang, dark))
  colours <- chart_palette(palette, nlevels(df$species), dark)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$hour, y = .data$n_detections, fill = .data$species)) +
    ggplot2::geom_col(width = 0.85, colour = if (dark) "#0f1a14" else "white", linewidth = 0.2) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 2), limits = c(-0.5, 23.5)) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::labs(x = tr("summary.axis_hour", lang, tz = local_timezone()),
                  y = tr("summary.axis_detections", lang), fill = NULL) +
    biosonora_plot_theme(dark) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))
}

#' Curva de acumulación: observada (línea) y aleatoria (media e intervalo)
#' @noRd
plot_species_accumulation <- function(df, lang = "es", palette = "okabe_ito", dark = FALSE) {
  if (nrow(df) == 0) return(empty_plot(lang, dark))
  cols <- chart_palette(palette, 3, dark)
  observed_label <- tr("summary.curve_observed", lang)
  random_label <- tr("summary.curve_random", lang)
  # Formato "largo": una fila por grabación y serie
  lines <- rbind(
    data.frame(n_recordings = df$n_recordings, species = df$random_mean, series = random_label),
    data.frame(n_recordings = df$n_recordings, species = df$observed, series = observed_label)
  )
  lines$series <- factor(lines$series, levels = c(observed_label, random_label))
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = df, ggplot2::aes(x = .data$n_recordings, ymin = .data$random_low,
                                                 ymax = .data$random_high),
                         fill = cols[1], alpha = 0.25) +
    ggplot2::geom_line(data = lines, ggplot2::aes(x = .data$n_recordings, y = .data$species,
                                                  colour = .data$series, linetype = .data$series),
                       linewidth = 1) +
    # Distintos colores Y tipos de línea: se distinguen también sin color
    ggplot2::scale_colour_manual(values = stats::setNames(cols[c(3, 1)], c(observed_label, random_label))) +
    ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", "dashed"), c(observed_label, random_label))) +
    ggplot2::scale_y_continuous(breaks = integer_breaks) +
    ggplot2::scale_x_continuous(breaks = integer_breaks) +
    ggplot2::labs(x = tr("summary.axis_recordings", lang), y = tr("summary.axis_species", lang),
                  colour = NULL, linetype = NULL) +
    biosonora_plot_theme(dark)
}

# Marcas del eje solo en números enteros (especies, grabaciones)
integer_breaks <- function(limits) {
  b <- pretty(limits)
  unique(floor(b[b == floor(b)]))
}

empty_plot <- function(lang, dark) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = tr("summary.no_data", lang), size = 5,
                      colour = if (dark) "#e6ece8" else "#1f2a24") +
    ggplot2::theme_void()
}

#' Texto alternativo de cada gráfico (para lectores de pantalla)
#' @noRd
plot_alt_text <- function(type, df, lang = "es") {
  if (nrow(df) == 0) return(tr("summary.no_data", lang))
  switch(type,
    sites = tr("summary.alt_sites", lang,
               details = paste(sprintf("%s: %d", df$site, df$n_species), collapse = "; ")),
    hours = {
      by_hour <- tapply(df$n_detections, df$hour, sum)
      tr("summary.alt_hours", lang, peak = names(by_hour)[which.max(by_hour)],
         n = sum(df$n_detections))
    },
    accumulation = tr("summary.alt_accumulation", lang, species = max(df$observed),
                      recordings = max(df$n_recordings))
  )
}
