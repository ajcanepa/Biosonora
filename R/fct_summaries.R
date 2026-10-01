# Resúmenes gráficos: especies por punto, actividad por hora y curva de
# acumulación de especies.
#
# Los cálculos (tablas) están separados de los gráficos para poder probarlos.
# Detecciones que cuentan: categoría "especie", no marcadas como incorrectas,
# con confianza >= umbral (las manuales siempre). Opcionalmente, solo las
# validadas como correctas.

#' Detecciones que entran en los resúmenes
#'
#' @param options Lista con min_confidence (0-1), only_validated, site_ids,
#'   date_from, date_to.
#' @noRd
summary_detections <- function(con, options = list(), local_tz = "Europe/Madrid") {
  det <- export_detections_base(con, local_tz)
  keep <- det$category == "species" & det$validation != "incorrect" &
    (is.na(det$confidence) | det$confidence >= (options$min_confidence %||% 0))
  if (isTRUE(options$only_validated)) keep <- keep & det$validation == "correct"
  if (length(options$site_ids)) keep <- keep & det$site_id %in% options$site_ids
  if (!is.null(options$date_from)) keep <- keep & det$local_date >= options$date_from
  if (!is.null(options$date_to)) keep <- keep & det$local_date <= options$date_to
  det <- det[keep, , drop = FALSE]
  det$hour <- as.integer(substr(det$local_datetime, 12, 13))
  det$site_label <- ifelse(is.na(det$site_name), NA_character_, det$site_name)
  det
}

#' Número de especies (y de detecciones) por punto de muestreo
#' @return data.frame site, n_species, n_detections, n_recordings
#' @noRd
species_by_site <- function(det, no_site_label = "(sin punto)") {
  if (nrow(det) == 0) {
    return(data.frame(site = character(), n_species = integer(), n_detections = integer(),
                      n_recordings = integer()))
  }
  site <- ifelse(is.na(det$site_label), no_site_label, det$site_label)
  out <- do.call(rbind, lapply(split(det, site), function(g) {
    data.frame(site = ifelse(is.na(g$site_label[1]), no_site_label, g$site_label[1]),
               n_species = length(unique(g$species)), n_detections = nrow(g),
               n_recordings = length(unique(g$recording_id)))
  }))
  rownames(out) <- NULL
  out[order(-out$n_species, out$site), , drop = FALSE]
}

#' Actividad por hora del día (hora local)
#'
#' @param top_n Número de especies con más detecciones que se muestran por
#'   separado; el resto se agrupan como "otras".
#' @return data.frame hour (0-23), species, n_detections
#' @noRd
activity_by_hour <- function(det, top_n = 6, other_label = "Otras") {
  if (nrow(det) == 0) {
    return(data.frame(hour = integer(), species = character(), n_detections = integer()))
  }
  label <- ifelse(is.na(det$species_common) | det$species_common == "", det$species,
                  det$species_common)
  top <- names(sort(table(label), decreasing = TRUE))[seq_len(min(top_n, length(unique(label))))]
  group <- ifelse(label %in% top, label, other_label)
  counts <- as.data.frame(table(hour = det$hour, species = group), stringsAsFactors = FALSE)
  names(counts)[3] <- "n_detections"
  counts$hour <- as.integer(counts$hour)
  counts <- counts[counts$n_detections > 0, , drop = FALSE]
  levels_order <- c(top, if (other_label %in% group) other_label)
  counts$species <- factor(counts$species, levels = levels_order)
  counts[order(counts$hour, counts$species), , drop = FALSE]
}

#' Curva de acumulación de especies
#'
#' Número de especies distintas a medida que se suman grabaciones, en orden
#' cronológico ("observed") y como media de órdenes al azar ("random", con su
#' intervalo del 95 %), que es lo habitual para comparar esfuerzos de muestreo.
#'
#' @param n_perm Número de órdenes al azar.
#' @return data.frame n_recordings, observed, random_mean, random_low, random_high
#' @noRd
species_accumulation <- function(det, n_perm = 100, seed = 1) {
  if (nrow(det) == 0) {
    return(data.frame(n_recordings = integer(), observed = integer(), random_mean = numeric(),
                      random_low = numeric(), random_high = numeric()))
  }
  recs <- unique(det[, c("recording_id", "start_utc")])
  recs <- recs[order(recs$start_utc, recs$recording_id), ]
  species_in <- split(det$species, det$recording_id)
  ids <- as.character(recs$recording_id)
  curve <- function(order_ids) {
    seen <- character()
    vapply(order_ids, function(id) {
      seen <<- union(seen, species_in[[id]])
      length(seen)
    }, integer(1))
  }
  observed <- curve(ids)
  set.seed(seed)
  perms <- if (length(ids) > 1) vapply(seq_len(n_perm), function(i) curve(sample(ids)), numeric(length(ids)))
           else matrix(observed, nrow = 1)
  perms <- matrix(perms, nrow = length(ids))
  data.frame(
    n_recordings = seq_along(ids),
    observed = unname(observed),
    random_mean = rowMeans(perms),
    random_low = apply(perms, 1, stats::quantile, 0.025, names = FALSE),
    random_high = apply(perms, 1, stats::quantile, 0.975, names = FALSE)
  )
}

#' Paletas para gráficos, aptas para daltonismo
#'
#' "okabe_ito": 8 colores distinguibles con cualquier tipo de daltonismo.
#' "viridis" y "cividis": escalas continuas que se leen igual en gris.
#' @param dark En modo oscuro se descartan los tonos más oscuros de viridis y
#'   cividis, que apenas se verían sobre el fondo.
#' @noRd
chart_palette <- function(name = "okabe_ito", n = 8, dark = FALSE) {
  continuous <- function(pal) {
    if (dark) grDevices::hcl.colors(n + 2, pal)[-(1:2)] else grDevices::hcl.colors(n, pal)
  }
  switch(name,
    okabe_ito = rep(grDevices::palette.colors(9, "Okabe-Ito")[-1], length.out = n), # sin el negro
    viridis = continuous("viridis"),
    cividis = continuous("Cividis"),
    biosonora_abort("error.invalid_palette")
  )
}
