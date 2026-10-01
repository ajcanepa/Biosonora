# Resúmenes gráficos y accesibilidad de los colores

summary_db <- function() {
  con <- db_connect(tempfile(fileext = ".sqlite"))
  db_upsert_recordings(con, scan_folder(example_dir()))
  recs <- DBI::dbGetQuery(con, "SELECT recording_id, file_path, file_name, original_name FROM recordings")
  for (name in c("24BBD608675B68F7_20260810_160000.BirdNET.results.csv",
                 "combinado.BirdNET.selection.table.txt")) {
    d <- match_detections_to_recordings(read_birdnet_file(example_dir("birdnet", name)), recs, name)
    db_import_detections(con, d, name, attr(d, "format"))
  }
  site <- db_save_site(con, "Río", 42.34, -3.70, 10)
  db_assign_site(con, recs$recording_id[grepl("^24BBD.*20260810_160000.WAV$", recs$file_name)], site)
  con
}

test_that("las detecciones de los resúmenes excluyen voz humana e incorrectas y respetan el umbral", {
  con <- summary_db()
  on.exit(DBI::dbDisconnect(con))
  det <- summary_detections(con)
  expect_equal(nrow(det), 4) # 5 importadas - 1 voz humana
  expect_equal(nrow(summary_detections(con, list(min_confidence = 0.5))), 3)
  db_validate_detection(con, det$detection_id[det$species == "Erithacus rubecula"], "incorrect")
  expect_equal(nrow(summary_detections(con)), 3)
  expect_equal(nrow(summary_detections(con, list(only_validated = TRUE))), 0)
})

test_that("especies por punto y actividad por hora", {
  con <- summary_db()
  on.exit(DBI::dbDisconnect(con))
  det <- summary_detections(con)
  sites <- species_by_site(det, "(sin punto)")
  expect_equal(sites$site, c("Río", "(sin punto)"))
  expect_equal(sites$n_species, c(2L, 1L))
  hours <- activity_by_hour(det, top_n = 1, other_label = "Otras")
  expect_equal(levels(hours$species), c("Mirlo común", "Otras"))
  expect_equal(sum(hours$n_detections), 4)
  expect_true(all(hours$hour == 18L)) # 16:00 UTC = 18:00 en Madrid (agosto)
})

test_that("la curva de acumulación crece y acaba en el total de especies", {
  con <- summary_db()
  on.exit(DBI::dbDisconnect(con))
  det <- summary_detections(con)
  acc <- species_accumulation(det, n_perm = 20)
  expect_equal(acc$n_recordings, seq_len(nrow(acc)))
  expect_true(all(diff(acc$observed) >= 0))
  expect_equal(max(acc$observed), length(unique(det$species)))
  expect_equal(acc$random_mean[nrow(acc)], length(unique(det$species)))
  expect_true(all(acc$random_low <= acc$random_high))
  expect_equal(nrow(species_accumulation(det[0, ])), 0)
})

test_that("los gráficos se dibujan con todas las paletas, en claro y oscuro, y tienen texto alternativo", {
  con <- summary_db()
  on.exit(DBI::dbDisconnect(con))
  det <- summary_detections(con)
  tables <- list(sites = species_by_site(det), hours = activity_by_hour(det),
                 accumulation = species_accumulation(det, n_perm = 10))
  plotters <- list(sites = plot_species_by_site, hours = plot_activity_by_hour,
                   accumulation = plot_species_accumulation)
  for (type in names(plotters)) {
    for (pal in c("okabe_ito", "viridis", "cividis")) {
      for (dark in c(FALSE, TRUE)) {
        p <- plotters[[type]](tables[[type]], "es", pal, dark)
        expect_s3_class(p, "ggplot")
        expect_no_error(ggplot2::ggplot_build(p))
      }
    }
    expect_true(nzchar(plot_alt_text(type, tables[[type]], "es")))
  }
  # Sin datos: un gráfico vacío con un mensaje, no un error
  expect_no_error(ggplot2::ggplot_build(plot_species_by_site(species_by_site(det[0, ]), "en")))
})

test_that("la paleta Okabe-Ito no incluye el negro y no repite colores en 8 categorías", {
  pal <- chart_palette("okabe_ito", 8)
  expect_false("#000000" %in% toupper(pal))
  expect_equal(length(unique(pal)), 8)
  expect_error(chart_palette("arcoiris"), class = "biosonora_error")
})

# Contraste WCAG: (L1 + 0,05) / (L2 + 0,05) con la luminancia relativa
contrast_ratio <- function(fg, bg) {
  lum <- function(hex) {
    rgb <- grDevices::col2rgb(hex)[, 1] / 255
    rgb <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * rgb)
  }
  l <- sort(c(lum(fg), lum(bg)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05)
}

test_that("los colores de texto de estados cumplen el contraste mínimo (WCAG AA, 4,5:1)", {
  # Colores de inst/app/www/biosonora.css
  light <- c(unreviewed = "#6c757d", in_review = "#9a5d00", reviewed = "#2E6B4F",
             pending = "#8a6d00", correct = "#2E6B4F", incorrect = "#B23A3A", doubtful = "#8f5610")
  dark <- c(unreviewed = "#adb5bd", in_review = "#ffc866", reviewed = "#8fd6a8",
            pending = "#ffd54f", correct = "#8fd6a8", incorrect = "#ff8a8a")
  for (col in names(light)) {
    expect_gte(contrast_ratio(light[[col]], "#ffffff"), 4.5, label = paste("claro:", col))
  }
  for (col in names(dark)) {
    expect_gte(contrast_ratio(dark[[col]], "#1d1f21"), 4.5, label = paste("oscuro:", col))
  }
})

test_that("las paletas siempre devuelven colores válidos (también en modo oscuro)", {
  for (pal in c("okabe_ito", "viridis", "cividis")) {
    for (n in c(1, 3, 8)) {
      for (dark in c(FALSE, TRUE)) {
        cols <- chart_palette(pal, n, dark)
        expect_length(cols, n)
        expect_false(any(is.na(cols)))
      }
    }
  }
  b <- integer_breaks(c(0, 12.5))
  expect_true(all(b == floor(b)))
  expect_equal(integer_breaks(c(1, 4)), 1:4)
})
