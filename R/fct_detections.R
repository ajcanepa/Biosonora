# Detecciones (de BirdNET o manuales), su validación e historial.
#
# Tablas:
#   - detections        : una fila por detección o anotación manual
#   - detection_history : quién cambió qué y cuándo (nunca se borra)
#
# Confianza: BirdNET da una puntuación entre 0 y 1 (no es una precisión
# medida). Se guarda entre 0 y 1 y se muestra como porcentaje.
# Validación: "pending" (sin revisar), "correct", "incorrect", "doubtful".
# Categoría: "species", "human" (voz humana: privada, nunca se exporta) u
# "other" (ruidos: perros, motores...).

validation_states <- c("pending", "correct", "incorrect", "doubtful")
detection_categories <- c("species", "human", "other")

#' Crea las tablas de detecciones si no existen
#' @noRd
db_init_detections <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS detections (
      detection_id    INTEGER PRIMARY KEY,
      recording_id    INTEGER NOT NULL REFERENCES recordings(recording_id) ON DELETE CASCADE,
      source          TEXT NOT NULL,              -- 'birdnet' o 'manual'
      classifier      TEXT,
      import_file     TEXT,
      import_format   TEXT,
      start_s         REAL NOT NULL,
      end_s           REAL NOT NULL,
      low_hz          REAL,
      high_hz         REAL,
      scientific_name TEXT NOT NULL,
      common_name     TEXT,
      confidence      REAL,                       -- 0 a 1 (NA en las manuales)
      category        TEXT NOT NULL DEFAULT 'species',
      validation      TEXT NOT NULL DEFAULT 'pending',
      corrected_scientific_name TEXT,
      corrected_common_name     TEXT,
      notes           TEXT,
      created_by      TEXT,
      created_at      TEXT,
      validated_by    TEXT,
      validated_at    TEXT,
      updated_at      TEXT,
      UNIQUE (recording_id, source, start_s, end_s, scientific_name)
    )")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_det_rec ON detections(recording_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_det_species ON detections(scientific_name)")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS detection_history (
      history_id   INTEGER PRIMARY KEY,
      detection_id INTEGER NOT NULL,
      action       TEXT NOT NULL,   -- created, validation, species, notes, deleted
      old_value    TEXT,
      new_value    TEXT,
      user         TEXT,
      at           TEXT NOT NULL
    )")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS species_lists (
      list_id      INTEGER PRIMARY KEY,
      generated_at TEXT NOT NULL,
      generated_by TEXT,
      filters      TEXT              -- filtros usados, en JSON
    )")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS species_list_items (
      list_id         INTEGER NOT NULL REFERENCES species_lists(list_id) ON DELETE CASCADE,
      scientific_name TEXT NOT NULL,
      common_name     TEXT,
      max_confidence  REAL,
      mean_confidence REAL,
      n_detections    INTEGER,
      n_recordings    INTEGER,
      n_correct       INTEGER,
      n_incorrect     INTEGER,
      n_doubtful      INTEGER,
      n_pending       INTEGER,
      status          TEXT
    )")
  invisible(con)
}

# Registra un cambio en el historial
log_change <- function(con, detection_id, action, old_value, new_value, user) {
  DBI::dbExecute(con, "
    INSERT INTO detection_history (detection_id, action, old_value, new_value, user, at)
    VALUES (?, ?, ?, ?, ?, ?)",
    params = list(as.integer(detection_id), action, as.character(old_value),
                  as.character(new_value), user %||% NA_character_, format_utc(Sys.time())))
}

#' Guarda detecciones de BirdNET ya asociadas a grabaciones
#'
#' Si se vuelve a importar el mismo archivo, se actualiza la confianza pero se
#' conserva la validación, las correcciones y las notas.
#'
#' @param detections data.frame de read_birdnet_file() con recording_id.
#' @return Número de detecciones nuevas.
#' @noRd
db_import_detections <- function(con, detections, import_file, import_format,
                                 classifier = "BirdNET", user = NA_character_) {
  detections <- detections[!is.na(detections$recording_id), , drop = FALSE]
  if (nrow(detections) == 0) return(0L)
  now <- format_utc(Sys.time())
  band <- birdnet_default_band
  df <- data.frame(
    recording_id = as.integer(detections$recording_id),
    source = "birdnet", classifier = classifier,
    import_file = import_file, import_format = import_format,
    start_s = detections$start_s, end_s = detections$end_s,
    low_hz = ifelse(is.na(detections$low_hz), band[1], detections$low_hz),
    high_hz = ifelse(is.na(detections$high_hz), band[2], detections$high_hz),
    scientific_name = detections$scientific_name,
    common_name = detections$common_name,
    confidence = detections$confidence,
    category = detections$category,
    created_by = user, created_at = now, updated_at = now,
    stringsAsFactors = FALSE
  )
  before <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM detections")$n
  DBI::dbWithTransaction(con, {
    DBI::dbWriteTable(con, "tmp_detections", df, temporary = TRUE, overwrite = TRUE)
    cols <- paste(names(df), collapse = ", ")
    DBI::dbExecute(con, paste0(
      "INSERT INTO detections (", cols, ") SELECT ", cols, " FROM tmp_detections WHERE true ",
      "ON CONFLICT(recording_id, source, start_s, end_s, scientific_name) DO UPDATE SET ",
      "confidence = excluded.confidence, common_name = excluded.common_name, ",
      "low_hz = excluded.low_hz, high_hz = excluded.high_hz, ",
      "import_file = excluded.import_file, updated_at = excluded.updated_at"))
    DBI::dbExecute(con, "DROP TABLE tmp_detections")
  })
  DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM detections")$n - before
}

#' Crea una anotación manual dibujada sobre el espectrograma
#' @return detection_id
#' @noRd
db_add_manual_detection <- function(con, recording_id, start_s, end_s, low_hz, high_hz,
                                    scientific_name, common_name = NA_character_,
                                    category = "species", notes = NA_character_,
                                    user = NA_character_) {
  scientific_name <- trimws(scientific_name %||% "")
  if (category == "human" && !nzchar(scientific_name)) scientific_name <- "Human vocal"
  if (!nzchar(scientific_name)) biosonora_abort("error.species_required")
  if (!category %in% detection_categories) biosonora_abort("error.invalid_category")
  if (!is.finite(start_s) || !is.finite(end_s) || end_s <= start_s) {
    biosonora_abort("error.annotation_too_small")
  }
  now <- format_utc(Sys.time())
  DBI::dbExecute(con, "
    INSERT INTO detections (recording_id, source, classifier, start_s, end_s, low_hz, high_hz,
      scientific_name, common_name, confidence, category, validation, notes,
      created_by, created_at, validated_by, validated_at, updated_at)
    VALUES (?, 'manual', NULL, ?, ?, ?, ?, ?, ?, NULL, ?, 'correct', ?, ?, ?, ?, ?, ?)",
    params = list(as.integer(recording_id), start_s, end_s, low_hz, high_hz,
                  scientific_name, common_name %||% NA_character_, category,
                  notes %||% NA_character_, user, now, user, now, now))
  id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
  log_change(con, id, "created", NA, scientific_name, user)
  id
}

#' Detecciones de una grabación (con la especie efectiva ya corregida)
#'
#' @param min_confidence Umbral (0-1). Las manuales (sin confianza) siempre se incluyen.
#' @noRd
db_get_detections <- function(con, recording_id, min_confidence = 0) {
  df <- DBI::dbGetQuery(con, "
    SELECT * FROM detections
    WHERE recording_id = ? AND (confidence IS NULL OR confidence >= ?)
    ORDER BY start_s, confidence DESC",
    params = list(as.integer(recording_id), min_confidence))
  add_effective_species(df)
}

#' Una detección
#' @noRd
db_get_detection <- function(con, detection_id) {
  df <- DBI::dbGetQuery(con, "SELECT * FROM detections WHERE detection_id = ?",
                        params = list(as.integer(detection_id)))
  if (nrow(df) == 0) return(NULL)
  as.list(add_effective_species(df))
}

# Especie "efectiva": la corregida por el revisor o, si no hay, la original
add_effective_species <- function(df) {
  corrected <- !is.na(df$corrected_scientific_name) & df$corrected_scientific_name != ""
  df$species <- ifelse(corrected, df$corrected_scientific_name, df$scientific_name)
  df$species_common <- ifelse(corrected, df$corrected_common_name, df$common_name)
  df
}

#' Cambia la validación de una detección
#' @noRd
db_validate_detection <- function(con, detection_id, validation, user = NA_character_) {
  if (!validation %in% validation_states) biosonora_abort("error.invalid_status")
  old <- db_get_detection(con, detection_id)
  if (is.null(old)) return(invisible(FALSE))
  now <- format_utc(Sys.time())
  DBI::dbExecute(con, "
    UPDATE detections SET validation = ?, validated_by = ?, validated_at = ?, updated_at = ?
    WHERE detection_id = ?",
    params = list(validation, user, now, now, as.integer(detection_id)))
  log_change(con, detection_id, "validation", old$validation, validation, user)
  invisible(TRUE)
}

#' Corrige la especie de una detección (queda validada como correcta)
#' @noRd
db_correct_species <- function(con, detection_id, scientific_name, common_name = NA_character_,
                               user = NA_character_) {
  scientific_name <- trimws(scientific_name %||% "")
  if (!nzchar(scientific_name)) biosonora_abort("error.species_required")
  old <- db_get_detection(con, detection_id)
  if (is.null(old)) return(invisible(FALSE))
  now <- format_utc(Sys.time())
  # Si se vuelve a la especie original, se quita la corrección
  same <- identical(scientific_name, old$scientific_name)
  DBI::dbExecute(con, "
    UPDATE detections SET corrected_scientific_name = ?, corrected_common_name = ?,
      validation = 'correct', validated_by = ?, validated_at = ?, updated_at = ?
    WHERE detection_id = ?",
    params = list(if (same) NA_character_ else scientific_name,
                  if (same) NA_character_ else (common_name %||% NA_character_),
                  user, now, now, as.integer(detection_id)))
  log_change(con, detection_id, "species", old$species, scientific_name, user)
  invisible(TRUE)
}

#' Guarda las notas de una detección
#' @noRd
db_set_detection_notes <- function(con, detection_id, notes, user = NA_character_) {
  old <- db_get_detection(con, detection_id)
  if (is.null(old)) return(invisible(FALSE))
  notes <- trimws(notes %||% "")
  DBI::dbExecute(con, "UPDATE detections SET notes = ?, updated_at = ? WHERE detection_id = ?",
                 params = list(if (nzchar(notes)) notes else NA_character_,
                               format_utc(Sys.time()), as.integer(detection_id)))
  log_change(con, detection_id, "notes", old$notes, notes, user)
  invisible(TRUE)
}

#' Marca (o desmarca) una detección como voz humana: privada
#' @noRd
db_set_detection_category <- function(con, detection_id, category, user = NA_character_) {
  if (!category %in% detection_categories) biosonora_abort("error.invalid_category")
  old <- db_get_detection(con, detection_id)
  if (is.null(old)) return(invisible(FALSE))
  DBI::dbExecute(con, "UPDATE detections SET category = ?, updated_at = ? WHERE detection_id = ?",
                 params = list(category, format_utc(Sys.time()), as.integer(detection_id)))
  log_change(con, detection_id, "category", old$category, category, user)
  invisible(TRUE)
}

#' Borra una anotación manual (las de BirdNET no se borran: se marcan incorrectas)
#' @noRd
db_delete_manual_detection <- function(con, detection_id, user = NA_character_) {
  old <- db_get_detection(con, detection_id)
  if (is.null(old)) return(invisible(FALSE))
  if (old$source != "manual") biosonora_abort("error.only_manual_delete")
  log_change(con, detection_id, "deleted", old$species, NA, user)
  DBI::dbExecute(con, "DELETE FROM detections WHERE detection_id = ?",
                 params = list(as.integer(detection_id)))
  invisible(TRUE)
}

#' Historial de cambios de una detección
#' @noRd
db_get_history <- function(con, detection_id) {
  DBI::dbGetQuery(con, "SELECT * FROM detection_history WHERE detection_id = ? ORDER BY history_id",
                  params = list(as.integer(detection_id)))
}

#' Nombres de especie ya usados (para sugerirlos al corregir o anotar)
#' @return data.frame con scientific_name y common_name
#' @noRd
db_known_species <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT name AS scientific_name, MAX(common) AS common_name FROM (
      SELECT scientific_name AS name, common_name AS common FROM detections
      UNION ALL
      SELECT corrected_scientific_name, corrected_common_name FROM detections
        WHERE corrected_scientific_name IS NOT NULL
    ) WHERE name IS NOT NULL GROUP BY name ORDER BY name")
}

#' Recuento de detecciones por grabación (para la lista de pistas)
#' @noRd
db_detection_counts <- function(con, min_confidence = 0) {
  DBI::dbGetQuery(con, "
    SELECT recording_id, COUNT(*) AS n_detections,
      SUM(validation = 'pending') AS n_pending,
      GROUP_CONCAT(DISTINCT COALESCE(corrected_scientific_name, scientific_name)) AS species
    FROM detections
    WHERE (confidence IS NULL OR confidence >= ?) AND validation != 'incorrect'
    GROUP BY recording_id", params = list(min_confidence))
}
