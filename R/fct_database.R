# Base de datos SQLite de Biosonora.
#
# Tablas:
#   - sites      : puntos de muestreo (nombre, coordenadas, precisión)
#   - recordings : una fila por archivo de audio, con sus metadatos,
#                  punto de muestreo, observador y estado de revisión
#   - detections, detection_history, species_lists: ver fct_detections.R
#
# Las fechas se guardan como texto ISO 8601 en UTC ("2026-08-10T16:00:00Z").
# Cada cambio se escribe al momento: no hace falta "guardar".

#' Carpeta de datos de la aplicación (base de datos y cachés)
#'
#' Por defecto es la carpeta de datos de usuario del sistema (fuera de
#' OneDrive/Dropbox, para evitar bloqueos de SQLite). Se puede cambiar con
#' `data_dir` en golem-config.yml o con la variable de entorno
#' BIOSONORA_DATA_DIR (útil para tests y pruebas).
#' @noRd
app_data_dir <- function() {
  dir <- Sys.getenv("BIOSONORA_DATA_DIR", "")
  if (!nzchar(dir)) dir <- tryCatch(get_golem_config("data_dir"), error = function(e) "")
  if (is.null(dir) || !nzchar(dir)) dir <- tools::R_user_dir("biosonora", "data")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(dir, winslash = "/")
}

#' Abre (y crea si no existe) la base de datos
#'
#' @param path Ruta del archivo SQLite. Por defecto, biosonora.sqlite en la
#'   carpeta de datos de la app.
#' @return Conexión DBI.
#' @noRd
db_connect <- function(path = file.path(app_data_dir(), "biosonora.sqlite")) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  # WAL permite leer mientras otro proceso escribe; esperar si está ocupada
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  db_init(con)
  con
}

#' Crea las tablas si no existen
#' @noRd
db_init <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sites (
      site_id     INTEGER PRIMARY KEY,
      name        TEXT NOT NULL UNIQUE,
      lat         REAL,
      lon         REAL,
      accuracy_m  REAL,
      created_at  TEXT
    )")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS recordings (
      recording_id      INTEGER PRIMARY KEY,
      file_path         TEXT NOT NULL UNIQUE,
      file_name         TEXT,
      file_size         REAL,
      fingerprint       TEXT,
      duplicate_of      TEXT,
      recorder_type     TEXT,
      model             TEXT,
      recorder_user_set INTEGER NOT NULL DEFAULT 0,
      device_id         TEXT,
      firmware          TEXT,
      start_utc         TEXT,
      datetime_source   TEXT,
      datetime_reliable INTEGER,
      duration_s        REAL,
      sample_rate       INTEGER,
      channels          INTEGER,
      bits              INTEGER,
      gain              TEXT,
      battery_v         REAL,
      temperature_c     REAL,
      original_name     TEXT,
      name_modified     INTEGER,
      config_path       TEXT,
      error_key         TEXT,
      site_id           INTEGER REFERENCES sites(site_id) ON DELETE SET NULL,
      observer          TEXT,
      review_status     TEXT NOT NULL DEFAULT 'unreviewed',
      reviewed_by       TEXT,
      reviewed_at       TEXT,
      imported_at       TEXT,
      updated_at        TEXT
    )")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_rec_start ON recordings(start_utc)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_rec_site ON recordings(site_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_rec_status ON recordings(review_status)")
  db_init_detections(con)
  invisible(con)
}

# Columnas de metadatos que se actualizan al volver a analizar una carpeta
# (nunca se tocan el punto, el observador ni el estado de revisión)
metadata_columns <- c(
  "file_name", "file_size", "fingerprint", "duplicate_of", "device_id",
  "firmware", "start_utc", "datetime_source", "datetime_reliable",
  "duration_s", "sample_rate", "channels", "bits", "gain", "battery_v",
  "temperature_c", "original_name", "name_modified", "config_path", "error_key"
)

#' Inserta o actualiza grabaciones (según su ruta)
#'
#' Si el usuario ya indicó a mano el tipo o modelo de grabadora, se respeta.
#' @param recordings data.frame devuelto por scan_folder().
#' @return Número de filas procesadas.
#' @noRd
db_upsert_recordings <- function(con, recordings) {
  if (nrow(recordings) == 0) return(0L)
  now <- format_utc(Sys.time())
  df <- recordings
  df$start_utc <- format_utc(df$start_utc)
  df$datetime_reliable <- as.integer(df$datetime_reliable)
  df$name_modified <- as.integer(df$name_modified)
  df$imported_at <- now
  df$updated_at <- now
  cols <- c("file_path", "recorder_type", "model", metadata_columns, "imported_at", "updated_at")
  df <- df[, cols]

  DBI::dbWithTransaction(con, {
    DBI::dbWriteTable(con, "tmp_recordings", df, temporary = TRUE, overwrite = TRUE)
    updates <- paste0(metadata_columns, " = excluded.", metadata_columns, collapse = ", ")
    DBI::dbExecute(con, paste0(
      "INSERT INTO recordings (", paste(cols, collapse = ", "), ") ",
      "SELECT ", paste(cols, collapse = ", "), " FROM tmp_recordings WHERE true ",
      "ON CONFLICT(file_path) DO UPDATE SET ", updates, ", ",
      "recorder_type = CASE WHEN recordings.recorder_user_set = 1 ",
      "  THEN recordings.recorder_type ELSE excluded.recorder_type END, ",
      "model = CASE WHEN recordings.recorder_user_set = 1 ",
      "  THEN recordings.model ELSE excluded.model END, ",
      "updated_at = excluded.updated_at"
    ))
    DBI::dbExecute(con, "DROP TABLE tmp_recordings")
  })
  nrow(df)
}

#' Todas las grabaciones con su punto de muestreo y fecha local
#'
#' @param local_tz Zona horaria para las columnas start_local, date_local y
#'   hour_local.
#' @noRd
db_get_recordings <- function(con, local_tz = "Europe/Madrid") {
  df <- DBI::dbGetQuery(con, "
    SELECT r.*, s.name AS site_name, s.lat, s.lon, s.accuracy_m
    FROM recordings r LEFT JOIN sites s ON s.site_id = r.site_id
    ORDER BY r.start_utc, r.file_name")
  df$start_utc <- parse_utc(df$start_utc)
  local <- format(df$start_utc, tz = local_tz, format = "%Y-%m-%d %H:%M:%S")
  df$start_local <- local
  df$date_local <- as.Date(substr(local, 1, 10))
  df$hour_local <- as.integer(substr(local, 12, 13))
  df$datetime_reliable <- as.logical(df$datetime_reliable)
  df$name_modified <- as.logical(df$name_modified)
  df
}

#' Una grabación por su identificador
#' @noRd
db_get_recording <- function(con, recording_id) {
  df <- DBI::dbGetQuery(con, "
    SELECT r.*, s.name AS site_name, s.lat, s.lon, s.accuracy_m
    FROM recordings r LEFT JOIN sites s ON s.site_id = r.site_id
    WHERE r.recording_id = ?", params = list(as.integer(recording_id)))
  if (nrow(df) == 0) return(NULL)
  df$start_utc <- parse_utc(df$start_utc)
  as.list(df)
}

#' Crea un punto de muestreo o actualiza sus coordenadas si ya existe
#' @return site_id
#' @noRd
db_save_site <- function(con, name, lat = NA, lon = NA, accuracy_m = NA) {
  name <- trimws(name)
  if (!nzchar(name)) biosonora_abort("error.site_name_required")
  validate_coordinates(lat, lon)
  DBI::dbExecute(con, "
    INSERT INTO sites (name, lat, lon, accuracy_m, created_at) VALUES (?, ?, ?, ?, ?)
    ON CONFLICT(name) DO UPDATE SET lat = excluded.lat, lon = excluded.lon,
      accuracy_m = excluded.accuracy_m",
    params = list(name, as.numeric(lat), as.numeric(lon), as.numeric(accuracy_m),
                  format_utc(Sys.time())))
  DBI::dbGetQuery(con, "SELECT site_id FROM sites WHERE name = ?", params = list(name))$site_id
}

#' Lista de puntos de muestreo
#' @noRd
db_get_sites <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM sites ORDER BY name")
}

#' Asigna punto de muestreo y/o observador a varias grabaciones a la vez
#' @param site_id NULL para no cambiarlo.
#' @param observer NULL para no cambiarlo.
#' @noRd
db_assign_site <- function(con, recording_ids, site_id = NULL, observer = NULL) {
  if (length(recording_ids) == 0) return(0L)
  now <- format_utc(Sys.time())
  n <- 0L
  DBI::dbWithTransaction(con, {
    for (id in recording_ids) {
      if (!is.null(site_id)) {
        n <- n + DBI::dbExecute(con,
          "UPDATE recordings SET site_id = ?, updated_at = ? WHERE recording_id = ?",
          params = list(as.integer(site_id), now, as.integer(id)))
      }
      if (!is.null(observer)) {
        DBI::dbExecute(con,
          "UPDATE recordings SET observer = ?, updated_at = ? WHERE recording_id = ?",
          params = list(observer, now, as.integer(id)))
      }
    }
  })
  length(recording_ids)
}

#' El usuario indica el tipo y modelo de grabadora (se respetará al re-analizar)
#' @noRd
db_set_recorder <- function(con, recording_ids, recorder_type, model) {
  if (!recorder_type %in% c("audiomoth", "manual", "unknown")) {
    biosonora_abort("error.invalid_recorder_type")
  }
  now <- format_utc(Sys.time())
  DBI::dbWithTransaction(con, {
    for (id in recording_ids) {
      DBI::dbExecute(con, "
        UPDATE recordings SET recorder_type = ?, model = ?, recorder_user_set = 1,
          updated_at = ? WHERE recording_id = ?",
        params = list(recorder_type, model, now, as.integer(id)))
    }
  })
  length(recording_ids)
}

#' Cambia el estado de revisión: "unreviewed", "in_review" o "reviewed"
#' @noRd
db_set_review_status <- function(con, recording_id, status, reviewer = NA_character_) {
  if (!status %in% review_statuses) biosonora_abort("error.invalid_status")
  now <- format_utc(Sys.time())
  DBI::dbExecute(con, "
    UPDATE recordings SET review_status = ?, reviewed_by = ?, reviewed_at = ?,
      updated_at = ? WHERE recording_id = ?",
    params = list(status, reviewer, now, now, as.integer(recording_id)))
}

review_statuses <- c("unreviewed", "in_review", "reviewed")

#' Comprueba que las coordenadas son válidas (WGS84, grados decimales)
#' @noRd
validate_coordinates <- function(lat, lon) {
  lat <- suppressWarnings(as.numeric(lat))
  lon <- suppressWarnings(as.numeric(lon))
  if (xor(is.na(lat), is.na(lon))) biosonora_abort("error.coordinates_incomplete")
  if (!is.na(lat) && (lat < -90 || lat > 90)) biosonora_abort("error.latitude_range")
  if (!is.na(lon) && (lon < -180 || lon > 180)) biosonora_abort("error.longitude_range")
  invisible(TRUE)
}

format_utc <- function(x) {
  out <- format(x, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  out[is.na(x)] <- NA_character_
  out
}

parse_utc <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}
