# Lectores específicos de AudioMoth: CONFIG.TXT, nombre de archivo,
# comentario del WAV (ICMT) y bloque GUANO.
#
# Zonas horarias: AudioMoth indica la zona en cada dato ("(UTC)", "(UTC+2)").
# Todas las fechas se convierten a un instante UTC (POSIXct con tz = "UTC");
# la conversión a hora local se hace después con la zona de la configuración.

#' Lee un archivo CONFIG.TXT de AudioMoth
#'
#' Formato: una línea por ajuste, "Clave<espacios>: valor". Las claves no
#' contienen ":" pero los valores sí (horas), así que se corta por el primero.
#' Un valor "-" significa "no aplica". Se aceptan claves desconocidas (otros
#' firmwares añaden campos).
#'
#' @param path Ruta al CONFIG.TXT.
#' @return Lista con `raw` (todos los pares clave/valor) y los campos
#'   interpretados: device_id, firmware, device_time_utc, sample_rate, gain,
#'   sleep_s, record_s, periods (data.frame), first_date, last_date,
#'   utc_offset_h.
#' @noRd
read_audiomoth_config <- function(path) {
  lines <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) biosonora_abort("error.config_unreadable", file = basename(path))
  )
  lines <- trimws(sub("\r$", "", lines))
  lines <- lines[grepl(":", lines, fixed = TRUE)]
  if (length(lines) == 0) {
    biosonora_abort("error.config_unreadable", file = basename(path))
  }
  keys <- trimws(sub(":.*$", "", lines))
  values <- trimws(sub("^[^:]*:", "", lines))
  values[values == "-"] <- NA_character_
  raw <- stats::setNames(as.list(values), keys)

  get <- function(key) {
    v <- raw[[key]]
    if (is.null(v)) NA_character_ else v
  }

  # Periodos de grabación: "16:00 - 21:00 (UTC)"
  period_keys <- grep("^Recording period [0-9]+$", keys, value = TRUE)
  periods <- do.call(rbind, lapply(period_keys, function(k) {
    v <- raw[[k]]
    m <- regmatches(v, regexec("([0-9]{2}:[0-9]{2})\\s*-\\s*([0-9]{2}:[0-9]{2})", v))[[1]]
    data.frame(
      start = if (length(m)) m[2] else NA_character_,
      end = if (length(m)) m[3] else NA_character_,
      utc_offset_h = parse_utc_offset(v),
      stringsAsFactors = FALSE
    )
  }))

  device_time <- get("Device time")
  list(
    raw = raw,
    device_id = get("Device ID"),
    firmware = get("Firmware"),
    device_time_utc = parse_local_datetime(
      sub("\\s*\\(.*$", "", device_time), "%Y-%m-%d %H:%M:%S",
      parse_utc_offset(device_time)
    ),
    sample_rate = suppressWarnings(as.integer(get("Sample rate (Hz)"))),
    gain = get("Gain"),
    sleep_s = suppressWarnings(as.numeric(get("Sleep duration (s)"))),
    record_s = suppressWarnings(as.numeric(get("Recording duration (s)"))),
    periods = periods,
    first_date = sub("\\s*\\(.*$", "", get("First recording date")),
    last_date = sub("\\s*\\(.*$", "", get("Last recording date")),
    utc_offset_h = parse_utc_offset(device_time)
  )
}

#' Interpreta el nombre de un archivo AudioMoth
#'
#' Formatos admitidos: "AAAAMMDD_HHMMSS.WAV" y "IDDISPOSITIVO_AAAAMMDD_HHMMSS.WAV".
#' Cualquier texto extra antes de la extensión (p. ej. " 1" de una copia) se
#' devuelve en `suffix` para avisar de que el nombre fue modificado.
#'
#' @param file_name Nombre del archivo (sin carpeta).
#' @return Lista con device_id, datetime_text ("AAAAMMDD_HHMMSS") y suffix, o
#'   NULL si el nombre no sigue el formato.
#' @noRd
parse_audiomoth_filename <- function(file_name) {
  pattern <- "^(?:([0-9A-Fa-f]{16})_)?([0-9]{8}_[0-9]{6})(.*)\\.wav$"
  m <- regmatches(file_name, regexec(pattern, file_name, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m) == 0) return(NULL)
  list(
    device_id = if (nzchar(m[2])) toupper(m[2]) else NA_character_,
    datetime_text = m[3],
    suffix = m[4]
  )
}

#' Interpreta el comentario (ICMT) que AudioMoth escribe en cada WAV
#'
#' Ejemplo: "Recorded at 16:00:00 10/08/2026 (UTC) by AudioMoth 2403750567C2A798
#' at medium gain while battery was 4.8V and temperature was 39.1C."
#' Cada dato se extrae por separado porque la frase cambia entre firmwares.
#'
#' @param comment Texto del comentario.
#' @return Lista con datetime_utc, device_id, gain, battery_v, temperature_c.
#' @noRd
parse_audiomoth_comment <- function(comment) {
  if (is.null(comment) || !grepl("AudioMoth", comment, fixed = TRUE)) return(NULL)
  pick <- function(pattern) {
    m <- regmatches(comment, regexec(pattern, comment, perl = TRUE))[[1]]
    if (length(m) > 1) m[2] else NA_character_
  }
  dt <- regmatches(
    comment,
    regexec("Recorded at ([0-9:]{8}) ([0-9/]{10}) \\((UTC[^)]*)\\)", comment)
  )[[1]]
  datetime_utc <- if (length(dt)) {
    parse_local_datetime(paste(dt[3], dt[2]), "%d/%m/%Y %H:%M:%S", parse_utc_offset(dt[4]))
  } else {
    as.POSIXct(NA, tz = "UTC")
  }
  list(
    datetime_utc = datetime_utc,
    device_id = pick("by AudioMoth ([0-9A-Fa-f]{16})"),
    gain = pick("at ([a-z -]+) gain"),
    # "4.8V", "less than 2.5V" o "greater than 4.9V": nos quedamos con el número
    battery_v = suppressWarnings(as.numeric(pick("battery (?:state )?was (?:less than |greater than )?([0-9.]+)V"))),
    temperature_c = suppressWarnings(as.numeric(pick("temperature was (-?[0-9.]+)C")))
  )
}

#' Interpreta la marca de tiempo GUANO (ISO 8601)
#'
#' Admite "2026-08-10T16:00:00Z", con desfase "+02:00" o sin zona (en ese
#' caso se devuelve NA, porque no sabemos a qué zona se refiere).
#' @noRd
parse_guano_timestamp <- function(x) {
  if (is.null(x) || is.na(x)) return(as.POSIXct(NA, tz = "UTC"))
  m <- regmatches(x, regexec(
    "^([0-9]{4}-[0-9]{2}-[0-9]{2})[T ]([0-9]{2}:[0-9]{2}:[0-9]{2})(?:\\.[0-9]+)?(Z|[+-][0-9]{2}:?[0-9]{2})?$",
    x, perl = TRUE
  ))[[1]]
  if (length(m) == 0 || !nzchar(m[4])) return(as.POSIXct(NA, tz = "UTC"))
  offset_h <- if (m[4] == "Z") 0 else {
    sign <- ifelse(substr(m[4], 1, 1) == "-", -1, 1)
    digits <- gsub("[^0-9]", "", m[4])
    sign * (as.numeric(substr(digits, 1, 2)) + as.numeric(substr(digits, 3, 4)) / 60)
  }
  parse_local_datetime(paste(m[2], m[3]), "%Y-%m-%d %H:%M:%S", offset_h)
}

#' Desfase respecto a UTC indicado en un texto: "(UTC)" -> 0, "(UTC+2)" -> 2,
#' "(UTC-3:30)" -> -3.5. Devuelve NA si el texto no indica zona.
#' @noRd
parse_utc_offset <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_real_)
  m <- regmatches(x, regexec("UTC(?:([+-])([0-9]{1,2})(?::?([0-9]{2}))?)?", x, perl = TRUE))[[1]]
  if (length(m) == 0) return(NA_real_)
  if (!nzchar(m[2])) return(0)
  sign <- ifelse(m[2] == "-", -1, 1)
  minutes <- if (nzchar(m[4])) as.numeric(m[4]) else 0
  sign * (as.numeric(m[3]) + minutes / 60)
}

#' Convierte una fecha "de reloj" con un desfase conocido en un instante UTC
#' @noRd
parse_local_datetime <- function(text, format, utc_offset_h) {
  t <- as.POSIXct(text, format = format, tz = "UTC")
  if (is.na(utc_offset_h)) utc_offset_h <- 0
  t - utc_offset_h * 3600
}
