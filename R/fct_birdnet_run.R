# Ejecutar BirdNET-Analyzer desde la app, en segundo plano.
#
# BirdNET es un programa de Python. Se instala aparte con uv (ver
# python/pyproject.toml) en la carpeta de datos de la app, fuera de OneDrive.
# La app lo lanza como un PROCESO SEPARADO (paquete processx): R no espera
# a que termine, así que la interfaz sigue respondiendo; cada segundo se
# consulta cuánto lleva.
#
# Seguridad: los argumentos se pasan como un vector a processx, nunca como un
# texto de comando, así que ningún dato del usuario se interpreta como orden.
#
# Las grabaciones con distinto punto de muestreo o semana necesitan distintos
# parámetros (--lat, --lon, --week): se agrupan en "lotes" que se ejecutan uno
# tras otro.
#
# Licencia de los modelos de BirdNET: CC BY-NC-SA 4.0 (solo uso no comercial).

#' Carpeta del entorno de Python de BirdNET
#'
#' Por defecto, en la carpeta de datos del usuario (fuera de OneDrive). Se
#' puede cambiar con la variable de entorno BIOSONORA_BIRDNET_ENV.
#' @noRd
birdnet_env_dir <- function() {
  dir <- Sys.getenv("BIOSONORA_BIRDNET_ENV", "")
  if (!nzchar(dir)) dir <- file.path(tools::R_user_dir("biosonora", "data"), "python_env")
  dir
}

#' Ruta del Python de BirdNET (NULL si no está instalado)
#'
#' Se puede indicar otra en golem-config.yml (birdnet: python_path).
#' @noRd
birdnet_python <- function() {
  configured <- tryCatch(get_golem_config("birdnet")$python_path, error = function(e) NULL)
  candidates <- c(
    configured,
    file.path(birdnet_env_dir(), "bin", "python"),
    file.path(birdnet_env_dir(), "Scripts", "python.exe") # Windows
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  hit <- candidates[file.exists(candidates)]
  # Sin normalizePath(): seguiría el enlace del entorno hasta el Python base,
  # que no tiene BirdNET instalado
  if (length(hit)) path.expand(hit[1]) else NULL
}

#' Archivo dentro del paquete birdnet_analyzer instalado (NULL si no está)
#' @noRd
birdnet_package_file <- function(...) {
  env <- birdnet_env_dir()
  dirs <- c(Sys.glob(file.path(env, "lib", "python3*", "site-packages", "birdnet_analyzer")),
            file.path(env, "Lib", "site-packages", "birdnet_analyzer"))
  files <- file.path(dirs, ...)
  files <- files[file.exists(files)]
  if (length(files)) files[1] else NULL
}

#' Semana del año al estilo de BirdNET: 48 semanas, 4 por mes (1 a 48)
#' @noRd
birdnet_week <- function(date) {
  date <- as.Date(date)
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  day <- as.integer(format(date, "%d"))
  first <- as.Date(sprintf("%d-%02d-01", year, month))
  next_first <- as.Date(sprintf("%d-%02d-01", ifelse(month == 12, year + 1L, year),
                                ifelse(month == 12, 1L, month + 1L)))
  days_in_month <- as.integer(next_first - first)
  (month - 1L) * 4L + pmin(4L, as.integer(ceiling(day * 4 / days_in_month)))
}

#' Argumentos de BirdNET-Analyzer para un lote (vector, nunca texto de comando)
#'
#' @param params Lista con min_confidence, overlap, sensitivity, locale y threads.
#' @param lat,lon Coordenadas (NA = sin filtro geográfico).
#' @param week Semana BirdNET (NA = todo el año).
#' @noRd
birdnet_args <- function(input_dir, output_dir, params, lat = NA, lon = NA, week = NA) {
  num <- function(x) format(x, scientific = FALSE, trim = TRUE)
  args <- c("-m", "birdnet_analyzer.analyze", input_dir,
            "-o", output_dir,
            "--min_conf", num(params$min_confidence),
            "--overlap", num(params$overlap),
            "--sensitivity", num(params$sensitivity),
            "-l", params$locale,
            "-t", num(params$threads),
            "--rtype", "csv")
  if (!is.na(lat) && !is.na(lon)) args <- c(args, "--lat", num(lat), "--lon", num(lon))
  if (!is.na(week)) args <- c(args, "--week", num(week))
  args
}

#' Agrupa las grabaciones en lotes con los mismos parámetros de BirdNET
#'
#' @param recordings data.frame con recording_id, file_path, start_utc, lat, lon.
#' @param use_location,use_week Si se usan las coordenadas del punto y la semana.
#' @return Lista de lotes (recording_ids, lat, lon, week).
#' @noRd
birdnet_batches <- function(recordings, use_location = TRUE, use_week = TRUE,
                            local_tz = "Europe/Madrid") {
  lat <- if (use_location) round(recordings$lat, 4) else rep(NA, nrow(recordings))
  lon <- if (use_location) round(recordings$lon, 4) else rep(NA, nrow(recordings))
  week <- if (use_week) {
    birdnet_week(as.Date(format(recordings$start_utc, tz = local_tz, "%Y-%m-%d")))
  } else {
    rep(NA, nrow(recordings))
  }
  key <- paste(lat, lon, week)
  lapply(split(seq_len(nrow(recordings)), key), function(i) {
    list(recording_ids = recordings$recording_id[i], file_paths = recordings$file_path[i],
         lat = lat[i[1]], lon = lon[i[1]], week = week[i[1]])
  })
}

#' Carpeta de entrada con enlaces a las grabaciones de un lote
#'
#' BirdNET analiza carpetas enteras; se crea una con enlaces (o copias si el
#' sistema no permite enlaces). Cada enlace lleva delante el identificador de
#' la grabación para que no choquen nombres repetidos y para asociar después
#' los resultados sin ambigüedad.
#' @return data.frame con link_name y recording_id
#' @noRd
prepare_birdnet_input <- function(batch, input_dir) {
  dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
  link_names <- paste0("rec", batch$recording_ids, "__", basename(batch$file_paths))
  for (k in seq_along(link_names)) {
    target <- file.path(input_dir, link_names[k])
    ok <- suppressWarnings(file.symlink(batch$file_paths[k], target))
    if (!ok) file.copy(batch$file_paths[k], target)
  }
  data.frame(link_name = link_names, recording_id = batch$recording_ids, stringsAsFactors = FALSE)
}

#' Crea un trabajo de BirdNET (todavía no lo lanza)
#'
#' @param recordings Grabaciones a analizar (ver birdnet_batches()).
#' @param params Parámetros de BirdNET.
#' @return Entorno con el estado del trabajo (se modifica al avanzar).
#' @noRd
new_birdnet_job <- function(recordings, params, use_location = TRUE, use_week = TRUE,
                            local_tz = "Europe/Madrid", work_dir = tempfile("birdnet_job_")) {
  python <- birdnet_python()
  if (is.null(python)) biosonora_abort("error.birdnet_not_installed")
  recordings <- recordings[file.exists(recordings$file_path), , drop = FALSE]
  if (nrow(recordings) == 0) biosonora_abort("error.birdnet_no_recordings")
  job <- new.env()
  job$python <- python
  job$params <- params
  job$batches <- birdnet_batches(recordings, use_location, use_week, local_tz)
  job$work_dir <- work_dir
  job$total <- nrow(recordings)
  job$done <- 0L
  job$current <- 0L
  job$process <- NULL
  job$status <- "pending"     # pending, running, finished, failed, cancelled
  job$links <- list()
  job$stdout_lines <- character()
  job$stderr_file <- NA_character_
  job$started_at <- Sys.time()
  job
}

# Lanza el siguiente lote
start_next_batch <- function(job) {
  job$current <- job$current + 1L
  batch <- job$batches[[job$current]]
  input_dir <- file.path(job$work_dir, sprintf("lote%02d_entrada", job$current))
  output_dir <- file.path(job$work_dir, sprintf("lote%02d_resultados", job$current))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  job$links[[job$current]] <- prepare_birdnet_input(batch, input_dir)
  job$stderr_file <- file.path(job$work_dir, sprintf("lote%02d_errores.txt", job$current))
  job$output_dir <- output_dir
  job$process <- processx::process$new(
    job$python,
    birdnet_args(input_dir, output_dir, job$params, batch$lat, batch$lon, batch$week),
    stdout = "|", stderr = job$stderr_file, cleanup = TRUE,
    # Sin esto, Python guarda la salida y solo la envía al final: no habría progreso
    env = c("current", PYTHONUNBUFFERED = "1")
  )
  job$status <- "running"
}

#' Hace avanzar un trabajo: lanza lotes y cuenta archivos terminados
#'
#' Se llama cada segundo desde la interfaz. Nunca espera al proceso.
#' @return El propio trabajo (con status, done y total actualizados).
#' @noRd
poll_birdnet_job <- function(job) {
  if (job$status %in% c("finished", "failed", "cancelled")) return(job)
  if (is.null(job$process)) {
    start_next_batch(job)
    return(job)
  }
  # BirdNET escribe "Finished <archivo> in X seconds" al terminar cada audio
  lines <- tryCatch(job$process$read_output_lines(), error = function(e) character())
  job$stdout_lines <- utils::tail(c(job$stdout_lines, lines), 200)
  job$done <- job$done + sum(grepl("^Finished ", lines))
  if (job$process$is_alive()) return(job)

  if (job$process$get_exit_status() != 0) {
    job$status <- "failed"
    message("[biosonora] BirdNET falló. Detalles en: ", job$stderr_file)
    return(job)
  }
  if (job$current < length(job$batches)) {
    start_next_batch(job)
  } else {
    job$done <- job$total
    job$status <- "finished"
  }
  job
}

#' Cancela un trabajo en marcha
#' @noRd
cancel_birdnet_job <- function(job) {
  if (!is.null(job$process) && job$process$is_alive()) job$process$kill()
  job$status <- "cancelled"
  job
}

#' Importa los resultados de un trabajo terminado
#'
#' Asocia cada archivo de resultados a su grabación por el enlace creado al
#' preparar la entrada (rec<ID>__nombre).
#' @return Número de detecciones nuevas.
#' @noRd
import_birdnet_job <- function(con, job, user = NA_character_) {
  total <- 0L
  for (k in seq_len(job$current)) {
    output_dir <- file.path(job$work_dir, sprintf("lote%02d_resultados", k))
    files <- list.files(output_dir, pattern = "\\.BirdNET\\.results\\.csv$", full.names = TRUE,
                        recursive = TRUE)
    links <- job$links[[k]]
    for (f in files) {
      det <- read_birdnet_file(f)
      if (nrow(det) == 0) next
      det$recording_id <- links$recording_id[match(basename(det$file_ref), links$link_name)]
      total <- total + db_import_detections(con, det, basename(f), "analyzer_csv",
                                            classifier = "BirdNET-Analyzer 2.4.0", user = user)
    }
  }
  total
}
