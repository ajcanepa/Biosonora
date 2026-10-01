# Subida segura de grabaciones (WAV sueltos o un ZIP) y carpetas del servidor.
#
# Comprobaciones de cada archivo subido:
#   1. Extensión permitida: .wav, .zip o CONFIG.TXT (de AudioMoth).
#   2. Tipo REAL del archivo (sus primeros bytes), no solo la extensión.
#   3. Tamaño máximo (max_upload_mb en golem-config.yml).
#   4. Nombre saneado: nunca se usa el nombre que envía el navegador para
#      construir rutas.
#   5. ZIP: se rechazan rutas absolutas o con ".." (evita escribir fuera de la
#      carpeta, ataque "zip slip") y se limita el tamaño descomprimido
#      (evita las "bombas ZIP").
# Que el audio se pueda leer se comprueba después, al analizar la carpeta.

#' Tamaño máximo de subida en MB (configuración)
#' @noRd
max_upload_mb <- function() {
  as.numeric(tryCatch(get_golem_config("max_upload_mb"), error = function(e) NULL) %||% 2000)
}

#' Nombre de archivo seguro: sin carpetas, sin caracteres especiales
#' @noRd
sanitize_filename <- function(x) {
  x <- basename(gsub("\\\\", "/", x))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  x <- gsub("[^A-Za-z0-9._ -]", "_", x)
  x <- gsub("\\s+", " ", trimws(x))
  x <- sub("^[.]+", "", x)          # sin archivos ocultos ni "..", "."
  x <- substr(x, 1, 120)
  x[!nzchar(x)] <- "archivo"
  x
}

#' Tipo real de un archivo según sus primeros bytes: "wav", "zip" o NA
#' @noRd
sniff_file_type <- function(path) {
  head <- tryCatch(readBin(path, "raw", n = 12), error = function(e) raw())
  if (length(head) >= 12 && rawToChar(head[1:4]) == "RIFF" && rawToChar(head[9:12]) == "WAVE") {
    return("wav")
  }
  if (length(head) >= 4 && identical(head[1:4], as.raw(c(0x50, 0x4B, 0x03, 0x04)))) return("zip")
  NA_character_
}

is_config_name <- function(name) grepl("^config\\.txt$", basename(name), ignore.case = TRUE)

#' Guarda los archivos subidos en una carpeta nueva, validándolos
#'
#' @param files data.frame de fileInput (name, size, datapath).
#' @param dest_dir Carpeta de destino (se crea).
#' @param max_mb Tamaño máximo por archivo subido y descomprimido.
#' @return Lista con dest_dir, stored (rutas guardadas) y rejected
#'   (data.frame name + reason, con la clave de traducción del motivo).
#' @noRd
store_uploads <- function(files, dest_dir, max_mb = max_upload_mb()) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  stored <- character()
  rejected <- data.frame(name = character(), reason = character(), stringsAsFactors = FALSE)
  reject <- function(name, reason) {
    rejected[nrow(rejected) + 1, ] <<- list(name, reason)
  }
  max_bytes <- max_mb * 1024^2

  for (i in seq_len(nrow(files))) {
    name <- files$name[i]
    path <- files$datapath[i]
    ext <- tolower(tools::file_ext(name))
    if (!ext %in% c("wav", "zip") && !is_config_name(name)) { reject(name, "upload.bad_extension"); next }
    if (isTRUE(file.size(path) > max_bytes)) { reject(name, "upload.too_big"); next }

    if (is_config_name(name)) {
      stored <- c(stored, safe_copy(path, dest_dir, "CONFIG.TXT"))
      next
    }
    type <- sniff_file_type(path)
    if (is.na(type) || type != ext) { reject(name, "upload.bad_content"); next }
    if (type == "wav") {
      stored <- c(stored, safe_copy(path, dest_dir, sanitize_filename(name)))
    } else {
      res <- extract_zip_safely(path, dest_dir, max_bytes)
      stored <- c(stored, res$stored)
      if (nrow(res$rejected)) {
        res$rejected$name <- paste0(name, ": ", res$rejected$name)
        rejected <- rbind(rejected, res$rejected)
      }
    }
  }
  list(dest_dir = dest_dir, stored = stored, rejected = rejected)
}

# Copia sin sobrescribir: si el nombre ya existe, añade _2, _3...
safe_copy <- function(path, dir, name) {
  target <- unique_path(file.path(dir, name))
  file.copy(path, target)
  target
}

unique_path <- function(path) {
  if (!file.exists(path)) return(path)
  stem <- tools::file_path_sans_ext(path)
  ext <- tools::file_ext(path)
  k <- 2
  repeat {
    candidate <- paste0(stem, "_", k, if (nzchar(ext)) paste0(".", ext))
    if (!file.exists(candidate)) return(candidate)
    k <- k + 1
  }
}

#' Extrae de un ZIP solo WAV y CONFIG.TXT, de forma segura
#'
#' Se conserva la estructura de subcarpetas (con nombres saneados) para que
#' cada WAV de AudioMoth se pueda asociar a su CONFIG.TXT.
#' @noRd
extract_zip_safely <- function(zip_path, dest_dir, max_bytes) {
  rejected <- data.frame(name = character(), reason = character(), stringsAsFactors = FALSE)
  entries <- tryCatch(utils::unzip(zip_path, list = TRUE), error = function(e) NULL)
  if (is.null(entries)) {
    rejected[1, ] <- list(basename(zip_path), "upload.bad_zip")
    return(list(stored = character(), rejected = rejected))
  }
  entries <- entries[!grepl("/$", entries$Name), , drop = FALSE] # sin carpetas
  dangerous <- grepl("^(/|[A-Za-z]:)", entries$Name) | grepl("(^|/)\\.\\.(/|$)", entries$Name)
  wanted <- grepl("\\.wav$", entries$Name, ignore.case = TRUE) | is_config_name(entries$Name)
  for (n in entries$Name[dangerous]) rejected[nrow(rejected) + 1, ] <- list(n, "upload.dangerous_path")
  for (n in entries$Name[!dangerous & !wanted]) rejected[nrow(rejected) + 1, ] <- list(n, "upload.bad_extension")
  entries <- entries[!dangerous & wanted, , drop = FALSE]
  if (sum(entries$Length) > max_bytes) {
    rejected[nrow(rejected) + 1, ] <- list(basename(zip_path), "upload.zip_too_big")
    return(list(stored = character(), rejected = rejected))
  }

  tmp <- tempfile("zip_")
  on.exit(unlink(tmp, recursive = TRUE))
  stored <- character()
  for (n in entries$Name) {
    utils::unzip(zip_path, files = n, exdir = tmp, junkpaths = FALSE)
    extracted <- file.path(tmp, n)
    if (!is_config_name(n) && !identical(sniff_file_type(extracted), "wav")) {
      rejected[nrow(rejected) + 1, ] <- list(n, "upload.bad_content")
      next
    }
    parts <- strsplit(dirname(n), "/", fixed = TRUE)[[1]]
    parts <- sanitize_filename(parts[parts != "."])
    target_dir <- do.call(file.path, as.list(c(dest_dir, parts)))
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    stored <- c(stored, safe_copy(extracted, target_dir,
                                  if (is_config_name(n)) "CONFIG.TXT" else sanitize_filename(n)))
  }
  list(stored = stored, rejected = rejected)
}

#' Carpetas del servidor que el administrador permite analizar
#'
#' En modo servidor nunca se acepta una ruta escrita por el usuario: solo se
#' elige entre las que figuran en golem-config.yml (server_folders).
#' @return data.frame name, path (solo las que existen)
#' @noRd
server_folders <- function() {
  cfg <- tryCatch(get_golem_config("server_folders"), error = function(e) NULL)
  if (length(cfg) == 0) return(data.frame(name = character(), path = character()))
  df <- data.frame(name = vapply(cfg, function(x) x$name %||% x$path, ""),
                   path = vapply(cfg, function(x) path.expand(x$path), ""),
                   stringsAsFactors = FALSE)
  df[dir.exists(df$path), , drop = FALSE]
}

#' Espacio de datos de cada usuario
#'
#' En local, cada persona usa su propio ordenador: un único espacio. En modo
#' servidor, cada usuario identificado (inicio de sesión) tiene su carpeta,
#' su base de datos y sus subidas.
#' @noRd
user_space <- function(session) {
  if (run_mode() != "server" || is.null(session$user) || !nzchar(session$user)) return(NULL)
  gsub("[^A-Za-z0-9._-]", "_", session$user)
}
