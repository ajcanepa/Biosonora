# Traducciones (español / inglés).
#
# Todos los textos de la interfaz están en inst/i18n/<idioma>.yml, con claves
# jerárquicas ("list.title" = sección "list", clave "title"). Nunca se escriben
# textos directamente en el código.
#
# - tr("clave", lang) devuelve el texto traducido (con {marcadores} rellenados).
# - i18n("clave") crea un <span> que el navegador vuelve a traducir cuando el
#   usuario cambia de idioma, sin recargar la página.

supported_languages <- c("es", "en")
default_language <- "es"

# Caché de los diccionarios ya leídos
i18n_cache <- new.env(parent = emptyenv())

#' Diccionario plano (clave -> texto) de un idioma
#' @noRd
i18n_dictionary <- function(lang = default_language) {
  if (!lang %in% supported_languages) lang <- default_language
  if (is.null(i18n_cache[[lang]])) {
    file <- app_sys("i18n", paste0(lang, ".yml"))
    i18n_cache[[lang]] <- flatten_keys(yaml::read_yaml(file))
  }
  i18n_cache[[lang]]
}

# Convierte list(a = list(b = "x")) en c(a.b = "x")
flatten_keys <- function(x, prefix = NULL) {
  out <- character()
  for (name in names(x)) {
    key <- if (is.null(prefix)) name else paste0(prefix, ".", name)
    if (is.list(x[[name]])) {
      out <- c(out, flatten_keys(x[[name]], key))
    } else {
      out[key] <- as.character(x[[name]])
    }
  }
  out
}

#' Traduce una clave
#'
#' @param key Clave, p. ej. "load.scan_button".
#' @param lang Idioma ("es" o "en").
#' @param ... Valores para los marcadores, p. ej. tr("x", n = 3) rellena "{n}".
#' @return Texto traducido. Si la clave no existe, devuelve la propia clave
#'   (así se detecta enseguida qué falta).
#' @noRd
tr <- function(key, lang = default_language, ...) {
  dict <- i18n_dictionary(lang)
  txt <- dict[key]
  if (is.na(txt)) {
    txt <- i18n_dictionary(default_language)[key]
    if (is.na(txt)) return(key)
  }
  values <- list(...)
  for (name in names(values)) {
    txt <- gsub(paste0("{", name, "}"), as.character(values[[name]]), txt, fixed = TRUE)
  }
  unname(txt)
}

#' Traduce una lista de valores con un prefijo de clave
#'
#' tr_values("validation.", c("correct", "pending")) -> c("Correcta", "Sin revisar").
#' Con una lista vacía devuelve character() (paste0 devolvería el prefijo solo).
#' @noRd
tr_values <- function(prefix, values, lang = default_language) {
  if (length(values) == 0) return(character())
  vapply(paste0(prefix, values), tr, "", lang = lang, USE.NAMES = FALSE)
}

#' Texto de interfaz traducible en vivo
#'
#' Crea <span data-i18n="clave">texto</span>. El script i18n.js cambia el
#' texto cuando el usuario elige otro idioma.
#' @noRd
i18n <- function(key, lang = default_language) {
  shiny::tags$span(`data-i18n` = key, tr(key, lang))
}

#' Envía al navegador el diccionario del idioma elegido
#' @noRd
send_language <- function(session, lang) {
  session$sendCustomMessage("biosonora-set-lang", list(
    lang = lang,
    dict = as.list(i18n_dictionary(lang))
  ))
}
