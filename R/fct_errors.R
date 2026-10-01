# Errores comprensibles para el usuario.
# En lugar de mostrar errores técnicos de R, las funciones lanzan un error con
# una clave de traducción (p. ej. "error.wav_not_riff") y datos opcionales.
# La interfaz traduce la clave al idioma elegido.

#' Lanza un error de Biosonora con una clave de traducción
#'
#' @param key Clave de traducción del mensaje (ver inst/i18n/).
#' @param ... Datos para rellenar el mensaje (p. ej. file = "x.wav").
#' @noRd
biosonora_abort <- function(key, ...) {
  cond <- structure(
    class = c("biosonora_error", "error", "condition"),
    list(message = key, call = NULL, key = key, data = list(...))
  )
  stop(cond)
}

#' Convierte cualquier error en un mensaje traducido
#'
#' Los errores de Biosonora se traducen con su clave; los errores inesperados
#' se registran en la consola (para depurar) y se muestra un mensaje genérico.
#'
#' @param e Condición capturada con tryCatch().
#' @param lang Idioma ("es" o "en").
#' @noRd
error_to_message <- function(e, lang = "es") {
  if (inherits(e, "biosonora_error")) {
    return(do.call(tr, c(list(key = e$key, lang = lang), e$data)))
  }
  message("[biosonora] Error inesperado: ", conditionMessage(e))
  tr("error.unexpected", lang = lang)
}
