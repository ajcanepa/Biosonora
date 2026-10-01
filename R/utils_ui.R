# Utilidades pequeñas para la interfaz

#' Valor por defecto si x es NULL o vacío
#' @noRd
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Añade un atributo traducible (placeholder, title, aria-label) a una etiqueta
#'
#' Ejemplo: i18n_attr(textInput(...), "placeholder", "load.folder_placeholder")
#' El atributo se busca en el <input> interno si existe.
#' @noRd
i18n_attr <- function(tag, attr, key, lang = default_language) {
  q <- htmltools::tagQuery(tag)
  target <- q$find("input")
  if (length(target$selectedTags()) == 0) target <- q
  attrs <- list(tr(key, lang), key)
  names(attrs) <- c(attr, paste0("data-i18n-", attr))
  do.call(target$addAttrs, attrs)
  target$allTags()
}

#' Botón que solo tiene icono: añade texto para lectores de pantalla y
#' descripción al pasar el ratón (ambos traducibles)
#' @noRd
icon_button <- function(tag, key) {
  i18n_attr(i18n_attr(tag, "title", key), "aria-label", key)
}

#' Duración en formato m:ss
#' @noRd
format_duration <- function(seconds) {
  seconds <- round(seconds)
  out <- sprintf("%d:%02d", seconds %/% 60, seconds %% 60)
  out[is.na(seconds)] <- ""
  out
}

#' Modo de ejecución ("local" o "server") según la configuración
#' @noRd
run_mode <- function() {
  tryCatch(get_golem_config("run_mode"), error = function(e) "local") %||% "local"
}

#' Zona horaria local según la configuración
#' @noRd
local_timezone <- function() {
  tryCatch(get_golem_config("local_timezone"), error = function(e) "Europe/Madrid") %||%
    "Europe/Madrid"
}
