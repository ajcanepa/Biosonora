#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # ---- Base de datos (una conexión por sesión) ----
  con <- db_connect()
  session$onSessionEnded(function() DBI::dbDisconnect(con))

  # ---- Idioma ----
  lang <- reactive({
    l <- input$lang %||% default_language
    if (l %in% supported_languages) l else default_language
  })
  observeEvent(lang(), send_language(session, lang()))

  # ---- Archivos que el navegador puede descargar ----
  # Audio de reproducción: carpeta temporal propia de esta sesión (se borra al salir)
  media_dir <- tempfile("biosonora_media_")
  dir.create(media_dir)
  media_prefix <- paste0("media-", session$token)
  addResourcePath(media_prefix, media_dir)
  # Espectrogramas: caché compartida en la carpeta de datos de la app
  cache_dir <- file.path(app_data_dir(), "cache_espectrogramas")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  addResourcePath("spectrograms", cache_dir)
  session$onSessionEnded(function() {
    removeResourcePath(media_prefix)
    unlink(media_dir, recursive = TRUE)
  })
  media <- list(dir = media_dir, url = media_prefix,
                cache_dir = cache_dir, cache_url = "spectrograms")

  # ---- Módulos ----
  data_changed <- reactiveVal(0)
  mod_load_server("load", con, lang, data_changed)
  tracks <- mod_track_list_server("tracks", con, lang, data_changed)
  key_event <- reactive(input$biosonora_key)
  nav <- mod_viewer_server("viewer", con, lang, tracks$current, media, data_changed, key_event)

  # Botones y teclas "siguiente / anterior" del visor mueven la lista
  observeEvent(nav(), tracks$move(nav()$step))
}
