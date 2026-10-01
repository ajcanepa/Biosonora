#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # ---- Espacio de datos y base de datos (una conexión por sesión) ----
  # En local hay un único espacio; en modo servidor, uno por usuario.
  data_dir <- app_data_dir(user_space(session))
  con <- db_connect(file.path(data_dir, "biosonora.sqlite"))
  options(shiny.maxRequestSize = max_upload_mb() * 1024^2)
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
  reviewer <- mod_reviewer_server("reviewer", lang)
  mod_load_server("load", con, lang, data_changed, data_dir)
  tracks <- mod_track_list_server("tracks", con, lang, data_changed)
  mod_birdnet_import_server("birdnet", con, lang, tracks$current, reviewer, data_changed)
  mod_birdnet_run_server("birdnet_run", con, lang, tracks, reviewer, data_changed)

  # Eventos que envía el navegador (viewer.js): teclas, clic en una detección
  # y rectángulo dibujado para una anotación nueva
  key_event <- reactive(input$biosonora_key)
  nav <- mod_viewer_server("viewer", con, lang, tracks$current, media, data_changed,
                           key_event, reviewer)
  mod_detections_server(
    "detections", con, lang, tracks$current, tracks$min_confidence, reviewer, data_changed,
    player_id = "viewer-player",
    events = list(key = key_event,
                  box_click = reactive(input$biosonora_box_click),
                  new_box = reactive(input$biosonora_new_box))
  )
  mod_species_list_server("species", con, lang, reviewer, data_changed)
  mod_export_server("export", con, lang, reviewer, data_changed)
  mod_summary_server("summary", con, lang, dark = reactive(identical(input$dark_mode, "dark")),
                     data_changed)
  mod_help_server("help", lang)

  # Botones y teclas "siguiente / anterior" del visor mueven la lista
  observeEvent(nav(), tracks$move(nav()$step))
}
