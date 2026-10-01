# ---------------------------------------------------------------------------
# Iniciar Biosonora
#
# En RStudio: abre este archivo y pulsa el botón «Source» (arriba a la derecha).
# Desde una terminal, en la carpeta del proyecto:  Rscript iniciar_biosonora.R
#
# La primera vez instala los paquetes necesarios con las versiones exactas del
# proyecto (renv); puede tardar varios minutos. Las siguientes veces es rápido.
# ---------------------------------------------------------------------------

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}
# Instala solo lo que falte (no hace nada si ya está todo)
renv::restore(prompt = FALSE)

pkgload::load_all(quiet = TRUE)
options(shiny.launch.browser = TRUE)
shiny::runApp(run_app())
