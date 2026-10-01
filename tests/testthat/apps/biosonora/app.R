# App de prueba para shinytest2: carga el paquete desde el código fuente y
# analiza datos_ejemplo/ al arrancar. Los datos van a una carpeta temporal
# (BIOSONORA_DATA_DIR), nunca a la base de datos real del usuario.
root <- normalizePath(file.path("..", "..", "..", ".."))
pkgload::load_all(root, quiet = TRUE, export_all = TRUE)
run_app(folder = file.path(root, "datos_ejemplo"))
