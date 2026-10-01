# Crea inst/extdata/taxonomia/ioc_15.2_aves.csv a partir de la Lista IOC de Aves
# del Mundo (https://www.worldbirdnames.org), que es la que usa Observation.org.
#
# Licencia de la lista: Creative Commons Attribution 3.0 Unported.
#   IOC World Bird List v15.2 by Frank Gill, David Donsker & Pamela Rasmussen (Eds)
#
# Este script solo hace falta al cambiar de versión de la lista. Necesita el
# paquete readxl (no es dependencia de la app). Ejecutar desde la raíz:
#   Rscript --no-init-file data-raw/crear_taxonomia_ioc.R
# (--no-init-file: usa la librería general de R, donde está readxl)
#
# Fuentes:
#  - "Multilingual Version": nombre científico IOC y nombres comunes
#  - "Comparison with other world lists": equivalencias con eBird/Clements
#    (la taxonomía que usa BirdNET), fila a fila para el mismo taxón

version <- "15.2"
tmp <- tempfile()
dir.create(tmp)
multiling <- file.path(tmp, "multiling.xlsx")
comparison <- file.path(tmp, "comparison.xlsx")
download.file(sprintf("https://worldbirdnames.org/Multiling%%20IOC%%20%s.xlsx", version), multiling, mode = "wb")
download.file(sprintf("https://www.worldbirdnames.org/IOC_%s_vs_other_lists.xlsx", version), comparison, mode = "wb")

# Nombres científicos (especies) y comunes en inglés y español
ml <- readxl::read_excel(multiling, sheet = "List")
names_df <- data.frame(
  ioc_name = trimws(ml[[paste0("IOC_", version)]]),
  english = trimws(ml$English),
  spanish = trimws(ml$Spanish),
  stringsAsFactors = FALSE
)
names_df <- names_df[!is.na(names_df$ioc_name), ]

# Equivalencias Clements -> IOC (solo nombres de especie, dos palabras)
cmp <- readxl::read_excel(comparison, sheet = 1)
ioc_col <- grep(paste0("IOC World Bird List \\(v", version, "\\)"), names(cmp), value = TRUE)[1]
clem_col <- grep("^Clements", names(cmp), value = TRUE)[1]
species_part <- function(x) {
  x <- trimws(x)
  ifelse(is.na(x), NA_character_, sub("^(\\S+\\s+\\S+).*$", "\\1", x))
}
eq <- data.frame(clements = species_part(cmp[[clem_col]]), ioc = species_part(cmp[[ioc_col]]),
                 stringsAsFactors = FALSE)
eq <- unique(eq[!is.na(eq$clements) & !is.na(eq$ioc), ])
# Si un nombre de Clements corresponde a varios de IOC (divisiones), se usa
# la fila de especie (la primera); el usuario puede cambiarlo en la app
eq <- eq[!duplicated(eq$clements), ]

# Nombres de Clements que no son también nombres IOC (para la traducción automática)
extra <- eq[!eq$clements %in% names_df$ioc_name & eq$ioc %in% names_df$ioc_name, ]
cat("Especies IOC:", nrow(names_df), "· equivalencias Clements distintas de IOC:", nrow(extra), "\n")

taxonomy <- names_df[, c("ioc_name", "english", "spanish")]
write.csv(taxonomy, "inst/extdata/taxonomia/ioc_15.2_aves.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data.frame(clements_name = extra$clements, ioc_name = extra$ioc),
          "inst/extdata/taxonomia/clements_a_ioc_15.2.csv", row.names = FALSE, fileEncoding = "UTF-8")
