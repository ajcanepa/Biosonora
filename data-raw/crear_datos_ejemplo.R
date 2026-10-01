# Crea los archivos de audio sintéticos de datos_ejemplo/ (sin voces ni datos
# personales: solo un tono). Ejecutar desde la raíz del proyecto con:
#   Rscript data-raw/crear_datos_ejemplo.R
# Reproducen la estructura de metadatos de grabaciones reales:
#   - AudioMoth (firmware 1.12.1): comentario ICMT, IART y bloque GUANO
#   - Grabadora manual con bloque BWF/bext (TASCAM)
#   - Grabación sin metadatos (grabadora desconocida)

`%||%` <- function(x, y) if (is.null(x)) y else x
source("tests/testthat/helper-wav.R")
dev <- "24BBD608675B68F7" # el del CONFIG.TXT de ejemplo
am <- "datos_ejemplo/audiomoth"

# Grabación normal (GUANO + comentario)
write_test_wav(file.path(am, "20260810", paste0(dev, "_20260810_160000.WAV")),
  tone_hz = 1500,
  info = audiomoth_info(dev, "16:00:00", "10/08/2026"),
  guano = audiomoth_guano(dev, "2026-08-10T16:00:00Z", paste0(dev, "_20260810_160000.WAV")))
# Copia idéntica con el nombre cambiado (duplicado)
file.copy(file.path(am, "20260810", paste0(dev, "_20260810_160000.WAV")),
          file.path(am, "20260810", paste0(dev, "_20260810_160000 1.WAV")), overwrite = TRUE)
# Renombrada sin original (nombre modificado, pero no duplicada)
write_test_wav(file.path(am, "20260810", paste0(dev, "_20260810_160100 1.WAV")),
  tone_hz = 2500,
  info = audiomoth_info(dev, "16:01:00", "10/08/2026", battery = "4.7"),
  guano = audiomoth_guano(dev, "2026-08-10T16:01:00Z", paste0(dev, "_20260810_160100.WAV"), battery = "4.7"))
# Firmware antiguo: solo comentario (sin GUANO)
write_test_wav(file.path(am, "20260811", paste0(dev, "_20260811_160000.WAV")),
  tone_hz = 3000,
  info = audiomoth_info(dev, "16:00:00", "11/08/2026", battery = "4.6", temp = "18.0"))

# Grabadora manual con BWF/bext (fecha en hora local, estéreo, 24 bits)
write_test_wav("datos_ejemplo/tascam/DR0000_0001.wav", sample_rate = 48000, channels = 2,
  bits = 24, tone_hz = 4000,
  bext = list(originator = "TASCAM DR-05X", date = "2025-04-10", time = "17:53:05"))

# Sin metadatos: grabadora desconocida
write_test_wav("datos_ejemplo/desconocida/grabacion_sin_metadatos.wav",
  sample_rate = 44100, tone_hz = 2000)

# Un archivo con extensión .wav que no es audio
dir.create("datos_ejemplo/otros", showWarnings = FALSE)
writeLines(rep("Esto no es un archivo de audio.", 5), "datos_ejemplo/otros/no_es_audio.wav")
