# Fase 1 — MVP local

Fecha: 2026-10-01

## Qué incluye

- Análisis de una carpeta (con subcarpetas): lectura de metadatos sin cargar el audio.
- Detección de grabadora: AudioMoth (GUANO / comentario), manual con BWF/bext
  (TASCAM u otra), desconocida (el usuario indica el tipo y modelo).
- Fechas en UTC en la base de datos; se muestran en hora local (Europe/Madrid)
  indicando siempre la zona. Se avisa cuando la fecha es poco fiable.
- Duplicados (huella del audio) y archivos renombrados (" 1.WAV").
- Base de datos SQLite fuera de OneDrive (`tools::R_user_dir("biosonora", "data")`).
- Lista de pistas con filtros, ordenación, búsqueda, paginación y estado de revisión.
- Asignar punto de muestreo, coordenadas y observador a varias pistas a la vez.
- Visor: espectrograma calculado en R (caché de PNG) dentro de wavesurfer.js,
  forma de onda, cursor, clic para saltar, zoom, rango de frecuencias, contraste,
  paletas viridis / magma / grises.
- Reproducción a 1x, 0,5x, 0,25x y 0,1x por expansión temporal (permite oír
  ultrasonidos de murciélago).
- Atajos de teclado, interfaz en español e inglés, modo oscuro.

## Decisiones técnicas

- **Sin el paquete `av`:** necesita `libavfilter-dev` (sudo). El espectrograma se
  calcula con `tuneR` + FFT de R base: ~2,3 s para 55 s a 250 kHz.
- **Audio como blob:** el servidor de archivos de Shiny no admite peticiones
  parciales (HTTP Range) y sin ellas el navegador no puede saltar dentro del
  audio. El navegador descarga el WAV de reproducción completo (~9 MB para una
  pista AudioMoth a 1x). Revisar en la fase de servidor.
- **wavesurfer.js 7.12.12** (BSD-3) en `inst/app/vendor/`, fuera de `www/` para
  controlar el orden de carga.

## Pendiente / limitaciones conocidas

- El cálculo del espectrograma bloquea la sesión unos segundos (se hará asíncrono
  más adelante).
- La fecha de grabaciones sin metadatos no se puede corregir aún desde la app.
- FLAC no está soportado todavía.
- La división de pistas largas en segmentos queda para más adelante.
