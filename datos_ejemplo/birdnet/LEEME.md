# Resultados de BirdNET de ejemplo

- `Blue_Jay_Sample_BirdNET_Results.csv`: archivo **real** (formato sencillo:
  start, end, scientific_name, common_name, confidence). No indica el audio.
- `24BBD608675B68F7_20260810_160000.BirdNET.results.csv`: CSV de BirdNET-Analyzer,
  con la misma estructura que genera la versión 2.4.0 (columna `File`, aquí con una
  ruta de Windows para probar la asociación por nombre).
- `24BBD608675B68F7_20260810_160000.BirdNET.selection.table.txt`: tabla de Raven de un
  audio con la estructura **real** de BirdNET-Analyzer 2.4.0: no trae nombre científico,
  solo nombre común y código de eBird.
- `combinado.BirdNET.selection.table.txt`: tabla de Raven combinada (con `Scientific Name`)
  según el código oficial: dos audios (inicio real en `File Offset (s)`), una detección
  de voz humana y otra de un audio que no está en los datos de ejemplo.

Los valores (especies, tiempos, confianzas) son inventados; la estructura se ha
comprobado con BirdNET-Analyzer 2.4.0 ejecutado desde Biosonora (2026-10-01).
