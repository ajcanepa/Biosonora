# Resultados de BirdNET de ejemplo

- `Blue_Jay_Sample_BirdNET_Results.csv`: archivo **real** (formato sencillo:
  start, end, scientific_name, common_name, confidence). No indica el audio.
- `24BBD608675B68F7_20260810_160000.BirdNET.results.csv`: CSV de BirdNET-Analyzer
  **sintético**, construido según el código oficial (columna `File` con ruta de Windows).
- `combinado.BirdNET.selection.table.txt`: tabla de Raven **sintética** según el código
  oficial, que combina dos audios (inicio real en `File Offset (s)`) e incluye una
  detección de voz humana y otra de un audio que no está en los datos de ejemplo.

Pendiente: validar los dos formatos sintéticos con archivos reales de BirdNET-Analyzer.
