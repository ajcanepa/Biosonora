# Fase 4 — Ejecutar BirdNET desde la app

Fecha: 2026-10-01

## Instalación del entorno de Python (reproducible)

- Herramienta: uv (https://docs.astral.sh/uv/), instalada en `~/.local/bin` sin sudo.
- Receta en el repositorio: `python/pyproject.toml` + `python/uv.lock` (BirdNET-Analyzer 2.4.0,
  Python 3.12). Equivale a `renv.lock` para R.
- El entorno (~2,5 GB) se crea FUERA de OneDrive, en la carpeta de datos de la app:

  ```bash
  cd python
  UV_PROJECT_ENVIRONMENT="$HOME/.local/share/R/biosonora/python_env" uv sync --frozen
  ```

- La primera ejecución descarga el modelo de BirdNET (224 MB).
- Otra ubicación: variable de entorno `BIOSONORA_BIRDNET_ENV` o `birdnet: python_path`
  en `inst/golem-config.yml`.

## Funcionamiento

- Botón «Analizar con BirdNET…»: pista abierta, marcadas o todas las visibles.
- Parámetros: confianza mínima, coordenadas del punto, semana del año, solapamiento,
  sensibilidad e idioma de los nombres.
- Proceso aparte con processx (argumentos como vector): la app no se bloquea; progreso
  cada segundo y botón de cancelar. Lotes por punto y semana.
- Al terminar, importación automática (sin duplicar; se conservan las validaciones).
- Rendimiento medido: ~4-5 s por pista de 55 s a 250 kHz (4 hilos).

## Hallazgos

- El CSV de BirdNET-Analyzer 2.4.0 coincide con el formato previsto.
- La tabla de Raven de un audio NO trae nombre científico: se traduce el código de eBird
  con la tabla del BirdNET instalado (no se copia al proyecto por su licencia).
- Sin coordenadas, BirdNET propone especies poco plausibles (p. ej. Calidris falcinellus
  en Burgos): la app avisa para asignar el punto antes de analizar.

## Licencias

- BirdNET-Analyzer (código): MIT. Modelos: CC BY-NC-SA 4.0. Uso confirmado no comercial
  (investigación con participación ciudadana). Biosonora no incluye el modelo.

## Limitaciones

- En shinyapps.io no es viable (TensorFlow, memoria). En servidor propio o Posit Connect sí.
