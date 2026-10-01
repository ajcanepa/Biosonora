# Biosonora

*[Versión en español](README.md)*

Biosonora is a Shiny (R) application for **acoustic biodiversity monitoring** with citizen
participation. It lets you organise, view, listen to, review and export field recordings,
integrate [BirdNET](https://birdnet.cornell.edu/) detections and prepare the data for
[Observation.org](https://observation.org).

## What you can do

- **Load recordings** from a folder (or upload WAV / ZIP files). It automatically recognises
  **AudioMoth** recorders (reads their `CONFIG.TXT`, the comment and GUANO metadata of each
  file) and **handheld recorders** such as TASCAM (BWF/bext chunk). UTC times are converted to
  local time (Europe/Madrid) and the time zone is always shown. Duplicate copies and unreliable
  dates are detected.
- **See and listen**: high-quality spectrogram (colour-blind friendly palettes), waveform,
  zoom, frequency range and contrast. Playback at 1x, 0.5x, 0.25x and 0.1x: slow speed lets you
  **hear ultrasound** (bats).
- **BirdNET**: import results (BirdNET-Analyzer CSV, Raven selection table or a simple table)
  or **analyse from the app**, in the background.
- **Review**: validate each detection (correct / incorrect / doubtful), correct the species,
  add notes, mark human voice (private, never exported) and draw your own annotations. Every
  change records who made it and when.
- **Species list**, **summary charts** (species per site, activity by hour, accumulation curve)
  and **exports**: CSV for Observation.org (with an IOC taxonomic equivalence table), full CSV
  and Excel tables.
- Interface in **Spanish and English**, dark mode, adjustable text size, keyboard shortcuts and
  a welcome guide.

## Installation (local use)

### 1. Requirements

- [R](https://cran.r-project.org/) 4.6 or later.
- [RStudio](https://posit.co/download/rstudio-desktop/) (recommended).
- [Git](https://git-scm.com/) to download the project (or download the ZIP from GitHub).
- Linux: some system libraries to build packages
  (`sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev`).

### 2. Download the project

```bash
git clone https://github.com/ajcanepa/Biosonora.git
```

### 3. Open the app

1. Open the `Biosonora` folder in RStudio (`Biosonora.Rproj` file or "Open Project").
2. Open **`iniciar_biosonora.R`** and press **"Source"**.

The first time, packages are installed with the project's exact versions
([renv](https://rstudio.github.io/renv/)); this may take several minutes. Then the app opens in
your browser.

From a terminal: `Rscript iniciar_biosonora.R`.

### 4. (Optional) Install BirdNET to analyse from the app

BirdNET is a Python program. It is installed once with [uv](https://docs.astral.sh/uv/)
(no administrator rights needed; ~2.5 GB):

```bash
# Install uv (Linux/macOS; on Windows see the uv website)
curl -LsSf https://astral.sh/uv/install.sh | sh

# From the project's python/ folder
cd python
UV_PROJECT_ENVIRONMENT="$HOME/.local/share/R/biosonora/python_env" uv sync --frozen
```

On Windows (PowerShell), from the `python\` folder:

```powershell
$env:UV_PROJECT_ENVIRONMENT = "$env:LOCALAPPDATA\R\data\R\biosonora\python_env"
uv sync --frozen
```

If BirdNET is not installed, the app shows the exact environment path for your system
("Analyse with BirdNET…" button). The first run downloads the model (224 MB).

## Quick start

1. **Load**: type or choose the recordings folder and press "Scan folder".
2. **Sampling site**: tick the recordings in the list and press "Assign sampling site" (name and
   coordinates). Do this before analysing with BirdNET.
3. **Review**: click a recording, listen (space bar) and validate detections with **V**
   (correct), **X** (incorrect) or **D** (doubtful).
4. **Export**: "Export" tab → "Check and preview" → download the CSV for Observation.org.

The **"Help"** button opens the welcome guide and "Keyboard shortcuts" lists all keys.

### Where data is stored

In the user's data folder, **outside** synchronised folders (OneDrive, Dropbox), to avoid
database locks:

- Linux: `~/.local/share/R/biosonora/`
- Windows: `%LOCALAPPDATA%\R\data\R\biosonora\`
- macOS: `~/Library/Application Support/org.R-project.R/R/biosonora/`

It holds the database (`biosonora.sqlite`), the spectrogram cache, uploaded recordings and the
BirdNET environment. Recordings in a scanned folder are **not copied**: they are read in place.

## Configuration

File `inst/golem-config.yml`:

| Option | Use |
|---|---|
| `run_mode` | `local` (computer folders) or `server` |
| `local_timezone` | Time zone for displaying dates (default `Europe/Madrid`) |
| `data_dir` | Data folder (empty = the user's) |
| `max_upload_mb` | Maximum size of each uploaded file |
| `server_folders` | Server mode: folders that can be scanned |
| `observation_org` | Fixed export values (method, counting, activity, note) |

Environment variables: `BIOSONORA_DATA_DIR` (data folder), `BIOSONORA_BIRDNET_ENV` (BirdNET
environment), `GOLEM_CONFIG_ACTIVE=production` (server mode).

## Server deployment (future)

Biosonora is ready to run on an own server (e.g. at the University of Burgos) with
[Shiny Server](https://posit.co/products/open-source/shinyserver/) or Posit Connect:

- Enable server mode with `GOLEM_CONFIG_ACTIVE=production`.
- People choose recordings from `server_folders` (they never type paths) or upload them. Each
  upload is validated (extension, real file type, size, sanitised name; ZIP without dangerous
  paths).
- Each identified user has their own folder and database.
- **To do before publishing**: login (proposed:
  [shinymanager](https://datastorm-open.github.io/shinymanager/), with encrypted passwords in a
  database outside the repository), HTTPS and a memory/CPU review for BirdNET.
- shinyapps.io is not suitable for BirdNET (TensorFlow and memory limits).

## Development

- R package structure with [golem](https://thinkr-open.github.io/golem/): `R/` (code), `inst/`
  (texts in `inst/i18n/`, configuration, web assets), `tests/`, `datos_ejemplo/` (synthetic test
  files), `data-raw/` (data-generating scripts), `notas/` (decisions of each phase), `python/`
  (BirdNET environment).
- Tests: `devtools::test()` (logic with testthat and interface with shinytest2, which needs
  Google Chrome or Chromium).
- All interface texts are in `inst/i18n/es.yml` and `inst/i18n/en.yml`.

## Licences and credits

- **Biosonora**: GPL-3 (see `LICENSE`).
- **BirdNET models**: [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/) —
  non-commercial use only (research, education, citizen science). Not included in this
  repository. BirdNET-Analyzer (code): MIT.
- **IOC World Bird List v15.2** (Observation.org taxonomy): Gill F, D Donsker & P Rasmussen
  (Eds), [CC BY 3.0](https://creativecommons.org/licenses/by/3.0/).
- **wavesurfer.js** 7.12.12: BSD-3-Clause.
- R packages: free licences (MIT, BSD, Apache, LGPL, GPL); details in
  `notas/fase6_servidor_seguridad.md`.
