# Biosonora

*[English version](README.en.md)*

Biosonora es una aplicación Shiny (R) para la **monitorización acústica de la biodiversidad**
con participación ciudadana. Permite ordenar, ver, escuchar, revisar y exportar grabaciones
de campo, integrar las detecciones de [BirdNET](https://birdnet.cornell.edu/) y preparar
los datos para [Observation.org](https://observation.org).

## Qué puedes hacer

- **Cargar grabaciones** de una carpeta (o subir WAV / ZIP). Reconoce sola las grabadoras
  **AudioMoth** (lee su `CONFIG.TXT`, el comentario y los metadatos GUANO de cada archivo)
  y las **grabadoras manuales** tipo TASCAM (bloque BWF/bext). Convierte la hora UTC a hora
  local (Europe/Madrid) e indica siempre la zona horaria. Detecta copias duplicadas y fechas
  poco fiables.
- **Ver y escuchar**: espectrograma de alta calidad (paletas aptas para daltonismo), forma de
  onda, zoom, rango de frecuencias y contraste. Reproducción a 1x, 0,5x, 0,25x y 0,1x: la
  velocidad lenta permite **oír ultrasonidos** (murciélagos).
- **BirdNET**: importar resultados (CSV de BirdNET-Analyzer, tabla de Raven o tabla sencilla)
  o **analizar desde la app**, en segundo plano.
- **Revisar**: validar cada detección (correcta / incorrecta / dudosa), corregir la especie,
  añadir notas, marcar voz humana (privada, nunca se exporta) y dibujar anotaciones propias.
  Cada cambio guarda quién lo hizo y cuándo.
- **Listado de especies**, **gráficos resumen** (especies por punto, actividad por hora,
  curva de acumulación) y **exportaciones**: CSV para Observation.org (con tabla de
  equivalencias taxonómicas IOC), CSV y Excel completos.
- Interfaz en **español e inglés**, modo oscuro, tamaño de texto ajustable, atajos de
  teclado y guía de bienvenida.

## Instalación (uso local)

### 1. Requisitos

- [R](https://cran.r-project.org/) 4.6 o superior.
- [RStudio](https://posit.co/download/rstudio-desktop/) (recomendado).
- [Git](https://git-scm.com/) para descargar el proyecto (o descarga el ZIP desde GitHub).
- Linux: algunas bibliotecas del sistema para compilar paquetes
  (`sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev`).

### 2. Descargar el proyecto

```bash
git clone https://github.com/ajcanepa/Biosonora.git
```

### 3. Abrir la app

1. Abre la carpeta `Biosonora` en RStudio (archivo `Biosonora.Rproj` o «Open Project»).
2. Abre el archivo **`iniciar_biosonora.R`** y pulsa **«Source»**.

La primera vez se instalan los paquetes con las versiones exactas del proyecto
([renv](https://rstudio.github.io/renv/)); puede tardar varios minutos. Después se abre la
app en el navegador.

Desde una terminal: `Rscript iniciar_biosonora.R`.

### 4. (Opcional) Instalar BirdNET para analizar desde la app

BirdNET es un programa de Python. Se instala una sola vez con
[uv](https://docs.astral.sh/uv/) (sin permisos de administrador; ~2,5 GB):

```bash
# Instalar uv (Linux/macOS; en Windows ver la web de uv)
curl -LsSf https://astral.sh/uv/install.sh | sh

# Desde la carpeta python/ del proyecto
cd python
UV_PROJECT_ENVIRONMENT="$HOME/.local/share/R/biosonora/python_env" uv sync --frozen
```

En Windows (PowerShell), desde la carpeta `python\`:

```powershell
$env:UV_PROJECT_ENVIRONMENT = "$env:LOCALAPPDATA\R\data\R\biosonora\python_env"
uv sync --frozen
```

La ruta exacta del entorno en tu sistema la muestra la propia app si BirdNET no está
instalado (botón «Analizar con BirdNET…»). La primera ejecución descarga el modelo (224 MB).

## Uso rápido

1. **Cargar**: escribe o elige la carpeta de grabaciones y pulsa «Analizar carpeta».
2. **Punto de muestreo**: marca las grabaciones en la lista y pulsa «Asignar punto de
   muestreo» (nombre y coordenadas). Hazlo antes de analizar con BirdNET.
3. **Revisar**: haz clic en una grabación, escucha (barra espaciadora) y valida las
   detecciones con **V** (correcta), **X** (incorrecta) o **D** (dudosa).
4. **Exportar**: pestaña «Exportar» → «Comprobar y previsualizar» → descargar el CSV para
   Observation.org.

El botón **«Ayuda»** abre la guía de bienvenida y «Atajos de teclado» muestra todas las teclas.

### Dónde se guardan los datos

En la carpeta de datos del usuario, **fuera** de carpetas sincronizadas (OneDrive, Dropbox),
para evitar bloqueos de la base de datos:

- Linux: `~/.local/share/R/biosonora/`
- Windows: `%LOCALAPPDATA%\R\data\R\biosonora\`
- macOS: `~/Library/Application Support/org.R-project.R/R/biosonora/`

Contiene la base de datos (`biosonora.sqlite`), la caché de espectrogramas, las grabaciones
subidas y el entorno de BirdNET. Las grabaciones de una carpeta **no se copian**: se leen
donde están.

## Configuración

Archivo `inst/golem-config.yml`:

| Opción | Uso |
|---|---|
| `run_mode` | `local` (carpetas del ordenador) o `server` |
| `local_timezone` | Zona horaria para mostrar las fechas (por defecto `Europe/Madrid`) |
| `data_dir` | Carpeta de datos (vacío = la del usuario) |
| `max_upload_mb` | Tamaño máximo de cada archivo subido |
| `server_folders` | Modo servidor: carpetas que se pueden analizar |
| `observation_org` | Valores fijos de la exportación (método, conteo, actividad, nota) |

Variables de entorno: `BIOSONORA_DATA_DIR` (carpeta de datos), `BIOSONORA_BIRDNET_ENV`
(entorno de BirdNET), `GOLEM_CONFIG_ACTIVE=production` (modo servidor).

## Despliegue en un servidor (futuro)

Biosonora está preparada para funcionar en un servidor propio (por ejemplo, de la UBU) con
[Shiny Server](https://posit.co/products/open-source/shinyserver/) o Posit Connect:

- Activar el modo servidor con `GOLEM_CONFIG_ACTIVE=production`.
- Las personas eligen grabaciones de `server_folders` (nunca escriben rutas) o las suben.
  Cada archivo subido se valida (extensión, tipo real, tamaño, nombre saneado; ZIP sin
  rutas peligrosas).
- Cada usuario identificado tiene su propia carpeta y base de datos.
- **Pendiente antes de publicar**: inicio de sesión (propuesto:
  [shinymanager](https://datastorm-open.github.io/shinymanager/), con contraseñas cifradas en
  una base de datos fuera del repositorio), HTTPS y revisión de memoria y CPU para BirdNET.
- shinyapps.io no es adecuado para BirdNET (TensorFlow y límites de memoria).

## Desarrollo

- Estructura de paquete R con [golem](https://thinkr-open.github.io/golem/): `R/` (código),
  `inst/` (textos en `inst/i18n/`, configuración, recursos web), `tests/`, `datos_ejemplo/`
  (archivos de prueba sintéticos), `data-raw/` (scripts que generan datos), `notas/`
  (decisiones de cada fase), `python/` (entorno de BirdNET).
- Tests: `devtools::test()` (lógica con testthat e interfaz con shinytest2, que necesita
  Google Chrome o Chromium).
- Todos los textos de la interfaz están en `inst/i18n/es.yml` y `inst/i18n/en.yml`.

## Licencias y créditos

- **Biosonora**: GPL-3 (ver `LICENSE`).
- **Modelos de BirdNET**: [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)
  — solo uso no comercial (investigación, educación, ciencia ciudadana). No se incluyen en
  este repositorio. BirdNET-Analyzer (código): MIT.
- **Lista IOC de Aves del Mundo v15.2** (taxonomía de Observation.org): Gill F, D Donsker &
  P Rasmussen (Eds), [CC BY 3.0](https://creativecommons.org/licenses/by/3.0/).
- **wavesurfer.js** 7.12.12: BSD-3-Clause.
- Paquetes de R: licencias libres (MIT, BSD, Apache, LGPL, GPL); detalle en
  `notas/fase6_servidor_seguridad.md`.
