# Fase 0 — Análisis de `Instrucciones/` y decisiones

Fecha: 2026-09-30

## 1. CONFIG.TXT (AudioMoth)

- Formato `Clave<espacios>: valor`, saltos de línea CRLF. `-` = "no aplica".
  Las unidades van en la clave (`Sample rate (Hz)`) y la zona horaria en el valor (`(UTC)`).
- Los valores pueden contener `:` (`16:00 - 21:00 (UTC)`): no partir por el primer `:`,
  sino por el separador `<espacios>: `.
- Puede haber varias líneas `Recording period N`.
- Otros firmwares añaden campos: el lector debe aceptar claves desconocidas.
- Datos útiles: Device ID, firmware, frecuencia de muestreo, ganancia, duración de
  grabación y de pausa, periodos de grabación, primera/última fecha, filtros.
- El ejemplo graba a 250 kHz (configuración puntual). La frecuencia de muestreo es
  variable: se lee del CONFIG.TXT (o del WAV) y, si no se puede, la introduce el usuario.
  A 250 kHz una pista de 55 s ocupa ~27 MB y BirdNET necesita remuestrear a 48 kHz.
- Duración de pista en el ejemplo: 55 s (+5 s de pausa), no 60 s.
- Con "Use device ID in WAV file name: Yes" y "Use daily folder: Yes" los WAV
  probablemente estén en subcarpetas diarias y se llamen `ID_AAAAMMDD_HHMMSS.WAV`.
  **Pendiente de confirmar con archivos reales.**
- Todo en UTC ("Use timezone from chime: No"); no hay ubicación en el CONFIG.TXT.

## 2. observations-import-example.xlsx (Observation.org)

Hojas: `Example` (cabecera + fila de ejemplo que hay que eliminar) y
`Lookup tables` (valores permitidos en español por grupo de especies).

Columnas, en este orden:

| # | Columna | Formato en el ejemplo |
|---|---|---|
| 1 | date | texto `AAAA-MM-DD` |
| 2 | time | texto `HH:MM` |
| 3 | scientific name | texto |
| 4 | lat | decimal con punto |
| 5 | lng | decimal con punto |
| 6 | accuracy | entero (metros) |
| 7 | number | entero |
| 8 | notes | texto |
| 9 | sex | `U` |
| 10 | is certain | `True` / `False` |
| 11 | is escape | `True` / `False` |
| 12 | activity | lista (p. ej. "Cantando", "Llamada") |
| 13 | life stage | lista |
| 14 | method | lista (p. ej. "Oído") |
| 15 | counting method | lista (p. ej. "Visto/Sin contar") |
| 16 | related species | texto |
| 17 | external reference | texto |
| 18 | obscurity | entero |
| 19 | embargo date | texto `AAAA-MM-DD` |

## 3. Algoritmo.R

Bien resuelto (se reutiliza): columnas tomadas de la plantilla (orden garantizado),
validación de columnas requeridas, tabla de estaciones separada, formatos de fecha/hora.

Problemas: no lee la salida real de BirdNET; `DROP TABLE` borra datos previos;
valores en inglés que no están en la lista permitida; una observación por detección
(sin agrupar); `is certain` basado en confianza > 0.85 en vez de validación humana;
no filtra clases no aviares ("Human", "Dog"…); sin equivalencias taxonómicas;
fechas sin zona horaria; instala/actualiza paquetes en cada ejecución; conexión sin
cerrar; escritura fila a fila; nombres y valores fijos en el código.

## 4. Decisiones tomadas

- Estructura: paquete de R con **golem**; dependencias con **renv** (modo explícito).
- Base de datos: **SQLite**, guardada fuera de OneDrive.
- Zona horaria local por defecto: **Europe/Madrid**.
- Licencia: **GPL-3**.
- Observation.org: exportar a **CSV**; valores en **español**; valores fijos
  `method = "Oído"`, `counting method = "Visto/Sin contar"`; en `notes`,
  "ID mediante {clasificador}" (de momento "BirdNET APP"), configurable en
  `inst/golem-config.yml`.

## 5. Pendiente

- Archivos WAV reales de AudioMoth (con su estructura de carpetas) y TASCAM para la Fase 1.
- Confirmar separador y codificación del CSV que acepta Observation.org, y qué campos
  son obligatorios.
- Grupos de observaciones para Observation.org (Fase 3).
