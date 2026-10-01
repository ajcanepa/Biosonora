# Fase 3 — Exportación a Observation.org y demás exportaciones

Fecha: 2026-10-01

## Observation.org (pestaña «Exportar»)

Decisiones acordadas:

| Campo | Valor |
|---|---|
| Agrupación | una observación por especie, punto de muestreo y día (hora local) |
| date / time | fecha local `AAAA-MM-DD` / hora `HH:MM` de la primera detección |
| scientific name | nombre IOC (tabla de equivalencias) |
| lat / lng / accuracy | del punto de muestreo |
| number | 1 |
| notes | «ID mediante BirdNet APP» (`inst/golem-config.yml`: `classifier`, `notes_template`) |
| is certain | True si hay alguna detección validada como correcta; False si solo hay no revisadas |
| is escape | False |
| activity | Presente |
| method / counting method | Oído / Visto/Sin contar |
| obscurity y resto | vacíos (Observation.org oculta las especies sensibles por su cuenta) |

- Por defecto solo detecciones validadas como correctas y anotaciones manuales. Opcional:
  no revisadas por encima de un umbral, con aviso de que no están verificadas.
- Nunca: voz humana, ruidos, incorrectas ni dudosas.
- Antes de descargar: comprobación de coordenadas, taxonomía y fechas poco fiables, y vista previa.
- CSV en UTF-8 sin BOM, separado por comas, punto decimal.
- Tests: columnas idénticas a `Instrucciones/observations-import-example.xlsx` y valores fijos
  presentes en sus tablas de valores permitidos para aves.

## Taxonomía

- Lista IOC v15.2 (la de Observation.org), licencia CC BY 3.0, en `inst/extdata/taxonomia/`
  (generada con `data-raw/crear_taxonomia_ioc.R`).
- Orden: equivalencia manual > mismo nombre en IOC > equivalencia Clements→IOC > sin correspondencia.
- Caso real encontrado: *Calocitta formosa* (BirdNET) → *Cyanocorax formosus* (IOC), resuelto a mano.

## Exportación completa

- Todas las detecciones y anotaciones (sin voz humana) en CSV (UTF-8 con BOM) o Excel (`writexl`).

## Pendiente

- Probar la importación real en Observation.org con un CSV generado por la app.
