# Fase 6 — Uso local, preparación para servidor, seguridad y documentación

Fecha: 2026-10-01

## Decisiones

- De momento **uso local**: cada persona clona el repositorio y abre `iniciar_biosonora.R`
  (renv instala las versiones exactas). Servidor propio (UBU) en el futuro, si hay financiación.
- Cada persona tiene sus propias grabaciones: en local, su ordenador; en servidor, una carpeta
  y base de datos por usuario identificado (`user_space()`).
- Grabaciones: carpeta (local) o carpetas autorizadas `server_folders` (servidor) **y** subida
  de WAV / ZIP / CONFIG.TXT en ambos modos.
- Autenticación: no necesaria en local. Para servidor se propone `shinymanager` (pendiente).

## Seguridad

| Riesgo | Medida |
|---|---|
| Archivos subidos maliciosos o disfrazados | Extensión permitida + tipo real por los primeros bytes + tamaño máximo + lectura del audio al analizar |
| Rutas manipuladas (`../`) | Nombres saneados (`sanitize_filename`); nunca se construyen rutas con nombres del usuario |
| ZIP con rutas fuera de la carpeta (zip slip) | Se rechazan entradas absolutas o con `..`; solo se extraen WAV y CONFIG.TXT |
| Bomba ZIP | Límite al tamaño descomprimido |
| Rutas escritas en servidor | En modo servidor solo se elige entre `server_folders`; la ruta nunca viene del navegador |
| Inyección de órdenes (BirdNET) | processx con argumentos en vector; sin `system()` ni `eval()` |
| Inyección SQL | Parámetros en todas las consultas; las dos con `paste0` solo usan nombres de columna fijos |
| Inyección de HTML/JS | `htmlEscape` en todas las celdas HTML; en JS, `textContent` |
| Datos de otras personas | Carpeta temporal de audio por sesión (se borra al salir); espacio por usuario en servidor |
| Temporales de BirdNET | Se borran tras importar |
| Voz humana | Marcada como privada; nunca se exporta |
| Contraseñas / claves | Ninguna en el código; `.Renviron` y configuraciones locales en `.gitignore` |
| Grabaciones / base de datos en git | `.gitignore` (solo WAV sintéticos de `datos_ejemplo/`) |

Riesgos que quedan (para el modo servidor):
- Falta inicio de sesión: sin él, en servidor todas las personas compartirían un espacio.
- La caché de espectrogramas es común (nombres = huellas no adivinables); revisar si se quiere
  aislar por usuario.
- El audio se descarga entero al navegador (~9 MB por pista): revisar ancho de banda.
- Sin HTTPS los datos viajan sin cifrar: usar un proxy con certificado (nginx + Let's Encrypt).
- BirdNET en servidor: limitar análisis simultáneos (CPU y memoria).

## Licencias de las dependencias (93 paquetes de R en renv.lock)

| Licencia | Paquetes |
|---|---|
| MIT | 64 |
| GPL-2 o GPL-3 / GPL (>= 2) / GPL (cualquier versión) | 16 |
| LGPL (>= 2.1) | 3 |
| BSD-2 / BSD-3 | 4 |
| Apache 2.0 | 1 |
| BSL-1.0 (AsioHeaders, permisiva) | 1 |
| MIT o Unlimited | 1 |
| **GPL-2 solo** | **2**: `signal` (dependencia de tuneR) y `websocket` (solo tests) |

Nota sobre GPL-2 solo: la Free Software Foundation considera incompatibles GPL-2 "solo" y
GPL-3 cuando se **distribuye una obra combinada**. Biosonora no incluye ni redistribuye esos
paquetes (cada persona los instala desde CRAN), que es la práctica habitual en R. Si algún día
se distribuyera un paquete todo-en-uno (p. ej. una imagen Docker), convendría valorar licenciar
Biosonora como `GPL (>= 2)` o sustituir `signal`.

Otros componentes: wavesurfer.js (BSD-3), Lista IOC v15.2 (CC BY 3.0, con cita),
BirdNET-Analyzer (MIT) y modelos de BirdNET (CC BY-NC-SA 4.0, no incluidos; uso no comercial).

## Tests

- testthat: lógica completa, incluidas subidas (ZIP malicioso de prueba en
  `datos_ejemplo/otros/subida_prueba.zip`).
- shinytest2: la app real en Chrome sin ventana (arranque y análisis, idioma, importar BirdNET
  y validar con el teclado, listado de especies, avisos de exportación y subida de ZIP).
- Encontrado por los tests de interfaz: las grabaciones sin fecha (archivos ilegibles) se
  ocultaban con el filtro de fechas. Corregido: siempre se muestran.
