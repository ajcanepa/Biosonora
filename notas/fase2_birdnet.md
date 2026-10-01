# Fase 2 — BirdNET, validación, anotación y listado de especies

Fecha: 2026-10-01

## Qué incluye

- Importación de resultados de BirdNET (barra lateral, «3. Resultados de BirdNET»):
  - CSV de BirdNET-Analyzer (`Start (s), End (s), Scientific name, Common name, Confidence, File`)
  - Tabla de selección de Raven (inicio real desde `File Offset (s)` en tablas combinadas)
  - Tabla sencilla (`start, end, scientific_name, common_name, confidence`): archivo real del proyecto
  - Formatos reconocidos por el nombre de las columnas; errores explicados con las filas afectadas.
  - Asociación a grabaciones por ruta, nombre o nombre deducido del archivo de resultados;
    si no se puede, la app pregunta (por defecto, la pista abierta).
  - Reimportar no duplica y conserva validaciones y notas.
- Rectángulos de detección sobre el espectrograma (estado con color + borde + símbolo).
- Umbral de confianza en directo (barra lateral); filtro por especie detectada y columna
  «Detecc.» en la lista de pistas.
- Panel de detecciones: correcta / incorrecta / dudosa (V / X / D), escuchar fragmento (F),
  corregir especie, notas, voz humana (privada), borrar anotaciones manuales, ↑ ↓ para moverse.
- Anotaciones manuales: botón «Nueva anotación» (o tecla A) y dibujar un rectángulo.
- Revisor: nombre recordado por el navegador; historial de cambios (quién y cuándo).
- Pestaña «Especies»: confianza máxima y media (%), detecciones, grabaciones, estado de
  validación; filtros por umbral, punto, fechas y grabaciones; exportación CSV (UTF-8 con BOM),
  que también guarda el listado en la base de datos.

## Decisiones

- Nombres comunes: los que vienen en los archivos de BirdNET (en la Fase 3 se igualarán a
  Observation.org con una tabla de equivalencias).
- No se incluyen en el listado: detecciones incorrectas, voz humana ni ruidos (perro, motor...).
- Si el archivo no indica frecuencias, se usa la banda de BirdNET (0–15 kHz).

## Pendiente

- Validar los formatos CSV de BirdNET-Analyzer y Raven con archivos reales.
- En modo servidor (Fase 6), el revisor saldrá de la autenticación.
