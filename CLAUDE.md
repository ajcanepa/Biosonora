# Proyecto Biosonora

Biosonora es una aplicación Shiny (R) para monitorizar la biodiversidad mediante acústica: ordenar, visualizar, escuchar, revisar y exportar grabaciones de campo (normalmente pistas de 1 minuto), integrando las detecciones de BirdNET y exportando los resultados a Observation.org.

## Cómo quiero que trabajes conmigo
Soy principiante en R y Shiny. Por eso:
- Explícame cada paso con claridad, sin dar por supuestos conocimientos previos. La primera vez que uses un concepto nuevo (módulos, reactividad, renv, etc.), explícalo brevemente.
- Antes de crear o modificar archivos, dime qué vas a hacer, qué archivos se verán afectados y por qué. En cambios grandes, presenta primero un plan y espera mi aprobación.
- Pídeme permiso antes de instalar paquetes, ejecutar comandos que modifiquen el sistema o borrar archivos.
- Trabaja por fases (ver "Plan de desarrollo"). No avances a la siguiente hasta que confirme que la anterior funciona.
- Antes de decisiones importantes de arquitectura o de añadir una dependencia pesada, explícame las alternativas con sus pros y contras.
- Usa git: haz un commit con un mensaje descriptivo en español después de cada avance que funcione, para que siempre podamos volver atrás.
- Después de cada cambio, ejecuta los tests y comprueba que la app arranca. Si algo falla, explícame qué pasó antes de corregirlo.
- Al terminar cada tarea, dime exactamente cómo probar el resultado (qué comando ejecutar y qué debería ver).
- Nombres de funciones y variables en inglés; comentarios del código y mensajes de commit en español.
- Si detectas un error en algo que hayas hecho antes, dímelo y corrígelo.

## Archivos de referencia (carpeta `Instrucciones/`)
La carpeta `Instrucciones/` contiene material de referencia. **No modifiques ni borres nada dentro de ella.** Si necesitas partir de alguno de estos archivos, cópialo a la carpeta correspondiente del proyecto.

- Archivo de configuración generado por AudioMoth, para conocer su formato real y que la app pueda leerlo: @Instrucciones/CONFIG.TXT
- `Instrucciones/observations-import-example.xlsx`: ejemplo del formato de importación de Observation.org. Léelo con R (por ejemplo con readxl) para conocer las columnas exactas, su orden, los formatos de fecha, hora y coordenadas y qué campos son obligatorios.
- `Instrucciones/Algoritmo.R`: un intento previo mío de generar el archivo para Observation.org. Antes de la fase correspondiente, léelo, explícame qué hace, qué partes están bien resueltas y qué problemas o limitaciones tiene. Reutiliza lo que sirva y mejora el resto.

Si algo de estos archivos es ambiguo o contradictorio, pregúntame en lugar de suponer.

## Usuarios
Voluntarios de ciencia ciudadana sin formación técnica. La app debe:
- Poder usarse sin manual: flujo de trabajo guiado y evidente.
- Incluir una guía de bienvenida (onboarding) y textos de ayuda contextuales.
- Mostrar mensajes de error comprensibles, que expliquen qué pasó y qué hacer; nunca errores técnicos de R.
- Guardar el trabajo automáticamente para no perder anotaciones.
- Ser bilingüe (español e inglés) con selector de idioma visible. Todos los textos de la interfaz deben estar en archivos de traducción, nunca escritos directamente en el código.

## Entornos de ejecución
1. Local, en el ordenador del usuario, leyendo una carpeta de grabaciones.
2. Servidor (shinyapps.io o Posit Connect), con subida de archivos.
El cambio de modo debe depender de un archivo de configuración, no de reescribir código. Avísame de las limitaciones de cada entorno (memoria, tamaño de subida, tiempo de cómputo), especialmente para ejecutar BirdNET en servidor.

## Fuentes de datos
La app debe detectar automáticamente de qué tipo de grabadora procede cada archivo y extraer sus metadatos. Si no puede determinarlo, debe pedírselo al usuario. Antes de escribir los lectores, analiza archivos de ejemplo reales y no supongas formatos.

### AudioMoth
- Audio en WAV (normalmente mono), acompañado del archivo CONFIG.TXT con la configuración del dispositivo.
- Extraer del CONFIG.TXT: identificador del dispositivo, frecuencia de muestreo, ganancia, horarios de grabación, zona horaria y cualquier otro dato útil.
- Extraer del nombre de archivo (formato AAAAMMDD_HHMMSS) la fecha y hora, y leer también los metadatos que AudioMoth escribe dentro del propio WAV (comentario con fecha, dispositivo, ganancia, batería, temperatura).
- Atención a la zona horaria: AudioMoth suele registrar en UTC. Convierte a hora local usando la información del CONFIG.TXT y deja siempre claro en qué zona horaria está cada dato.
- Asociar cada grabación a su CONFIG.TXT correspondiente (normalmente uno por tarjeta SD o carpeta).

### Grabadoras manuales (tipo TASCAM DR-05X)
- Audio en WAV, posiblemente estéreo y con distintas frecuencias de muestreo y profundidades de bits.
- Leer la fecha y hora de los metadatos internos del WAV (por ejemplo el bloque BWF/bext) cuando existan; si no, del nombre de archivo o de la fecha del archivo, avisando al usuario de que puede ser menos fiable.
- Estas grabadoras no registran ubicación: la app debe permitir introducir punto de muestreo y coordenadas, y aplicarlos a varias grabaciones a la vez.
- Las pistas pueden durar más de 1 minuto: permitir trabajar con ellas y, opcionalmente, dividirlas en segmentos.

### Metadatos comunes
Cada grabación debe tener: tipo y modelo de grabadora, identificador del dispositivo, fecha y hora (con zona horaria), duración, frecuencia de muestreo, canales, punto de muestreo, coordenadas y observador.

## Volumen de datos
Se revisarán cientos de pistas por sesión (una pista WAV de 1 minuto a 48 kHz ocupa unos 5-6 MB). Por tanto:
- No cargues todos los audios en memoria: carga bajo demanda la pista seleccionada.
- Genera los espectrogramas bajo demanda y guárdalos en caché.
- Guarda grabaciones, metadatos, detecciones y anotaciones en una base de datos ligera (SQLite o DuckDB; compáralas conmigo antes de elegir).
- Tablas grandes con paginación, búsqueda y filtros.

## Funcionalidades

### 1. Carga y organización
- Formatos: WAV como mínimo; valora FLAC.
- Lista navegable de pistas con filtros (fecha, hora, punto, grabadora, estado de revisión, especie detectada) y ordenación.
- Indicador visual del estado de cada pista: sin revisar, en revisión, revisada.

### 2. Visualización
- Espectrograma de alta calidad con paleta apta para daltonismo (tipo viridis) y paleta alternativa.
- Forma de onda sincronizada con el espectrograma.
- Zoom en tiempo y frecuencia, ajuste de rango de frecuencias y contraste.
- Cursor de reproducción que avance sobre el espectrograma mientras suena el audio.
- Detecciones de BirdNET mostradas como regiones sobre el espectrograma, con color según especie o confianza.

### 3. Reproducción
- Reproducir, pausar y saltar a un punto haciendo clic en el espectrograma.
- Control de velocidad (al menos 1x, 0,5x, 0,25x y 0,1x).
- Reproducir solo el fragmento de una detección.
- Atajos de teclado para las acciones frecuentes (reproducir/pausar, siguiente pista, validar detección), con una ayuda visible que los muestre.

### 4. Integración con BirdNET
a) Importar resultados ya generados: formatos de salida de BirdNET-Analyzer (CSV y tabla de selección de Raven). Validar las columnas y avisar claramente si el formato no es el esperado.
b) Ejecutar BirdNET desde la app:
- Parámetros configurables: umbral de confianza, coordenadas y semana del año (para filtrar especies plausibles), solapamiento, idioma de los nombres.
- Ejecución en segundo plano (asíncrona) con barra de progreso, sin bloquear la app ni a otros usuarios.
- Explícame cómo instalar y conectar la parte de Python de forma reproducible.

### 5. Revisión y anotación
- Para cada detección: validar (correcta / incorrecta / dudosa), corregir la especie y añadir notas.
- Añadir anotaciones manuales dibujando una región sobre el espectrograma.
- Filtro por umbral de confianza ajustable en directo.
- Registrar quién hizo cada anotación y cuándo.

### 6. Listado de especies detectadas
Tras clasificar los audios, generar y guardar en la base de datos un listado de especies detectadas, exportable a CSV, con al menos:
- Nombre científico (y nombre común en el idioma seleccionado).
- Confianza de BirdNET expresada como porcentaje. Nota: BirdNET da una puntuación de confianza entre 0 y 1, no una precisión (accuracy) medida; guárdala como valor numérico entre 0 y 1 y muéstrala como porcentaje, llamándola "confianza" en la interfaz.
- Por especie: confianza máxima, confianza media, número de detecciones, número de grabaciones en las que aparece y estado de validación.
- Posibilidad de filtrar por grabación, punto de muestreo, fecha y umbral de confianza.

### 7. Exportación a Observation.org
- Generar un CSV listo para importar en Observation.org, siguiendo exactamente el formato de `Instrucciones/observations-import-example.xlsx` (columnas, orden, codificación, separador, formatos de fecha, hora y coordenadas). Si el ejemplo no deja claro algún detalle, pregúntame.
- Partir del análisis de `Instrucciones/Algoritmo.R`.
- Por defecto, exportar solo las detecciones validadas como correctas; permitir opcionalmente incluir detecciones no revisadas por encima de un umbral de confianza elegido por el usuario, avisando de que no están verificadas.
- Taxonomía: los nombres científicos de BirdNET pueden no coincidir con los de Observation.org. Crea una tabla de equivalencias editable y, antes de exportar, muestra al usuario las especies sin correspondencia para que las resuelva.
- Decide conmigo cómo agrupar las detecciones en observaciones (por ejemplo, un registro por especie, punto y día, o por grabación), según lo que admita Observation.org.
- Antes de generar el archivo, validar que no falten campos obligatorios (especie, fecha, coordenadas…) y mostrar una vista previa.
- Crear tests que comprueben que el CSV generado tiene exactamente la estructura del ejemplo.

### 8. Otras exportaciones y resúmenes
- Exportar a CSV y Excel el listado completo de detecciones y anotaciones: archivo, grabadora, fecha, hora, punto, coordenadas, inicio y fin de la detección, frecuencias, especie (nombre científico y común), confianza, validación, notas y revisor.
- Gráficos resumen: especies por punto, actividad por hora del día y curva de acumulación de especies.

## Seguridad y privacidad
- Validar todos los archivos subidos: extensión, tipo real del archivo, tamaño máximo y que el audio se pueda leer.
- Sanear los nombres de archivo; nunca construir rutas directamente con nombres proporcionados por el usuario.
- Al ejecutar procesos externos (BirdNET), pasar los argumentos como vector (por ejemplo con processx), nunca construyendo un comando de texto con datos del usuario.
- En servidor, aislar los archivos de cada sesión y borrar los temporales al terminar.
- Autenticación de usuarios en modo servidor.
- Nada de contraseñas ni claves en el código: usar variables de entorno o archivos de configuración incluidos en `.gitignore`.
- No subir nunca al repositorio grabaciones reales ni la base de datos: añádelas a `.gitignore`.
- Privacidad de personas: las grabaciones pueden captar voces humanas. Permitir marcar y excluir fragmentos con voz humana (BirdNET tiene una clase "Human") y excluirlos siempre de las exportaciones.
- Especies sensibles: permitir ocultar o generalizar las coordenadas de especies amenazadas en las exportaciones.
- Explícame cualquier riesgo de seguridad que detectes.

## Diseño visual
- Interfaz moderna, limpia y muy visual, basada en bslib (Bootstrap 5), con paleta inspirada en la naturaleza.
- Modo oscuro disponible (el espectrograma se lee mejor sobre fondo oscuro).
- Diseño adaptable a portátil y tableta.
- Accesibilidad: buen contraste, textos legibles, controles de tamaño suficiente y nunca transmitir información solo con el color.

## Arquitectura y calidad
- Estructura modular con módulos de Shiny (carga, visualización, reproducción, BirdNET, anotación, listado de especies, exportación).
- Organizar el proyecto como paquete de R; valora el framework golem y explícame por qué sí o no.
- Gestionar dependencias con renv.
- Tests con testthat (lógica: lectura de AudioMoth y TASCAM, importación de BirdNET, listado de especies, exportación a Observation.org) y shinytest2 (interfaz).
- README en español e inglés con instalación, uso y despliegue.
- Prioriza paquetes bien mantenidos (por ejemplo tuneR, seewave, av para audio). Para el espectrograma interactivo y la reproducción sincronizada, valora soluciones JavaScript como wavesurfer.js y compáralas conmigo antes de elegir.
- Crea una carpeta `datos_ejemplo/` con archivos pequeños de prueba para los tests (sin datos personales).

## Licencias
- La licencia de Biosonora está por decidir (MIT o GPL-3).
- Revisa las licencias de todas las dependencias. Los modelos de BirdNET tienen su propia licencia (Creative Commons no comercial según su repositorio): compruébala y avísame de sus implicaciones.

## Plan de desarrollo
- Fase 0: preparar el proyecto (estructura, git, renv, .gitignore) y analizar los archivos de `Instrucciones/`. Entrégame un resumen de lo que has encontrado en CONFIG.TXT, en el ejemplo de Observation.org y en Algoritmo.R.
- Fase 1 (MVP local): cargar una carpeta, detectar la grabadora (AudioMoth o TASCAM), extraer metadatos, listar pistas, espectrograma y reproducción con control de velocidad.
- Fase 2: importar resultados de BirdNET, mostrarlos sobre el espectrograma, validación y anotación, y listado de especies guardado en la base de datos.
- Fase 3: exportación a Observation.org y demás exportaciones.
- Fase 4: ejecutar BirdNET desde la app en segundo plano.
- Fase 5: interfaz bilingüe, resúmenes gráficos, pulido visual y accesibilidad.
- Fase 6: modo servidor, autenticación, seguridad y despliegue.
Al empezar cada fase, resume qué vamos a construir y qué archivos se crearán o modificarán.
