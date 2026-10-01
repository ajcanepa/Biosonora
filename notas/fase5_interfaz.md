# Fase 5 — Interfaz bilingüe, resúmenes gráficos, pulido y accesibilidad

Fecha: 2026-10-01

## Qué incluye

- Pestaña «Resumen» (ggplot2): especies por punto de muestreo, actividad por hora del día
  (hora local, especies más frecuentes por separado) y curva de acumulación de especies
  (orden cronológico + media e intervalo del 95 % de 100 órdenes al azar).
  Filtros: umbral, solo validadas, puntos y fechas. Descarga en PNG (fondo blanco).
- Paletas aptas para daltonismo: Okabe-Ito, Viridis y Cividis en los gráficos; Viridis,
  Magma, Cividis y grises en el espectrograma. En modo oscuro se descartan los tonos
  demasiado oscuros.
- Rectángulos de detección con colores Okabe-Ito (antes verde/rojo, que se confunden con
  el daltonismo más común) + tipo de borde + símbolo.
- Guía de bienvenida de 5 pasos (primera visita en cada navegador; botón «Ayuda» para
  volver a verla).
- 14 iconos «?» con ayudas contextuales (ratón o teclado).
- Accesibilidad:
  - textos de estado con contraste ≥ 4,5:1 (WCAG AA) en modo claro y oscuro (test);
    se oscureció el naranja de «dudosa» (3,8:1 → 6:1);
  - etiquetas para lectores de pantalla en botones de solo icono y en el visor;
  - texto alternativo en cada gráfico;
  - información nunca solo por color (símbolos, tipos de línea, números sobre barras);
  - foco visible al navegar con el teclado.

## Ya hecho en fases anteriores

Interfaz completa en español e inglés, modo oscuro y selector de tamaño de texto.
