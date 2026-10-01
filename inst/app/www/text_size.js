// Tamaño del texto de la interfaz (A− / A / A+).
// Bootstrap mide textos, márgenes y botones en "rem" (relativo al tamaño de
// letra de la página), así que cambiar ese valor escala toda la interfaz.
// La elección se guarda en el navegador de cada persona.
(function () {
  'use strict';
  const STORAGE_KEY = 'biosonora-text-size';
  const SIZES = { small: '13px', normal: '14px', large: '17px' };

  function apply(size) {
    if (!SIZES[size]) size = 'normal';
    document.documentElement.style.fontSize = SIZES[size];
    document.querySelectorAll('.bs-text-size [data-size]').forEach(function (btn) {
      const active = btn.getAttribute('data-size') === size;
      btn.classList.toggle('active', active);
      btn.setAttribute('aria-pressed', active ? 'true' : 'false');
    });
    try { localStorage.setItem(STORAGE_KEY, size); } catch (e) { /* sin almacenamiento */ }
    // Los gráficos que dependen del ancho (wavesurfer) se recolocan solos
    window.dispatchEvent(new Event('resize'));
  }

  let saved = null;
  try { saved = localStorage.getItem(STORAGE_KEY); } catch (e) { saved = null; }
  // Se aplica cuanto antes para que la página no "salte" al cargar
  document.documentElement.style.fontSize = SIZES[saved] || SIZES.normal;

  $(function () {
    apply(saved || 'normal');
    $(document).on('click', '.bs-text-size [data-size]', function () {
      apply(this.getAttribute('data-size'));
    });
  });
})();
