// Guía de bienvenida: se muestra solo la primera vez en cada navegador.
(function () {
  'use strict';
  const STORAGE_KEY = 'biosonora-welcome-seen';
  $(document).on('shiny:sessioninitialized', function () {
    let seen = null;
    try { seen = localStorage.getItem(STORAGE_KEY); } catch (e) { seen = '1'; }
    if (seen) return;
    const btn = document.querySelector('.bs-help-button');
    if (!btn) return;
    try { localStorage.setItem(STORAGE_KEY, '1'); } catch (e) { /* sin almacenamiento */ }
    Shiny.setInputValue(btn.getAttribute('data-first-visit-input'), true);
  });
})();
