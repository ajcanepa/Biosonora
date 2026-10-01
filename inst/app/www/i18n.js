// Traducción en vivo de la interfaz.
// Los textos traducibles llevan data-i18n="clave" (texto) o
// data-i18n-placeholder / data-i18n-title / data-i18n-aria-label (atributos).
// El servidor envía el diccionario del idioma elegido y aquí se sustituyen.
(function () {
  'use strict';
  const STORAGE_KEY = 'biosonora-lang';
  const ATTRS = ['placeholder', 'title', 'aria-label'];

  function applyLanguage(msg) {
    const dict = msg.dict || {};
    document.documentElement.lang = msg.lang;
    document.querySelectorAll('[data-i18n]').forEach(function (el) {
      const text = dict[el.getAttribute('data-i18n')];
      if (text !== undefined) el.textContent = text;
    });
    ATTRS.forEach(function (attr) {
      document.querySelectorAll('[data-i18n-' + attr + ']').forEach(function (el) {
        const text = dict[el.getAttribute('data-i18n-' + attr)];
        if (text !== undefined) el.setAttribute(attr, text);
      });
    });
    try { localStorage.setItem(STORAGE_KEY, msg.lang); } catch (e) { /* sin almacenamiento */ }
  }

  // Al cargar la página (antes de conectar con el servidor)
  $(function () {
    Shiny.addCustomMessageHandler('biosonora-set-lang', applyLanguage);
    // Recuperar el idioma elegido la última vez: se fija antes de que Shiny
    // lea el valor inicial del selector
    let saved = null;
    try { saved = localStorage.getItem(STORAGE_KEY); } catch (e) { saved = null; }
    const select = document.getElementById('lang');
    if (saved && select && select.value !== saved) {
      select.value = saved;
    }
  });
})();
