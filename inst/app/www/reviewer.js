// Nombre del revisor: se guarda en el navegador y se envía a Shiny.
(function () {
  'use strict';
  const STORAGE_KEY = 'biosonora-reviewer';

  function show(name) {
    document.querySelectorAll('.bs-reviewer .bs-reviewer-name').forEach(function (el) {
      if (name) {
        el.textContent = name;
        el.removeAttribute('data-i18n'); // ya no es un texto traducible
      }
    });
  }

  function send(name) {
    const btn = document.querySelector('.bs-reviewer');
    if (btn && window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(btn.getAttribute('data-reviewer-input'), name || '');
    }
  }

  $(document).on('shiny:connected', function () {
    let saved = '';
    try { saved = localStorage.getItem(STORAGE_KEY) || ''; } catch (e) { saved = ''; }
    show(saved);
    send(saved);
  });

  $(function () {
    Shiny.addCustomMessageHandler('biosonora-set-reviewer', function (msg) {
      try { localStorage.setItem(STORAGE_KEY, msg.name); } catch (e) { /* sin almacenamiento */ }
      show(msg.name);
      Shiny.setInputValue(msg.id, msg.name);
    });
  });
})();
