// Visor de Biosonora: wavesurfer.js + imagen de espectrograma calculada en R.
//
// La imagen del espectrograma se inserta dentro del contenedor de wavesurfer,
// así se desplaza y amplía junto con la forma de onda, el cursor la recorre y
// un clic sobre ella salta a ese momento.
//
// Velocidad lenta: el servidor envía un audio "expandido" (más largo). Por eso
// las etiquetas de tiempo se multiplican por la velocidad para mostrar siempre
// el tiempo real de la grabación.
(function () {
  'use strict';

  const players = {};
  window.biosonoraPlayers = players; // para depurar desde la consola
  const SPECTRO_HEIGHT = 300;
  const WAVE_HEIGHT = 60;

  // Estilos de los rectángulos de detección. Van dentro de wavesurfer (shadow
  // DOM), adonde no llega el CSS de la página. El estado se indica con color,
  // tipo de borde y símbolo en la etiqueta (nunca solo con el color).
  const OVERLAY_CSS = `
    .bs-overlay { position: absolute; inset: 0; pointer-events: none; overflow: hidden; }
    .bs-overlay.drawing { pointer-events: auto; cursor: crosshair; background: rgba(255,255,255,0.05); }
    .bs-box { position: absolute; box-sizing: border-box; border: 2px dashed #ffd54f; pointer-events: none; }
    .bs-box.correct { border-style: solid; border-color: #7cf29a; }
    .bs-box.incorrect { border-style: dotted; border-color: #ff7b7b; opacity: 0.75; }
    .bs-box.doubtful { border-style: dashed; border-color: #ffb14e; }
    .bs-box.human { border-color: #e6e6e6;
      background: repeating-linear-gradient(45deg, rgba(255,255,255,0.10) 0 6px, transparent 6px 12px); }
    .bs-box.selected { border-width: 3px; box-shadow: 0 0 0 1px #000, 0 0 0 3px #fff; z-index: 2; }
    .bs-box-label { position: absolute; left: 0; pointer-events: auto; cursor: pointer;
      font: 11px/1.35 system-ui, sans-serif; color: #111; background: rgba(255,255,255,0.88);
      padding: 0 4px; border-radius: 2px; white-space: nowrap; }
    .bs-box.selected .bs-box-label { background: #fff; font-weight: 700; }
    .bs-draw-rect { position: absolute; border: 2px solid #fff; background: rgba(255,255,255,0.18);
      pointer-events: none; }
  `;

  function formatTime(seconds) {
    if (!isFinite(seconds) || seconds < 0) seconds = 0;
    const m = Math.floor(seconds / 60);
    const s = seconds - m * 60;
    const decimals = s % 1 > 0.05 ? 1 : 0;
    const text = s.toFixed(decimals);
    return m + ':' + (s < 10 ? '0' : '') + text;
  }

  function getPlayer(id) {
    if (!players[id]) {
      const root = document.getElementById(id);
      if (!root) return null;
      players[id] = {
        id: id,
        root: root,
        waveEl: root.querySelector('.bs-wave'),
        axisEl: root.querySelector('.bs-freq-axis'),
        emptyEl: root.querySelector('.bs-player-empty'),
        loadingEl: root.querySelector('.bs-player-loading'),
        ws: null,
        img: null,
        speed: 1,
        origDuration: 0,
        zoom: 1,
        recordingId: null
      };
      bindButtons(players[id]);
    }
    return players[id];
  }

  // Botones del propio visor (el id del módulo va delante de "player")
  function bindButtons(p) {
    const prefix = p.id.replace(/player$/, '');
    const play = document.getElementById(prefix + 'play');
    const zoomIn = document.getElementById(prefix + 'zoom_in');
    const zoomOut = document.getElementById(prefix + 'zoom_out');
    p.playBtn = play;
    p.timeEl = document.getElementById(prefix + 'time');
    if (play) play.addEventListener('click', () => togglePlay(p));
    if (zoomIn) zoomIn.addEventListener('click', () => setZoom(p, p.zoom * 2));
    if (zoomOut) zoomOut.addEventListener('click', () => setZoom(p, p.zoom / 2));
  }

  function togglePlay(p) {
    if (p && p.ws) p.ws.playPause();
  }

  function setZoom(p, zoom) {
    if (!p.ws) return;
    p.zoom = Math.min(64, Math.max(1, zoom));
    const duration = p.ws.getDuration();
    if (!duration) return;
    const basePxPerSec = p.waveEl.clientWidth / duration;
    p.ws.zoom(basePxPerSec * p.zoom);
    renderBoxes(p); // al ampliar caben más etiquetas completas
  }

  function seekRelative(p, seconds) {
    if (!p.ws) return;
    const duration = p.ws.getDuration();
    if (!duration) return;
    // `seconds` está en tiempo real de la grabación: se convierte a tiempo de reproducción
    const t = p.ws.getCurrentTime() + seconds / p.speed;
    p.ws.setTime(Math.min(duration, Math.max(0, t)));
  }

  function updatePlayButton(p, playing) {
    if (!p.playBtn) return;
    const icon = p.playBtn.querySelector('i');
    if (icon) {
      icon.classList.toggle('fa-play', !playing);
      icon.classList.toggle('fa-pause', playing);
    }
    p.playBtn.setAttribute('aria-pressed', playing ? 'true' : 'false');
  }

  function updateTime(p, playbackTime) {
    if (p.timeEl) {
      p.timeEl.textContent = formatTime(playbackTime * p.speed) + ' / ' + formatTime(p.origDuration);
    }
  }

  // Eje de frecuencias (kHz) a la izquierda del espectrograma
  function drawAxis(p, fmin, fmax) {
    p.fmin = fmin;
    p.fmax = fmax;
    const axis = p.axisEl;
    axis.innerHTML = '';
    axis.style.paddingTop = WAVE_HEIGHT + 'px';
    const inner = document.createElement('div');
    inner.className = 'bs-freq-axis-inner';
    inner.style.height = SPECTRO_HEIGHT + 'px';
    axis.appendChild(inner);
    const rangeKHz = (fmax - fmin) / 1000;
    const steps = [0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 25, 50];
    let step = steps[steps.length - 1];
    for (const s of steps) { if (rangeKHz / s <= 8) { step = s; break; } }
    const start = Math.ceil(fmin / 1000 / step) * step;
    for (let f = start; f <= fmax / 1000 + 1e-9; f += step) {
      const label = document.createElement('span');
      label.className = 'bs-freq-label';
      label.style.bottom = ((f * 1000 - fmin) / (fmax - fmin) * 100) + '%';
      label.textContent = (Math.round(f * 10) / 10) + ' kHz';
      inner.appendChild(label);
    }
  }

  function setLoading(p, loading) {
    p.loadingEl.classList.toggle('d-none', !loading);
  }

  // El servidor de Shiny no admite descargas parciales (HTTP Range) y sin ellas
  // el navegador no puede saltar dentro del audio. Por eso se descarga el WAV
  // completo y se reproduce desde memoria (blob), donde sí se puede saltar.
  function load(msg) {
    const p = getPlayer(msg.id);
    if (!p || typeof WaveSurfer === 'undefined') return;
    const token = (p.loadToken || 0) + 1;
    p.loadToken = token;
    setLoading(p, true);
    fetch(msg.audio_url)
      .then((r) => { if (!r.ok) throw new Error('HTTP ' + r.status); return r.blob(); })
      .then((blob) => {
        if (p.loadToken !== token) return; // llegó otra petición más reciente
        create(p, msg, URL.createObjectURL(blob));
      })
      .catch((e) => { console.error('[biosonora] audio', e); setLoading(p, false); });
  }

  function create(p, msg, blobUrl) {
    // Al cambiar de velocidad se mantiene la posición y si estaba sonando
    let fraction = 0;
    let wasPlaying = false;
    if (p.ws && msg.keep_position && p.recordingId === msg.recording_id) {
      const d = p.ws.getDuration();
      fraction = d ? p.ws.getCurrentTime() / d : 0;
      wasPlaying = p.ws.isPlaying();
    }
    if (p.ws) { p.ws.destroy(); p.ws = null; }
    if (p.blobUrl) URL.revokeObjectURL(p.blobUrl);
    p.blobUrl = blobUrl;

    p.speed = msg.speed;
    p.origDuration = msg.orig_duration;
    p.recordingId = msg.recording_id;
    p.emptyEl.classList.add('d-none');
    setLoading(p, false);

    const ws = WaveSurfer.create({
      container: p.waveEl,
      height: WAVE_HEIGHT,
      waveColor: '#8fbf9a',
      progressColor: '#2E6B4F',
      cursorColor: '#ff5722',
      cursorWidth: 2,
      normalize: true,
      dragToSeek: true,
      autoScroll: true,
      autoCenter: true,
      url: blobUrl,
      peaks: [msg.peaks],
      duration: msg.play_duration
    });

    // Imagen del espectrograma dentro del contenedor de wavesurfer
    // (los estilos van en línea: el CSS de la página no llega al interior de wavesurfer)
    const spectro = document.createElement('div');
    spectro.className = 'bs-spectro';
    spectro.style.cssText = 'position:relative;width:100%;height:' + SPECTRO_HEIGHT + 'px;';
    const img = document.createElement('img');
    img.src = msg.spectro_url;
    img.alt = '';
    img.draggable = false;
    img.style.cssText = 'position:absolute;inset:0;width:100%;height:100%;display:block;' +
      'user-select:none;cursor:crosshair;';
    spectro.appendChild(img);
    ws.getWrapper().appendChild(spectro);
    p.img = img;

    // Capa para los rectángulos de detección y para dibujar anotaciones
    const root = ws.getWrapper().getRootNode();
    if (root && root.appendChild && !root.querySelector('style.bs-overlay-style')) {
      const style = document.createElement('style');
      style.className = 'bs-overlay-style';
      style.textContent = OVERLAY_CSS;
      root.appendChild(style);
    }
    const overlay = document.createElement('div');
    overlay.className = 'bs-overlay';
    spectro.appendChild(overlay);
    p.overlay = overlay;
    p.drawing = false;
    bindDrawing(p, overlay);

    // Línea de tiempo (debajo), con el tiempo real de la grabación
    ws.registerPlugin(WaveSurfer.Timeline.create({
      height: 18,
      formatTimeCallback: (s) => formatTime(s * p.speed),
      style: { fontSize: '11px' }
    }));

    ws.on('ready', () => {
      if (p.zoom > 1) setZoom(p, p.zoom);
      if (fraction > 0) ws.seekTo(fraction);
      if (wasPlaying) ws.play();
      updateTime(p, ws.getCurrentTime());
    });
    ws.on('play', () => updatePlayButton(p, true));
    ws.on('pause', () => { updatePlayButton(p, false); p.stopAt = null; });
    ws.on('finish', () => updatePlayButton(p, false));
    ws.on('timeupdate', (t) => {
      updateTime(p, t);
      if (p.stopAt !== null && p.stopAt !== undefined && t >= p.stopAt) {
        p.stopAt = null;
        ws.pause();
      }
    });
    ws.on('seeking', (t) => updateTime(p, t));
    ws.on('error', (e) => console.error('[biosonora] audio', e));

    p.ws = ws;
    updatePlayButton(p, false);
    updateTime(p, 0);
    drawAxis(p, msg.fmin, msg.fmax);
    renderBoxes(p);
  }

  // Solo cambia la imagen (rango de frecuencias, contraste o paleta)
  function updateSpectrogram(msg) {
    const p = getPlayer(msg.id);
    if (!p || !p.img) return;
    p.img.src = msg.spectro_url;
    drawAxis(p, msg.fmin, msg.fmax);
    renderBoxes(p); // las posiciones dependen del rango de frecuencias
  }

  function showLoading(msg) {
    const p = getPlayer(msg.id);
    if (!p) return;
    if (p.ws) p.ws.pause();
    p.emptyEl.classList.add('d-none');
    setLoading(p, true);
  }

  function showError(msg) {
    const p = getPlayer(msg.id);
    if (p) setLoading(p, false);
  }

  // ---- Detecciones sobre el espectrograma ----
  function setDetections(msg) {
    const p = getPlayer(msg.id);
    if (!p) return;
    p.items = msg.items || [];
    renderBoxes(p);
  }

  function renderBoxes(p) {
    const overlay = p.overlay;
    if (!overlay || !p.origDuration || p.fmax === undefined) return;
    overlay.querySelectorAll('.bs-box').forEach((el) => el.remove());
    const span = p.fmax - p.fmin;
    const labelsAt = {}; // etiquetas que empiezan en el mismo instante se apilan
    (p.items || []).forEach((d) => {
      const high = Math.min(d.high, p.fmax);
      const low = Math.max(d.low, p.fmin);
      if (high <= low) return; // fuera del rango de frecuencias visible
      const box = document.createElement('div');
      box.className = 'bs-box ' + d.state + (d.category === 'human' ? ' human' : '') +
        (d.selected ? ' selected' : '');
      box.style.left = (d.start / p.origDuration * 100) + '%';
      box.style.width = ((d.end - d.start) / p.origDuration * 100) + '%';
      box.style.top = ((p.fmax - high) / span * 100) + '%';
      box.style.height = ((high - low) / span * 100) + '%';
      const label = document.createElement('span');
      label.className = 'bs-box-label';
      // Si el rectángulo es estrecho, solo el símbolo de estado (el nombre
      // completo aparece al pasar el ratón); la seleccionada, siempre completa
      const widthPx = (d.end - d.start) / p.origDuration * overlay.clientWidth;
      label.textContent = (d.selected || widthPx >= 90) ? d.label : d.label.split(' ')[0];
      label.title = d.label;
      const key = d.start.toFixed(2);
      labelsAt[key] = (labelsAt[key] || 0) + 1;
      label.style.top = ((labelsAt[key] - 1) * 16) + 'px';
      // Clic en la etiqueta: seleccionar la detección y saltar a su inicio
      label.addEventListener('pointerdown', (e) => e.stopPropagation());
      label.addEventListener('click', (e) => {
        e.stopPropagation();
        if (p.ws) p.ws.setTime(d.start / p.speed);
        Shiny.setInputValue('biosonora_box_click', { id: d.id, t: Date.now() }, { priority: 'event' });
      });
      box.appendChild(label);
      overlay.appendChild(box);
    });
  }

  // Modo "nueva anotación": el usuario dibuja un rectángulo (tiempo x frecuencia)
  function startDrawing(msg) {
    const p = getPlayer(msg.id);
    if (!p || !p.overlay) return;
    if (p.ws) p.ws.pause();
    p.drawing = true;
    p.overlay.classList.add('drawing');
  }

  function stopDrawing(p) {
    p.drawing = false;
    if (p.overlay) {
      p.overlay.classList.remove('drawing');
      p.overlay.querySelectorAll('.bs-draw-rect').forEach((el) => el.remove());
    }
  }

  function bindDrawing(p, overlay) {
    let start = null;
    let rectEl = null;
    const pos = (e) => {
      const r = overlay.getBoundingClientRect();
      return {
        x: Math.min(1, Math.max(0, (e.clientX - r.left) / r.width)),
        y: Math.min(1, Math.max(0, (e.clientY - r.top) / r.height)),
        w: r.width, h: r.height
      };
    };
    // Mientras se dibuja, los clics no llegan a wavesurfer (no saltan)
    ['pointerdown', 'pointermove', 'pointerup', 'click'].forEach((type) => {
      overlay.addEventListener(type, (e) => { if (p.drawing) e.stopPropagation(); });
    });
    overlay.addEventListener('pointerdown', (e) => {
      if (!p.drawing) return;
      e.preventDefault();
      start = pos(e);
      rectEl = document.createElement('div');
      rectEl.className = 'bs-draw-rect';
      overlay.appendChild(rectEl);
      overlay.setPointerCapture(e.pointerId);
    });
    overlay.addEventListener('pointermove', (e) => {
      if (!p.drawing || !start || !rectEl) return;
      const c = pos(e);
      rectEl.style.left = (Math.min(start.x, c.x) * 100) + '%';
      rectEl.style.top = (Math.min(start.y, c.y) * 100) + '%';
      rectEl.style.width = (Math.abs(c.x - start.x) * 100) + '%';
      rectEl.style.height = (Math.abs(c.y - start.y) * 100) + '%';
    });
    overlay.addEventListener('pointerup', (e) => {
      if (!p.drawing || !start) return;
      const c = pos(e);
      const tooSmall = Math.abs(c.x - start.x) * c.w < 4 || Math.abs(c.y - start.y) * c.h < 4;
      const x0 = Math.min(start.x, c.x), x1 = Math.max(start.x, c.x);
      const y0 = Math.min(start.y, c.y), y1 = Math.max(start.y, c.y);
      start = null;
      stopDrawing(p);
      if (tooSmall) return;
      const span = p.fmax - p.fmin;
      Shiny.setInputValue('biosonora_new_box', {
        start_s: x0 * p.origDuration,
        end_s: x1 * p.origDuration,
        low_hz: p.fmax - y1 * span,
        high_hz: p.fmax - y0 * span,
        t: Date.now()
      }, { priority: 'event' });
    });
  }

  // Reproducir solo un fragmento (tiempos reales de la grabación)
  function playRange(msg) {
    const p = getPlayer(msg.id);
    if (!p || !p.ws) return;
    p.ws.setTime(msg.start / p.speed);
    p.ws.play();
    p.stopAt = msg.end / p.speed; // se fija después de play(): la pausa previa lo borraría
  }

  // ---- Atajos de teclado ----
  // Se ignoran mientras se escribe en un campo de texto.
  function isTyping(e) {
    const el = e.target;
    if (!el) return false;
    const tag = (el.tagName || '').toLowerCase();
    if (tag === 'input') {
      // Casillas, botones de opción y deslizadores no son "escribir"
      const type = (el.type || 'text').toLowerCase();
      return !['radio', 'checkbox', 'button', 'range', 'submit'].includes(type);
    }
    return tag === 'textarea' || tag === 'select' || el.isContentEditable;
  }

  document.addEventListener('keydown', function (e) {
    if (e.ctrlKey || e.metaKey || e.altKey || isTyping(e)) return;
    const p = Object.values(players)[0];
    switch (e.key) {
      case ' ':
        e.preventDefault();
        togglePlay(p);
        return;
      case 'ArrowRight':
        e.preventDefault();
        seekRelative(p, 2);
        return;
      case 'ArrowLeft':
        e.preventDefault();
        seekRelative(p, -2);
        return;
      case '+':
        if (p) setZoom(p, p.zoom * 2);
        return;
      case '-':
        if (p) setZoom(p, p.zoom / 2);
        return;
      case 'Escape':
        if (p && p.drawing) stopDrawing(p);
        return;
      case 'ArrowUp':
      case 'ArrowDown':
        e.preventDefault();
        Shiny.setInputValue('biosonora_key', { key: e.key, t: Date.now() }, { priority: 'event' });
        return;
      default:
        if (/^[nNpPrRvVxXdDfFaA1-4]$/.test(e.key) && window.Shiny) {
          Shiny.setInputValue('biosonora_key', { key: e.key, t: Date.now() }, { priority: 'event' });
        }
    }
  });

  // Pista abierta: se resalta en la tabla
  function setCurrentTrack(msg) {
    window.biosonoraCurrentTrack = msg.id;
  }

  $(function () {
    Shiny.addCustomMessageHandler('biosonora-viewer-load', load);
    Shiny.addCustomMessageHandler('biosonora-viewer-spectro', updateSpectrogram);
    Shiny.addCustomMessageHandler('biosonora-viewer-loading', showLoading);
    Shiny.addCustomMessageHandler('biosonora-viewer-error', showError);
    Shiny.addCustomMessageHandler('biosonora-current-track', setCurrentTrack);
    Shiny.addCustomMessageHandler('biosonora-viewer-detections', setDetections);
    Shiny.addCustomMessageHandler('biosonora-viewer-draw', startDrawing);
    Shiny.addCustomMessageHandler('biosonora-viewer-play-range', playRange);
  });
})();
