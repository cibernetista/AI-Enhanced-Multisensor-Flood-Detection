// ======== CONFIGURACIÓN y ESTADO CENTRALIZADO ========
const CONFIG_DEFAULTS = { 
  endpoint: '/api/alerts', 
  token: '', 
  thresholdKm2: 50,
  initialCenter: [7.856272, 30.040413], // Lat, Lng
  initialZoom: 5
};

let map, baseLight, markersLayer;
let chart;
let AOI_DATA = {}; // Objeto para almacenar { AOI_ID: [Lat, Lng], ... }
let state = { 
  alerts: [], 
  timeseries: [],
  // Cargar configuración de localStorage o usar defaults
  config: JSON.parse(localStorage.getItem('dashboardConfig')) || CONFIG_DEFAULTS
};

// ======== Utilidades de Fechas y Formato ========
const sevColor = (s) => ({ low: '#8ecae6', medium: '#ffb703', high: '#d00000' }[s]);
const fmt = new Intl.NumberFormat('es-MX');

const iso = (d) => d.toISOString().slice(0,10);
function dateRange(from, to){
  const out = [];
  const d0 = new Date(from + 'T00:00:00Z');
  const d1 = new Date(to   + 'T00:00:00Z');
  for(let d = new Date(d0); d <= d1; d.setDate(d.getDate()+1)){
    out.push(iso(d));
  }
  return out;
}

// ======== Lógica de Carga de AOI (desde GeoJSON) ========
async function loadAoiData(){
  try {
    // Nota: La ruta al archivo GeoJSON debe ser correcta. Suponemos 'data/AOI.geojson'.
    const response = await fetch('data/AOI.geojson'); 
    const geojson = await response.json();
    
    // Procesar GeoJSON para extraer centroides y mapear a [Lat, Lng]
    geojson.features
      .filter(f => f.properties.kind === 'centroid')
      .forEach(f => {
        const aoiId = f.properties.aoi_id;
        // GeoJSON usa [Lng, Lat], lo almacenamos como [Lat, Lng] para consistencia en JS
        const [lon, lat] = f.geometry.coordinates; 
        AOI_DATA[aoiId] = [lat, lon];
      });
      
    // Si hay datos, usar el centro de la primera AOI como centro inicial
    if (Object.keys(AOI_DATA).length > 0 && 
       (state.config.initialCenter.join(',') === CONFIG_DEFAULTS.initialCenter.join(','))) {
        const firstAoiCenter = Object.values(AOI_DATA)[0];
        state.config.initialCenter = firstAoiCenter;
    }
    
  } catch (err) {
    console.error('Error cargando datos AOI desde GeoJSON', err);
  }
}

// Función de detección de riesgo simple (para el chart)
const calculateFloodRisk = (rain, alerts) => Math.min(10, Math.round(rain * 0.5 + alerts * 2));

// Base para la simulación de alertas (se complementa con los centroides de AOI_DATA)
// EL BLOQUE ALERTS_BASE FUE ELIMINADO Y MOVIDO A data/alerts.json

async function fetchAlertsAPI(params){
  // 1. Cargar datos desde el nuevo archivo JSON
  const alertsResponse = await fetch('data/alerts.json');
  const alertsData = await alertsResponse.json();
  
  await new Promise(r=>setTimeout(r,500)); // Simula latencia (después del fetch para simular un API más realista)
  
  const threshold = Number(state.config.thresholdKm2) || 50; 
  const highThreshold = threshold * 2;
  
  // Asignar centroides [Lat, Lng] desde AOI_DATA
  const rawAlerts = alertsData // <--- Ahora usamos los datos cargados del JSON
    .filter(a => AOI_DATA[a.aoi])
    .map(a => {
      const centroid = AOI_DATA[a.aoi]; // [Lat, Lng]
      return { ...a, centroid };
  });

  // Aplicar lógica de severidad dinámica
  const alerts = rawAlerts.map(a => {
    let severity;
    if (a.areaKm2 >= highThreshold) {
        severity = 'high';
    } else if (a.areaKm2 >= threshold) {
        severity = 'medium';
    } else {
        severity = 'low';
    }
    return { ...a, severity };
  }).filter(a => params.aoi === 'Default' || a.aoi === params.aoi); // Filtrar por AOI

  return { alerts };
}


// ======== Fusión lluvia + alertas + riesgo por día ========
function mergeSeriesWithAlerts(rainSeries, alerts){
  const countByDay = {};
  alerts.forEach(a=>{
    const d = new Date(a.when);
    const day = iso(new Date(Date.UTC(d.getUTCFullYear(), d.getUTCMonth(), d.getUTCDate())));
    countByDay[day] = (countByDay[day] || 0) + 1;
  });
  return rainSeries.map(r => {
    const alertsCount = countByDay[r.date] || 0;
    const risk = calculateFloodRisk(r.rain, alertsCount);
    return { date:r.date, rain:r.rain, alerts: alertsCount, risk: risk };
  });
}

// ======== Funciones de Configuración ========
const settingsDialog = document.getElementById('settings');

function populateSettingsDialog(){
  document.getElementById('endpoint').value = state.config.endpoint;
  document.getElementById('token').value = state.config.token;
  document.getElementById('thr').value = state.config.thresholdKm2;
}

function saveSettings(event){
  event.preventDefault(); 
  const formData = new FormData(event.target);
  const newConfig = {
    endpoint: formData.get('endpoint'),
    token: formData.get('token'),
    thresholdKm2: Number(formData.get('thresholdKm2')) || CONFIG_DEFAULTS.thresholdKm2
  };
  
  state.config = newConfig;
  localStorage.setItem('dashboardConfig', JSON.stringify(newConfig));
  
  settingsDialog.close();
  reload(); 
}

// ======== Renderizado ========

function populateAoiSelect(){
  const select = document.getElementById('aoi');
  // Limpia todas las opciones excepto la primera ('Default')
  while (select.options.length > 1) {
    select.remove(1);
  }
  
  // Rellena con los datos cargados de AOI_DATA
  Object.keys(AOI_DATA).sort().forEach(aoiId => {
    const option = document.createElement('option');
    option.value = aoiId;
    option.textContent = aoiId;
    select.appendChild(option);
  });
}

function initMap(){
  // Leaflet usa [Lat, Lng]
  map = L.map('map', { zoomControl:true, attributionControl:true }).setView(state.config.initialCenter, state.config.initialZoom);
  baseLight = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
    maxZoom:19, attribution: '&copy; OpenStreetMap'
  }).addTo(map);
  markersLayer = L.layerGroup().addTo(map);

  document.getElementById('mapLegend').style.display = 'block';
}

function renderAlerts(){
  markersLayer.clearLayers();
  state.alerts.forEach(a=>{
    const r = Math.max(8, Math.min(20, Math.sqrt(a.areaKm2))); 
    // a.centroid es [Lat, Lng], que es lo que espera L.circleMarker
    const marker = L.circleMarker(a.centroid, { 
        radius:r, 
        color:sevColor(a.severity), 
        fillColor:sevColor(a.severity), 
        fillOpacity:.6, 
        weight:2 
    });
    const html = `
      <div style="font-size:14px">
      <b>${a.aoi} (${a.severity.toUpperCase()})</b><br/>
      Área: ${a.areaKm2.toFixed(1)} km²<br/>
      Población: ${fmt.format(a.people)}<br/>
      Fecha: ${new Date(a.when).toLocaleString('es-MX')}
      </div>
    `;
    marker.bindPopup(html);
    markersLayer.addLayer(marker);
  });
}

function updateKPIs(){
  const area = state.alerts.reduce((s,a)=>s+a.areaKm2,0);
  const people = state.alerts.reduce((s,a)=>s+a.people,0);
  const counts = state.alerts.reduce((acc,a)=>{acc[a.severity]++; return acc},{high:0,medium:0,low:0});
  document.getElementById('kpiArea').textContent = `${Math.round(area).toLocaleString('es-MX')} km²`;
  document.getElementById('kpiPeople').textContent = `${fmt.format(people)}`;
  document.getElementById('kpiCounts').textContent = `${counts.high}/${counts.medium}/${counts.low}`;
  document.getElementById('lastUpdate').textContent = new Date().toLocaleString('es-MX');
}

function renderChart(){
  const ctx = document.getElementById('tsChart').getContext('2d');
  const labels = state.timeseries.map(d=>d.date);
  const rain = state.timeseries.map(d=>d.rain);
  const alerts = state.timeseries.map(d=>d.alerts);
  const risk = state.timeseries.map(d=>d.risk); 
  if(chart) chart.destroy();
  chart = new Chart(ctx, {
    type:'bar',
    data:{ labels, datasets:[
      { type:'bar',  label:'Lluvia (mm)', data:rain,  yAxisID:'y',  borderWidth:0, backgroundColor: '#8ecae6' },
      { type:'line', label:'Riesgo de Inundación', data:risk, yAxisID:'y1', borderColor: '#d00000', backgroundColor: 'rgba(208, 0, 0, 0.1)', fill:true, tension:.4, borderWidth:2, pointRadius:3 },
      { type:'line', label:'# Alertas',  data:alerts, yAxisID:'y2', borderColor: '#117161', tension:.3, borderWidth:2, pointRadius:0, hidden:true }
    ]},
    options:{
      responsive:true, maintainAspectRatio:false,
      plugins:{ legend:{ position:'top' }, tooltip:{ mode:'index', intersect:false } },
      scales:{
        y:{  position:'left',  title:{ display:true, text:'Lluvia (mm)' } },
        y1:{ position:'right', grid:{ drawOnChartArea:false }, title:{ display:true, text:'Riesgo (0-10)' }, suggestedMax: 10, ticks:{stepSize:1} },
        y2:{ position:'right', grid:{ drawOnChartArea:false }, title:{ display:true, text:'Alertas (#)' }, suggestedMax: Math.max(3, Math.max(...alerts)+1), hidden:true }
      }
    }
  });
}

function fillNotifications(){
  const box = document.getElementById('notifList');
  box.innerHTML = '';
  const recentAlerts = state.alerts.sort((a,b) => new Date(b.when) - new Date(a.when)).slice(0, 8); 
  
  if(recentAlerts.length === 0){
    box.innerHTML = '<div style="color:var(--muted);text-align:center;padding:20px">No se han detectado notificaciones en este período.</div>';
    return;
  }

  recentAlerts.forEach(a=>{
    const div = document.createElement('div');
    div.className = 'notif';
    const chipClass = a.severity === 'high' ? 'chip high' : a.severity === 'medium' ? 'chip medium' : 'chip low';
    div.innerHTML = `
      <div style="min-width:0; flex:1">
        <div style="font-weight:600;white-space:nowrap;overflow:hidden;text-overflow:ellipsis">${a.aoi} (${a.areaKm2.toFixed(1)} km²)</div>
        <div style="font-size:12px;color:var(--muted)">${new Date(a.when).toLocaleDateString('es-MX', {hour:'2-digit', minute:'2-digit'})}</div>
      </div>
      <span class="${chipClass}">${a.severity.toUpperCase()}</span>
      <button class="btn ghost" data-lat="${a.centroid[0]}" data-lng="${a.centroid[1]}">Ver</button>
    `;
    div.querySelector('button').addEventListener('click', (e)=>{
      const lat = parseFloat(e.target.getAttribute('data-lat'));
      const lng = parseFloat(e.target.getAttribute('data-lng'));
      map.flyTo([lat,lng], 10, { duration:.8 }); 
    });
    box.appendChild(div);
  });
}

function fillTable(){
  const tb = document.getElementById('alertsTable');
  tb.innerHTML = '';
  state.alerts.forEach(a=>{
    const tr = document.createElement('tr');
    tr.innerHTML = `
      <td><code>${a.id}</code></td>
      <td>${new Date(a.when).toLocaleString('es-MX')}</td>
      <td>${a.aoi}</td>
      <td><span class="chip ${a.severity==='high'?'high':a.severity==='medium'?'medium':'low'}">${a.severity.toUpperCase()}</span></td>
      <td>${a.areaKm2.toFixed(1)}</td>
      <td>${fmt.format(a.people)}</td>
      <td><button class="btn ghost" data-lat="${a.centroid[0]}" data-lng="${a.centroid[1]}">Zoom</button></td>
    `;
    tr.querySelector('button').addEventListener('click', (e)=>{
      const lat = parseFloat(e.target.getAttribute('data-lat'));
      const lng = parseFloat(e.target.getAttribute('data-lng'));
      map.flyTo([lat,lng], 10, { duration:.8 });
    });
    tb.appendChild(tr);
  });
}

function exportCSV(){
  const header = ['id','fecha_hora','aoi','severidad','area_km2','poblacion','lat','lng'];
  const rows = state.alerts.map(a=>[a.id,a.when,a.aoi,a.severity,a.areaKm2,a.people,a.centroid[0],a.centroid[1]]); // [Lat, Lng]
  const csv = [header.join(','), ...rows.map(r=>r.map(v => (typeof v === 'string' && v.includes(',')) ? `"${v}"` : v).join(','))].join('\n');
  const blob = new Blob([csv], {type:'text/csv;charset=utf-8;'});
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url; a.download = `alerts_${new Date().toISOString().slice(0,10)}.csv`;
  document.body.appendChild(a); a.click(); document.body.removeChild(a); URL.revokeObjectURL(url);
}

// ======== Control de Recarga ========
async function reload(){
  document.getElementById('refresh').disabled = true;
  const params = getParams();
  try{
    const [{ alerts }, rainSeries] = await Promise.all([
      fetchAlertsAPI(params),
      fetchOpenMeteoDaily(params.aoi, params.from, params.to)
    ]);
    state.alerts = alerts;
    state.timeseries = mergeSeriesWithAlerts(rainSeries, alerts);

    renderAlerts(); 
    updateKPIs(); 
    renderChart(); 
    fillNotifications(); 
    fillTable();
  }catch(err){
    console.error('Error recargando datos', err);
    if(!state.timeseries.length){
      const days = dateRange(params.from, params.to);
      state.timeseries = days.map(d=>({date:d, rain:0, alerts:0, risk:0}));
      renderChart();
    }
  } finally {
    document.getElementById('refresh').disabled = false;
  }
}

function getParams(){
  const from = document.getElementById('from').value;
  const to = document.getElementById('to').value;
  const aoi = document.getElementById('aoi').value;
  return { from, to, aoi, hazard:'flood' };
}

// ======== Open-Meteo: precipitación diaria ERA5 ========
async function fetchOpenMeteoDaily(aoi, from, to){
  // points es un array de [Lat, Lng]
  const points = aoi === 'Default' 
      ? Object.values(AOI_DATA) 
      : AOI_DATA[aoi] ? [AOI_DATA[aoi]] : []; 
      
  const qs = (lat,lon) =>
    `https://archive-api.open-meteo.com/v1/era5?latitude=${lat}&longitude=${lon}`+
    `&start_date=${from}&end_date=${to}&daily=precipitation_sum&timezone=auto`;
  
  // Desestructuración ([lat, lon]) es correcta para puntos [Lat, Lng]
  const resps = await Promise.all(points.map(([lat,lon]) => fetch(qs(lat,lon)).then(r=>r.json())));
  const days = dateRange(from, to);
  const byDay = {};
  days.forEach(d => byDay[d] = []);

  resps.forEach(j=>{
    const dates = j?.daily?.time || [];
    const vals  = j?.daily?.precipitation_sum || [];
    dates.forEach((d, i)=>{
      if(byDay[d]) byDay[d].push(Number(vals[i] ?? 0));
    });
  });

  return days.map(d=>{
    const arr = byDay[d] ?? [];
    const avg = arr.length ? arr.reduce((s,x)=>s+x,0)/arr.length : 0;
    return { date:d, rain: Math.round(avg*10)/10 };
  });
}

// ======== Inicialización (BOOT) ========
(function(){
  // 1. Fechas por defecto (últimos 14 días)
  const now = new Date();
  const to = iso(now);
  const from = iso(new Date(now.getTime()-13*86400000));
  document.getElementById('from').value = from;
  document.getElementById('to').value = to;
  
  // 2. Inicialización de UI y Datos: Cargar AOI Data primero
  loadAoiData().then(()=>{
      populateAoiSelect(); 
      initMap();
      populateSettingsDialog();
      reload();
  }).catch(err => {
      console.error("Fallo al inicializar el dashboard.", err);
      initMap();
      populateSettingsDialog();
      reload();
  });

  // 3. Event Listeners
  document.getElementById('settingsForm').addEventListener('submit', saveSettings);
  document.getElementById('refresh').addEventListener('click', reload);
  document.getElementById('exportCsv').addEventListener('click', exportCSV);

  // Live refresh
  const live = document.getElementById('live');
  let timer = setInterval(()=> live.checked && reload(), 5*60*1000); // 5 minutos
  live.addEventListener('change', ()=>{
    clearInterval(timer);
    if(live.checked){ timer = setInterval(()=> reload(), 5*60*1000); }
  });

  // Recenter by aoi
  document.getElementById('aoi').addEventListener('change', (e)=>{
    const c = e.target.value;
    const coords = AOI_DATA[c]; // coords es [Lat, Lng]
    if (coords) {
        map.flyTo(coords, 10, { duration:.8 }); 
    } else {
        map.flyTo(state.config.initialCenter, state.config.initialZoom);
    }
    reload();
  });

  // Settings button
  document.getElementById('settingsBtn').addEventListener('click', ()=>{
    populateSettingsDialog();
    settingsDialog.showModal();
  });
})();