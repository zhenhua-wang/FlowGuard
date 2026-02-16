library(shiny)
library(shinyMobile)
library(leaflet)
library(shinyjs)

f7Page(
  title = "FlowGuard",
  options = list(theme = "auto", dark = FALSE, filled = FALSE),
  
  useShinyjs(),

  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Material+Symbols+Rounded:opsz,wght,FILL,GRAD@24,400,0,0"),
    tags$script(HTML("
      function showSubView(id) { $('#settings_main').addClass('hidden-left'); $('#' + id).addClass('active'); }
      function hideSubViews() { $('#settings_main').removeClass('hidden-left'); $('.settings-subview').removeClass('active'); }
      function selectRadio(el, groupName, val) { $(el).siblings().removeClass('selected'); $(el).addClass('selected'); Shiny.setInputValue(groupName, val); $('#val_' + groupName).text(val); }
      function toggleCheck(el, groupName) { $(el).toggleClass('selected'); let vals = []; $(el).parent().children('.selected').each(function() { vals.push($(this).attr('data-val')); }); Shiny.setInputValue(groupName, vals); $('#val_' + groupName).text(vals.length > 0 ? vals.join(', ') : 'None'); }
      
      function updateSliderColor(el) {
        const min = parseFloat(el.min); const max = parseFloat(el.max); const val = (el.value - min) / (max - min) * 100;
        el.style.setProperty('--value', val + '%');
        const tooltip = document.getElementById('slider_tooltip');
        tooltip.innerText = el.value; tooltip.classList.add('active');
        const offset = (50 - val) / 50 * 13; tooltip.style.left = 'calc(' + val + '% + ' + offset + 'px)';
        Shiny.setInputValue('traffic_days', el.value);
      }
      function hideSliderTooltip() { document.getElementById('slider_tooltip').classList.remove('active'); }
      
      let activeInputId = 'q_input'; 
      function handleSuggestClick(el) {
        let lat = $(el).attr('data-lat'); let lon = $(el).attr('data-lon'); let shortName = $(el).attr('data-short');
        $('#' + activeInputId).val(shortName); $('#search_suggestions').empty().removeClass('active');
        Shiny.setInputValue('fly_to_loc', {lat: lat, lon: lon, name: shortName, type: activeInputId, rand: Math.random()});
      }
      
      function triggerCustomRoute() {
        let start_val = $('#start_input').val().trim(); let end_val = $('#q_input').val().trim();
        let click_lat = $('#q_input').attr('data-clicked-lat'); let click_lon = $('#q_input').attr('data-clicked-lon');
        if(end_val === '') { alert('Please enter a destination.'); return; }
        
        Shiny.setInputValue('close_all', Math.random());
        Shiny.setInputValue('do_custom_route', {
          start: start_val, end: end_val, dest_lat: click_lat ? parseFloat(click_lat) : null, dest_lon: click_lon ? parseFloat(click_lon) : null, rand: Math.random()
        });
        $('#q_input').removeAttr('data-clicked-lat'); $('#q_input').removeAttr('data-clicked-lon');
      }
      
      function showToast(msg) {
        $('#fg-toast').text(msg).addClass('show'); setTimeout(function() { $('#fg-toast').removeClass('show'); }, 3000);
      }

      let watchId = null;
      let lastLocTime = 0;

      function startLocationTracking() {
        if (\"geolocation\" in navigator) {
          watchId = navigator.geolocation.watchPosition(function(position) {
            let now = Date.now();
            if (now - lastLocTime > 5000) {
                lastLocTime = now;
                Shiny.setInputValue(\"user_location\", {
                    lat: position.coords.latitude,
                    lon: position.coords.longitude,
                    rand: Math.random()
                });
            }
          }, function(error) {
            console.warn(\"Geolocation watch error: \" + error.message);
          }, { enableHighAccuracy: true, maximumAge: 2000, timeout: 10000 });
        }
      }

      function requestLocation() {
        if (\"geolocation\" in navigator) {
          navigator.geolocation.getCurrentPosition(function(position) {
            Shiny.setInputValue(\"user_location\", { lat: position.coords.latitude, lon: position.coords.longitude, rand: Math.random() });
            lastLocTime = Date.now(); 
          }, function(error) {
            showToast(\"Failed to get location. Please allow GPS.\");
          }, { enableHighAccuracy: true, timeout: 10000 });
        } else {
          showToast(\"Geolocation is not supported by your browser.\");
        }
      }

      $(document).ready(function() {
        let timeout = null;
        $(document).on('focus input', '#start_input, #q_input', function() {
          activeInputId = $(this).attr('id'); 
          if (activeInputId === 'q_input') { $(this).removeAttr('data-clicked-lat'); $(this).removeAttr('data-clicked-lon'); }
          clearTimeout(timeout); let query = $(this).val().trim();
          if (query.length < 2) { $('#search_suggestions').empty().removeClass('active'); return; }
          timeout = setTimeout(function() {
             let viewbox = '-93.0,39.5,-91.5,38.0'; 
             fetch(`https://nominatim.openstreetmap.org/search?q=${encodeURIComponent(query)}&format=json&limit=5&viewbox=${viewbox}`)
             .then(r => r.json()).then(data => {
                let html = '';
                data.forEach(item => {
                   let shortName = item.display_name.split(',')[0];
                   html += `<div class=\"suggestion-item\" data-lat=\"${item.lat}\" data-lon=\"${item.lon}\" data-short=\"${shortName.replace(/\"/g, '&quot;')}\" onclick=\"handleSuggestClick(this)\"><div class=\"suggestion-text\"><div class=\"sugg-title\">${shortName}</div><div class=\"sugg-sub\">${item.display_name}</div></div></div>`;
                });
                if(html !== '') { $('#search_suggestions').html(html).addClass('active'); } 
                else { $('#search_suggestions').html('<div style=\"padding: 16px; color: #888; text-align: center;\">No results found nearby</div>').addClass('active'); }
             }).catch(err => { console.error('Geocoding error:', err); });
          }, 400);
        });
        $(document).on('keyup', '#q_input, #start_input', function(e) { if(e.key === 'Enter' || e.keyCode === 13) { triggerCustomRoute(); $(this).blur(); } });
      });

      $(document).on('shiny:connected', function() { 
          requestLocation(); 
          startLocationTracking(); 
      });

      Shiny.addCustomMessageHandler('start_nav_ui', function(msg) {
         $('#nav_time').text(msg.time + ' min'); $('#nav_dist').text(msg.dist + ' mi');
         $('#bottom_tray').addClass('nav-active'); $('#nav_info_panel').addClass('active');    
         $('#view_capsule').addClass('nav-active'); 
         $('#q_input, #start_input').prop('disabled', true); $('.get-dir-btn').prop('disabled', true).css('opacity', '0.5');
         $('#search_trigger').css({'pointer-events': 'none', 'opacity': '0.5'});
      });
      
      Shiny.addCustomMessageHandler('end_nav_ui', function(msg) {
         $('#nav_info_panel').removeClass('active'); $('#bottom_tray').removeClass('nav-active'); 
         $('#view_capsule').removeClass('nav-active'); 
         $('#q_input, #start_input').prop('disabled', false).val(''); $('#start_input').val('Current Location');
         $('.get-dir-btn').prop('disabled', false).css('opacity', '1'); $('#search_trigger').css({'pointer-events': 'auto', 'opacity': '1'});
      });

      Shiny.addCustomMessageHandler('set_map_rotation', function(msg) {
         let mapEl = $('#fg-map');
         if (msg.mode === 'global') {
             mapEl.removeClass('map-driving-mode');
             mapEl.css('transform', 'translate(-50%, -50%) rotate(0deg)');
             $('#toggle_view_icon').text('explore'); 
             
             // 【核心动态层级】：全局模式下，将路线面板的 Z-index 降到 405 (沉在风险图层 410 之下)
             try {
                 let mapObj = HTMLWidgets.getInstance(document.getElementById('map')).getMap();
                 mapObj.getPane('route_pane').style.zIndex = 405; 
             } catch(e) {}
             
             let count = 0;
             let interval = setInterval(function() { window.dispatchEvent(new Event('resize')); count++; if(count > 10) clearInterval(interval); }, 50);
             
             if (msg.bounds) {
                 setTimeout(function() {
                     try {
                         let mapObj = HTMLWidgets.getInstance(document.getElementById('map')).getMap();
                         mapObj.invalidateSize(true); 
                         mapObj.fitBounds([
                             [msg.bounds.lat1, msg.bounds.lng1],
                             [msg.bounds.lat2, msg.bounds.lng2]
                         ], {
                             paddingTopLeft: [50, 120],     
                             paddingBottomRight: [50, 320], 
                             animate: true,
                             duration: 1
                         });
                     } catch(e) { console.log(e); }
                 }, 450); 
             }
         } else {
             mapEl.addClass('map-driving-mode');
             mapEl.css('transform', 'translate(-50%, -50%) rotate(' + msg.deg + 'deg)');
             $('#toggle_view_icon').text('route'); 
             
             // 【核心动态层级】：第一视角模式下，将路线面板的 Z-index 提至 430 (悬浮在风险图层 410 之上)
             try {
                 let mapObj = HTMLWidgets.getInstance(document.getElementById('map')).getMap();
                 mapObj.getPane('route_pane').style.zIndex = 430; 
             } catch(e) {}
             
             let count = 0;
             let interval = setInterval(function() { window.dispatchEvent(new Event('resize')); count++; if(count > 10) clearInterval(interval); }, 50);
         }
      });
      
      Shiny.addCustomMessageHandler('update_risk_level', function(msg) {
         let level = msg.level; 
         $('#risk_btn').removeClass('risk-active risk-low risk-medium risk-high');
         if (level === 'Low') { $('#risk_btn').addClass('risk-active risk-low'); $('#risk_text').text('Low'); } 
         else if (level === 'Medium') { $('#risk_btn').addClass('risk-active risk-medium'); $('#risk_text').text('Medium'); } 
         else if (level === 'High') { $('#risk_btn').addClass('risk-active risk-high'); $('#risk_text').text('High'); }
      });

      Shiny.addCustomMessageHandler('update_ai_advice', function(msg) {
         $('#ai_advice_text').html(msg.text).css({ 'font-style': 'normal', 'color': '#000', 'text-align': 'left', 'line-height': '1.5' });
      });
    ")),

    tags$style(HTML("
      body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; overflow: hidden; }
      
      path.smooth-loc-marker, path.smooth-halo {
        transition: d 2.5s cubic-bezier(0.25, 1, 0.5, 1), fill 0.5s ease, fill-opacity 0.5s ease !important;
      }
      
      #fg-map { 
        position: fixed; top: 50%; left: 50%; width: 100vw; height: 100vh; 
        transform: translate(-50%, -50%) rotate(0deg); z-index: 1; 
        transition: transform 1.2s cubic-bezier(0.68, -0.55, 0.265, 1.55); 
      }
      #fg-map.map-driving-mode { width: 150vmax; height: 150vmax; }
      
      :root { --ios-glass-bg: rgba(255, 255, 255, 0.45); --ios-glass-blur: blur(35px) saturate(190%); --ios-glass-border: 1px solid rgba(255, 255, 255, 0.5); --ios-glass-shadow: 0 10px 30px rgba(0,0,0,0.15); }
      .material-symbols-rounded { font-variation-settings: 'FILL' 0, 'wght' 400, 'GRAD' 0, 'opsz' 24; font-size: 28px; display: flex; align-items: center; justify-content: center; transition: color 0.4s ease; }
      .risk-indicator .material-symbols-rounded { color: #000; }
      .ios-panel { position: fixed; z-index: 10001; background: var(--ios-glass-bg) !important; backdrop-filter: var(--ios-glass-blur); -webkit-backdrop-filter: var(--ios-glass-blur); border: var(--ios-glass-border); box-shadow: 0 25px 50px rgba(0,0,0,0.2); opacity: 0; pointer-events: none; transition: transform 0.4s cubic-bezier(0.19, 1, 0.22, 1), opacity 0.4s ease; padding: 26px; box-sizing: border-box; display: flex; flex-direction: column; backface-visibility: hidden; }
      .ios-panel.active { opacity: 1; pointer-events: auto; }
      .panel-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 22px; }
      .panel-title { font-size: 24px; font-weight: 800; color: #000; margin: 0; }
      .panel-close-btn { background: rgba(0,0,0,0.06) !important; border: none; width: 30px; height: 30px; border-radius: 50%; color: #555; font-size: 22px; display: flex; align-items: center; justify-content: center; cursor: pointer; transform: translateZ(0); outline: none !important; }
      #risk_panel { top: 16px; left: 12px; right: 12px; height: 45vh; border-radius: 28px; transform-origin: 23px 23px; transform: scale(0); padding: 0; overflow: hidden;}
      #risk_panel.active { transform: scale(1); }
      #settings_panel { top: 85px; bottom: 12px; left: 12px; right: 12px; border-radius: 30px; transform: translateY(100%) scale(0.9); padding: 0; overflow: hidden; }
      #settings_panel.active { transform: translateY(0) scale(1); }
      #search_panel { bottom: 90px; left: 12px; right: 12px; border-radius: 28px; transform: translateY(100vh); opacity: 1 !important; transition: transform 0.4s cubic-bezier(0.19, 1, 0.22, 1) !important; }
      #search_panel.active { transform: translateY(0); }
      .settings-view { position: absolute; top: 0; left: 0; right: 0; bottom: 0; padding: 26px 16px; box-sizing: border-box; overflow-y: auto; background: transparent; transition: transform 0.4s cubic-bezier(0.36, 0.66, 0.04, 1), opacity 0.4s ease; }
      #settings_main { transform: translateX(0); opacity: 1; }
      #settings_main.hidden-left { transform: translateX(-30%); opacity: 0; pointer-events: none; }
      .settings-subview { transform: translateX(100%); opacity: 0; pointer-events: none; }
      .settings-subview.active { transform: translateX(0); opacity: 1; pointer-events: auto; }
      .subview-header { display: flex; align-items: center; justify-content: space-between; margin-bottom: 24px; position: relative; }
      .subview-header .back-btn { background: none; border: none; color: #007AFF; font-size: 17px; display: flex; align-items: center; padding: 0; cursor: pointer; }
      .subview-header .subview-title { font-size: 17px; font-weight: 600; color: #000; margin: 0; position: absolute; left: 50%; transform: translateX(-50%); pointer-events: none; }
      .ios-section-title { font-size: 13px; color: #6D6D72; margin: 16px 0 8px 16px; text-transform: uppercase; }
      .ios-group { background: rgba(255, 255, 255, 0.65); border-radius: 12px; margin-bottom: 24px; overflow: hidden; box-shadow: 0 1px 4px rgba(0,0,0,0.05); }
      .ios-row { display: flex; justify-content: space-between; align-items: center; padding: 12px 16px; min-height: 44px; border-bottom: 0.5px solid rgba(0,0,0,0.1); cursor: pointer; position: relative; }
      .ios-row:last-child { border-bottom: none; }
      .ios-row-title { font-size: 17px; color: #000; }
      .ios-row-val { font-size: 17px; color: #8E8E93; display: flex; align-items: center; gap: 8px; }
      .ios-chevron { color: #C7C7CC; font-size: 20px; font-weight: bold; margin-bottom: 2px;}
      .ios-checkmark { color: #007AFF; font-size: 20px; font-weight: bold; display: none; }
      .ios-row.selected .ios-checkmark { display: block; }
      .slider-row { padding: 14px 16px; display: flex; align-items: center; gap: 14px; }
      .slider-label { font-size: 15px; color: #8E8E93; font-weight: 500; }
      .slider-container { position: relative; flex-grow: 1; display: flex; align-items: center; }
      .slider-tooltip { position: absolute; top: -28px; transform: translateX(-50%); background: rgba(0, 0, 0, 0.8); color: #fff; padding: 4px 10px; border-radius: 8px; font-size: 14px; font-weight: 600; opacity: 0; pointer-events: none; transition: opacity 0.2s ease; white-space: nowrap; z-index: 10; box-shadow: 0 4px 12px rgba(0,0,0,0.15); }
      .slider-tooltip.active { opacity: 1; }
      .slider-tooltip::after { content: ''; position: absolute; bottom: -4px; left: 50%; transform: translateX(-50%); border-width: 4px 5px 0; border-style: solid; border-color: rgba(0, 0, 0, 0.8) transparent transparent transparent; }
      input[type=range].ios-slider { width: 100%; -webkit-appearance: none; background: transparent; height: 30px; --value: 48.27%; }
      input[type=range].ios-slider::-webkit-slider-thumb { -webkit-appearance: none; height: 26px; width: 26px; border-radius: 50%; background: #fff; box-shadow: 0 1px 5px rgba(0,0,0,0.3); cursor: pointer; margin-top: -11px; }
      input[type=range].ios-slider::-webkit-slider-runnable-track { width: 100%; height: 4px; border-radius: 2px; background: linear-gradient(to right, #34c759 0%, #34c759 var(--value), #D1D1D6 var(--value), #D1D1D6 100%); }
      .ios-switch { position: relative; display: inline-block; width: 51px; height: 31px; }
      .ios-switch input { opacity: 0; width: 0; height: 0; }
      .ios-slider-toggle { position: absolute; cursor: pointer; top: 0; left: 0; right: 0; bottom: 0; background-color: #e9e9ea; transition: .4s; border-radius: 31px; }
      .ios-slider-toggle:before { position: absolute; content: ''; height: 27px; width: 27px; left: 2px; bottom: 2px; background-color: white; transition: .4s; border-radius: 50%; box-shadow: 0 2px 4px rgba(0,0,0,0.2); }
      .ios-switch input:checked + .ios-slider-toggle { background-color: #34c759; }
      .ios-switch input:checked + .ios-slider-toggle:before { transform: translateX(20px); }
      @keyframes glow-low { 0% { box-shadow: 0 0 0 0 rgba(52, 199, 89, 0.5); } 70% { box-shadow: 0 0 0 15px rgba(52, 199, 89, 0); } 100% { box-shadow: 0 0 0 0 rgba(52, 199, 89, 0); } }
      @keyframes glow-medium { 0% { box-shadow: 0 0 0 0 rgba(255, 204, 0, 0.6); } 70% { box-shadow: 0 0 0 15px rgba(255, 204, 0, 0); } 100% { box-shadow: 0 0 0 0 rgba(255, 204, 0, 0); } }
      @keyframes glow-high { 0% { box-shadow: 0 0 0 0 rgba(255, 59, 48, 0.6); } 70% { box-shadow: 0 0 0 15px rgba(255, 59, 48, 0); } 100% { box-shadow: 0 0 0 0 rgba(255, 59, 48, 0); } }
      
      .risk-indicator { position: fixed; top: 16px; left: 12px; height: 46px; width: 46px; z-index: 999999 !important; background: var(--ios-glass-bg) !important; backdrop-filter: var(--ios-glass-blur); -webkit-backdrop-filter: var(--ios-glass-blur); border: var(--ios-glass-border); border-radius: 23px; box-shadow: var(--ios-glass-shadow); padding: 0; outline: none; cursor: pointer; transform: translateZ(0); display: flex; align-items: center; justify-content: center; transition: width 0.4s cubic-bezier(0.19, 1, 0.22, 1), transform 0.6s cubic-bezier(0.19, 1, 0.22, 1), border-color 0.4s ease; }
      .risk-indicator.panel-open { transform: translateY(-100px) scale(0.8); pointer-events: none; }
      .risk-indicator.risk-active { width: 125px; }
      #risk_text { max-width: 0; opacity: 0; overflow: hidden; white-space: nowrap; font-weight: 800; font-size: 16px; margin-left: 0; transition: max-width 0.4s ease, opacity 0.3s ease, margin-left 0.3s ease, color 0.4s ease; }
      .risk-indicator.risk-active #risk_text { max-width: 75px; opacity: 1; margin-left: 6px; }
      .risk-low { animation: glow-low 2s infinite; border: 1px solid rgba(52,199,89,0.5) !important; }
      .risk-low .material-symbols-rounded, .risk-low #risk_text { color: #34c759 !important; }
      .risk-medium { animation: glow-medium 1.5s infinite; border: 1px solid rgba(255,204,0,0.5) !important; }
      .risk-medium .material-symbols-rounded, .risk-medium #risk_text { color: #ffcc00 !important; }
      .risk-high { animation: glow-high 1s infinite; border: 1px solid rgba(255,59,48,0.5) !important; }
      .risk-high .material-symbols-rounded, .risk-high #risk_text { color: #ff3b30 !important; }
      
      .fg-bottombar { position: fixed; bottom: 32px; left: 0; right: 0; z-index: 999998 !important; display: flex; align-items: center; justify-content: center; gap: 14px; pointer-events: none; transition: transform 0.6s cubic-bezier(0.19, 1, 0.22, 1); }
      .fg-bottombar.panel-open, .fg-bottombar.nav-active { transform: translateY(150px); pointer-events: none; transition: transform 0.4s cubic-bezier(0.19, 1, 0.22, 1); }
      .fg-iconbtn, .fg-search-trigger { pointer-events: auto; background: var(--ios-glass-bg) !important; backdrop-filter: var(--ios-glass-blur); -webkit-backdrop-filter: var(--ios-glass-blur); border: var(--ios-glass-border); box-shadow: var(--ios-glass-shadow); transform: translateZ(0); transition: opacity 0.3s; }
      .fg-iconbtn { width: 46px; height: 46px; border-radius: 23px; display: flex; align-items: center; justify-content: center; border:none; outline: none; }
      .fg-search-trigger { width: 65%; max-width: 320px; height: 46px; border-radius: 23px; display: flex; justify-content: space-between; align-items: center; padding: 0 16px 0 20px; color: #666; font-size: 16px; font-weight: 500; cursor: pointer; }
      
      .view-capsule {
        position: fixed; right: 12px; bottom: 160px; z-index: 999997 !important;
        background: var(--ios-glass-bg) !important; backdrop-filter: var(--ios-glass-blur); -webkit-backdrop-filter: var(--ios-glass-blur);
        border: var(--ios-glass-border); box-shadow: var(--ios-glass-shadow);
        border-radius: 23px; display: flex; flex-direction: column; overflow: hidden;
        height: 46px; 
        transition: transform 0.6s cubic-bezier(0.19, 1, 0.22, 1), height 0.4s cubic-bezier(0.19, 1, 0.22, 1);
      }
      .view-capsule.panel-open { transform: translateX(100px); pointer-events: none; }
      .view-capsule.nav-active { height: 93px; } 
      
      .capsule-btn {
        width: 46px; height: 46px; flex-shrink: 0; border: none; background: transparent; outline: none;
        display: flex; align-items: center; justify-content: center; cursor: pointer;
        transition: background 0.2s ease;
      }
      .capsule-btn:active { background: rgba(0,0,0,0.1); }
      
      .toggle-view-btn { display: none !important; }
      .capsule-divider { width: 30px; height: 1px; flex-shrink: 0; background: rgba(0,0,0,0.15); margin: 0 auto; display: none !important; }

      .view-capsule.nav-active .toggle-view-btn { display: flex !important; }
      .view-capsule.nav-active .capsule-divider { display: block !important; }

      .nav-info-panel { position: fixed; bottom: 32px; left: 16px; right: 16px; max-width: 400px; margin: 0 auto; z-index: 999998 !important; background: var(--ios-glass-bg) !important; backdrop-filter: var(--ios-glass-blur); -webkit-backdrop-filter: var(--ios-glass-blur); border: var(--ios-glass-border); border-radius: 24px; box-shadow: var(--ios-glass-shadow); display: flex; justify-content: space-between; align-items: center; padding: 12px 20px; transform: translateY(150px); pointer-events: none; transition: transform 0.6s cubic-bezier(0.19, 1, 0.22, 1); }
      .nav-info-panel.active { transform: translateY(0); pointer-events: auto; }
      .nav-text-container { display: flex; align-items: baseline; gap: 8px; }
      .nav-time { font-size: 24px; font-weight: 800; color: #34c759; }
      .nav-dist { font-size: 16px; font-weight: 600; color: #8E8E93; }
      .end-nav-btn { background: #FF3B30 !important; color: #fff !important; border: none !important; border-radius: 16px !important; padding: 8px 20px !important; font-size: 16px !important; font-weight: 700 !important; cursor: pointer; outline: none !important; box-shadow: 0 4px 10px rgba(255,59,48,0.3) !important; transition: transform 0.15s ease, opacity 0.15s ease !important; }
      .end-nav-btn:active { transform: scale(0.92) !important; opacity: 0.8 !important; }
      .get-dir-btn { background: #007AFF !important; color: #fff !important; border: none !important; border-radius: 14px !important; font-size: 16px !important; font-weight: 700 !important; cursor: pointer; outline: none !important; box-shadow: 0 2px 5px rgba(0,122,255,0.3) !important; width: 100% !important; margin-top: 15px !important; height: 46px !important; transition: transform 0.15s ease, opacity 0.15s ease !important; }
      .get-dir-btn:active { transform: scale(0.95) !important; opacity: 0.85 !important; }
      .search-box-group { background: rgba(0,0,0,0.04); border-radius: 18px; padding: 6px; }
      .search-input-wrapper { position: relative; width: 100%; }
      .custom-search-input { background: #E5E5EA !important; border: none !important; height: 44px !important; padding: 0 40px 0 16px !important; font-size: 16px !important; color: #000 !important; width: 100% !important; box-sizing: border-box !important; outline: none !important; box-shadow: none !important; -webkit-appearance: none !important; appearance: none !important; transition: background 0.3s;}
      .custom-search-input:focus { background: #D1D1D6 !important; }
      .input-top { border-radius: 14px 14px 4px 4px !important; margin-bottom: 2px; }
      .input-bottom { border-radius: 4px 4px 14px 14px !important; }
      .search-icon-right { position: absolute; right: 10px; top: 50%; transform: translateY(-50%); color: #007AFF; font-size: 26px; pointer-events: auto; z-index: 10; cursor: pointer; padding: 5px; border-radius: 50%; }
      .suggestions-container { margin-top: 10px; background: rgba(255, 255, 255, 0.75); border-radius: 14px; max-height: 250px; overflow-y: auto; display: none; flex-direction: column; box-shadow: 0 4px 15px rgba(0,0,0,0.1); border: var(--ios-glass-border); }
      .suggestions-container.active { display: flex; }
      .suggestion-item { padding: 12px 16px; border-bottom: 0.5px solid rgba(0,0,0,0.1); display: flex; flex-direction: row; justify-content: space-between; align-items: center; cursor: pointer; }
      .suggestion-item:active { background: rgba(0,0,0,0.06); }
      .suggestion-item:last-child { border-bottom: none; }
      .suggestion-text { display: flex; flex-direction: column; flex: 1; padding-right: 12px; overflow: hidden; text-align: left !important; align-items: flex-start !important; }
      .sugg-title { font-size: 16px; font-weight: 600; color: #000; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 100%; }
      .sugg-sub { font-size: 12px; color: #8E8E93; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; margin-top: 2px; max-width: 100%; }
      .ios-map-label { background: transparent !important; border: none !important; box-shadow: none !important; font-size: 18px !important; font-weight: 800 !important; color: #1c1c1e !important; text-shadow: 0px 0px 6px rgba(255,255,255,0.9), 0px 0px 10px rgba(255,255,255,1), 0px 0px 3px rgba(255,255,255,1) !important; padding: 0 !important; }
      #global_overlay { position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; background: rgba(0, 0, 0, 0.05); backdrop-filter: blur(8px); z-index: 10000; opacity: 0; pointer-events: none; transition: opacity 0.4s ease; }
      #global_overlay.active { opacity: 1; pointer-events: auto; }
      .ios-toast { position: fixed; top: 60px; left: 50%; transform: translateX(-50%); background: rgba(0,0,0,0.8); color: white; padding: 12px 24px; border-radius: 20px; z-index: 100000; font-size: 14px; font-weight: 600; opacity: 0; pointer-events: none; transition: opacity 0.3s ease; white-space: nowrap; box-shadow: 0 4px 15px rgba(0,0,0,0.2); }
      .ios-toast.show { opacity: 1; }
      "))
  ),

  div(id = "fg-toast", class = "ios-toast", "Toast Message"),

  div(id = "fg-map", leafletOutput("map", height = "100%")),
  div(id = "global_overlay", onclick = "Shiny.setInputValue('close_all', Math.random())"),

  div(id = "view_capsule", class = "view-capsule",
    tags$button(
      id = "toggle_view_btn", class = "capsule-btn toggle-view-btn",
      onclick = "Shiny.setInputValue('trigger_toggle_view', Math.random())",
      tags$span(id = "toggle_view_icon", class = "material-symbols-rounded", style="color: #007AFF; font-size: 26px;", "explore")
    ),
    div(class = "capsule-divider"),
    tags$button(
      id = "my_loc_btn", class = "capsule-btn", 
      onclick = "requestLocation(); Shiny.setInputValue('trigger_loc_btn', Math.random())",
      tags$span(class = "material-symbols-rounded", style="color: #007AFF; font-size: 26px;", "my_location")
    )
  ),

  div(id = "risk_panel", class = "ios-panel",
    div(class = "settings-view",
      div(class = "panel-header", style = "margin-bottom: 12px;", tags$h2(class = "panel-title", "Alerts"), tags$button(id="c1", class="action-button panel-close-btn", "✕")),
      div(class = "ios-section-title", "LIVE MAP"),
      div(class = "ios-group", div(class = "ios-row no-click", div(class = "ios-row-title", "Risk Spot"), tags$label(class = "ios-switch", tags$input(id = "chk_risk_spot", type = "checkbox", onchange = "Shiny.setInputValue('show_heatmap', this.checked)"), tags$span(class = "ios-slider-toggle")))),
      div(class = "ios-section-title", "AI DRIVING ASSISTANT"),
      div(class = "ios-group", div(style = "padding: 20px; min-height: 80px; display: flex; flex-direction: column; justify-content: center; align-items: flex-start;", 
          div(id = "ai_advice_text", style = "color: #8E8E93; font-size: 15px; text-align: left; font-style: italic;", "Please turn on 'Risk Spot' or start navigation to view AI suggestions.")
      ))
    )
  ),

  div(id = "settings_panel", class = "ios-panel",
    div(id = "settings_main", class = "settings-view",
      div(class = "panel-header", style = "margin-bottom: 12px;", tags$h2(class = "panel-title", "Settings"), tags$button(id="c2", class="action-button panel-close-btn", "✕")),
      div(class = "ios-group", div(class = "ios-row", onclick = "showSubView('settings_sub_alerts')", div(class = "ios-row-title", "Alerts"), div(class = "ios-row-val", tags$span(id="val_alerts", "Sound, Vibrate"), tags$span(class="ios-chevron", "›")))),
      div(class = "ios-section-title", "DATA TRACEBACK DAYS"),
      div(class = "ios-group", style = "overflow: visible;", div(class = "slider-row", tags$span(class = "slider-label", "1"), div(class = "slider-container", div(id = "slider_tooltip", class = "slider-tooltip", "15"), tags$input(type = "range", class = "ios-slider", min = "1", max = "30", value = "15", oninput = "updateSliderColor(this)", onmouseup = "hideSliderTooltip()", ontouchend = "hideSliderTooltip()")), tags$span(class = "slider-label", "30"))),
      div(class = "ios-group", div(class = "ios-row no-click", div(class = "ios-row-title", "Show Traffic Stops"), tags$label(class = "ios-switch", tags$input(type = "checkbox", onchange = "Shiny.setInputValue('individual_case', this.checked)"), tags$span(class = "ios-slider-toggle")))),
      div(class = "ios-group", div(class = "ios-row", onclick = "showSubView('settings_sub_risk')", div(class = "ios-row-title", "Risk Tolerance"), div(class = "ios-row-val", tags$span(id="val_risk", "Moderate"), tags$span(class="ios-chevron", "›"))))
    ),
    div(id = "settings_sub_alerts", class = "settings-view settings-subview",
      div(class = "subview-header", tags$button(class = "back-btn", onclick = "hideSubViews()", "‹ Settings"), tags$h2(class = "subview-title", "Alerts")),
      div(class = "ios-group", div(class = "ios-row selected", `data-val` = "Sound", onclick = "toggleCheck(this, 'alerts')", div(class = "ios-row-title", "Sound"), div(class = "ios-checkmark", "✓")), div(class = "ios-row selected", `data-val` = "Vibrate", onclick = "toggleCheck(this, 'alerts')", div(class = "ios-row-title", "Vibrate"), div(class = "ios-checkmark", "✓")))
    ),
    div(id = "settings_sub_risk", class = "settings-view settings-subview",
      div(class = "subview-header", tags$button(class = "back-btn", onclick = "hideSubViews()", "‹ Settings"), tags$h2(class = "subview-title", "Risk Tolerance")),
      div(class = "ios-section-title", style = "text-transform: none; margin: 0 16px 16px 16px; font-size: 13.5px; line-height: 1.5;", "Choose your preferred routing behavior:", tags$br(), tags$br(), tags$b("Aggressive:"), " High risk tolerance.", tags$br(), tags$b("Moderate:"), " Medium risk tolerance.", tags$br(), tags$b("Conservative:"), " Low risk tolerance."),
      div(class = "ios-group", div(class = "ios-row", onclick = "selectRadio(this, 'risk', 'Aggressive')", div(class = "ios-row-title", "Aggressive"), div(class = "ios-checkmark", "✓")), div(class = "ios-row selected", onclick = "selectRadio(this, 'risk', 'Moderate')", div(class = "ios-row-title", "Moderate"), div(class = "ios-checkmark", "✓")), div(class = "ios-row", onclick = "selectRadio(this, 'risk', 'Conservative')", div(class = "ios-row-title", "Conservative"), div(class = "ios-checkmark", "✓")))
    )
  ),

  div(id = "search_panel", class = "ios-panel",
    div(class = "panel-header", style = "margin-bottom: 12px;", tags$h2(class = "panel-title", "Directions"), tags$button(id="c3", class="action-button panel-close-btn", "✕")),
    div(class = "search-box-group",
        div(class = "search-input-wrapper", tags$input(id = "start_input", type = "text", class = "custom-search-input input-top", value = "Current Location", placeholder = "Start Location", autocomplete = "off")),
        div(class = "search-input-wrapper", tags$input(id = "q_input", type = "text", class = "custom-search-input input-bottom", placeholder = "Destination", autocomplete = "off"), tags$span(class = "material-symbols-rounded search-icon-right", "search", onclick = "triggerCustomRoute()"))
    ),
    tags$button(class = "get-dir-btn", onclick = "triggerCustomRoute()", "Get Directions"),
    div(id = "search_suggestions", class = "suggestions-container")
  ),

  tags$button(id = "risk_btn", class = "action-button risk-indicator", 
    tags$span(class = "material-symbols-rounded", "siren_question"),
    tags$span(id = "risk_text", "Low")
  ),
  
  div(id = "nav_info_panel", class = "nav-info-panel",
      div(class = "nav-text-container", tags$span(id = "nav_time", class = "nav-time", "-- min"), tags$span(id = "nav_dist", class = "nav-dist", "-- mi")),
      tags$button(id = "end_nav_btn", class = "end-nav-btn", onclick = "Shiny.setInputValue('trigger_end_nav', Math.random())", "END")
  ),

  div(id = "bottom_tray", class = "fg-bottombar",
    div(id = "search_trigger", class = "action-button fg-search-trigger", tags$span("Search Map"), tags$span(class = "material-symbols-rounded", style = "color: #8E8E93; font-size: 24px;", "search")),
    tags$button(id = "settings_btn", class = "action-button fg-iconbtn", tags$span(class = "material-symbols-rounded", "settings"))
  )
)