library(shiny)
library(shinyMobile)
library(leaflet)
library(leaflet.extras)
library(shinyjs)
ui <- f7Page(
  title = "FlowGuard",
  options = list(theme = "auto", dark = FALSE, filled = FALSE),
  
  useShinyjs(),

  tags$head(
    # 引入 Google Material Symbols
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Material+Symbols+Rounded:opsz,wght,FILL,GRAD@24,400,0,0"),
    
    # --- 自定义交互 JS ---
    tags$script(HTML("
      function showSubView(id) {
        $('#settings_main').addClass('hidden-left');
        $('#' + id).addClass('active');
      }
      function hideSubViews() {
        $('#settings_main').removeClass('hidden-left');
        $('.settings-subview').removeClass('active');
      }
      function selectRadio(el, groupName, val) {
        $(el).siblings().removeClass('selected');
        $(el).addClass('selected');
        Shiny.setInputValue(groupName, val);
        $('#val_' + groupName).text(val);
      }
      function toggleCheck(el, groupName) {
        $(el).toggleClass('selected');
        let vals = [];
        $(el).parent().children('.selected').each(function() {
          vals.push($(this).attr('data-val'));
        });
        Shiny.setInputValue(groupName, vals);
        $('#val_' + groupName).text(vals.length > 0 ? vals.join(', ') : 'None');
      }
      // 动态更新滑块颜色与浮动提示气泡
      function updateSliderColor(el) {
        const min = parseFloat(el.min);
        const max = parseFloat(el.max);
        const val = (el.value - min) / (max - min) * 100;
        
        el.style.setProperty('--value', val + '%');
        
        const tooltip = document.getElementById('slider_tooltip');
        tooltip.innerText = el.value;
        tooltip.classList.add('active');
        
        const offset = (50 - val) / 50 * 13; 
        tooltip.style.left = 'calc(' + val + '% + ' + offset + 'px)';
        
        Shiny.setInputValue('traffic_days', el.value);
      }
      function hideSliderTooltip() {
        document.getElementById('slider_tooltip').classList.remove('active');
      }
    ")),

    tags$style(HTML("
      /* 基础页面设置 */
      body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; overflow: hidden; }
      #fg-map { position: fixed; top: 0; bottom: 0; left: 0; right: 0; z-index: 1; }

      /* --- 统一材质变量 --- */
      :root {
        --ios-glass-bg: rgba(255, 255, 255, 0.45);
        --ios-glass-blur: blur(35px) saturate(190%);
        --ios-glass-border: 1px solid rgba(255, 255, 255, 0.5);
        --ios-glass-shadow: 0 10px 30px rgba(0,0,0,0.15);
      }

      .material-symbols-rounded {
        font-variation-settings: 'FILL' 0, 'wght' 400, 'GRAD' 0, 'opsz' 24;
        font-size: 28px; color: #000;
        display: flex; align-items: center; justify-content: center;
      }

      /* --- 弹窗面板基础 --- */
      .ios-panel {
        position: fixed; z-index: 10001;
        background: var(--ios-glass-bg) !important;
        backdrop-filter: var(--ios-glass-blur); -webkit-backdrop-filter: var(--ios-glass-blur);
        border: var(--ios-glass-border); box-shadow: 0 25px 50px rgba(0,0,0,0.2);
        opacity: 0; pointer-events: none; 
        transition: transform 0.4s cubic-bezier(0.19, 1, 0.22, 1), opacity 0.4s ease;
        padding: 26px; box-sizing: border-box; display: flex; flex-direction: column;
        backface-visibility: hidden;
      }
      .ios-panel.active { opacity: 1; pointer-events: auto; }
      
      .panel-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 22px; }
      .panel-title { font-size: 24px; font-weight: 800; color: #000; margin: 0; }
      .panel-close-btn {
        background: rgba(0,0,0,0.06) !important; border: none; width: 30px; height: 30px; border-radius: 50%;
        color: #555; font-size: 22px; display: flex; align-items: center; justify-content: center;
        cursor: pointer; transform: translateZ(0); -webkit-tap-highlight-color: transparent; outline: none !important;
      }
      .panel-close-btn:active { background: rgba(0,0,0,0.15) !important; }

      /* 面板位置定义 */
      #risk_panel { top: 16px; left: 12px; right: 12px; height: 45vh; border-radius: 28px; transform-origin: 23px 23px; transform: scale(0); padding: 0; overflow: hidden;}
      #risk_panel.active { transform: scale(1); }
      #settings_panel { top: 85px; bottom: 12px; left: 12px; right: 12px; border-radius: 30px; transform: translateY(100%) scale(0.9); padding: 0; overflow: hidden; }
      #settings_panel.active { transform: translateY(0) scale(1); }
      #search_panel { bottom: 90px; left: 12px; right: 12px; height: 50vh; border-radius: 28px; transform-origin: center bottom; transform: scale(0.8) translateY(20px); }
      #search_panel.active { transform: scale(1) translateY(0); }

      /* ==================================================== */
      /* iOS SwiftUI 共享样式容器                            */
      /* ==================================================== */
      .settings-view {
        position: absolute; top: 0; left: 0; right: 0; bottom: 0;
        padding: 26px 16px; box-sizing: border-box; overflow-y: auto; background: transparent;
        transition: transform 0.4s cubic-bezier(0.36, 0.66, 0.04, 1), opacity 0.4s ease;
      }
      
      #settings_main { transform: translateX(0); opacity: 1; }
      #settings_main.hidden-left { transform: translateX(-30%); opacity: 0; pointer-events: none; }
      .settings-subview { transform: translateX(100%); opacity: 0; pointer-events: none; }
      .settings-subview.active { transform: translateX(0); opacity: 1; pointer-events: auto; }

      .subview-header { display: flex; align-items: center; justify-content: space-between; margin-bottom: 24px; position: relative; }
      .subview-header .back-btn { background: none; border: none; color: #007AFF; font-size: 17px; display: flex; align-items: center; padding: 0; cursor: pointer; }
      .subview-header .subview-title { font-size: 17px; font-weight: 600; color: #000; margin: 0; position: absolute; left: 50%; transform: translateX(-50%); pointer-events: none; }

      .ios-section-title { font-size: 13px; color: #6D6D72; margin: 16px 0 8px 16px; text-transform: uppercase; }
      .ios-group { background: rgba(255, 255, 255, 0.65); border-radius: 12px; margin-bottom: 24px; overflow: hidden; box-shadow: 0 1px 4px rgba(0,0,0,0.05); }
      
      .ios-row {
        display: flex; justify-content: space-between; align-items: center; padding: 12px 16px; min-height: 44px;
        border-bottom: 0.5px solid rgba(0,0,0,0.1); cursor: pointer; position: relative;
      }
      .ios-row:last-child { border-bottom: none; }
      .ios-row:active { background: rgba(0,0,0,0.06); }
      .ios-row.no-click { cursor: default; }
      .ios-row.no-click:active { background: transparent; }

      .ios-row-content { display: flex; flex-direction: column; flex-grow: 1; padding-right: 15px; }
      .ios-row-title { font-size: 17px; color: #000; }
      .ios-row-subtitle { font-size: 13px; color: #8E8E93; margin-top: 4px; line-height: 1.3; }
      
      .ios-row-val { font-size: 17px; color: #8E8E93; display: flex; align-items: center; gap: 8px; }
      .ios-chevron { color: #C7C7CC; font-size: 20px; font-weight: bold; margin-bottom: 2px;}
      
      .ios-checkmark { color: #007AFF; font-size: 20px; font-weight: bold; display: none; }
      .ios-row.selected .ios-checkmark { display: block; }

      /* --- 自定义滑块容器与动态气泡 --- */
      .slider-row { padding: 14px 16px; display: flex; align-items: center; gap: 14px; }
      .slider-label { font-size: 15px; color: #8E8E93; font-weight: 500; }
      .slider-container { position: relative; flex-grow: 1; display: flex; align-items: center; }
      
      .slider-tooltip {
        position: absolute; top: -28px; transform: translateX(-50%);
        background: rgba(0, 0, 0, 0.8); color: #fff; padding: 4px 10px; border-radius: 8px;
        font-size: 14px; font-weight: 600; opacity: 0; pointer-events: none;
        transition: opacity 0.2s ease; white-space: nowrap; z-index: 10;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      .slider-tooltip.active { opacity: 1; }
      .slider-tooltip::after {
        content: ''; position: absolute; bottom: -4px; left: 50%; transform: translateX(-50%);
        border-width: 4px 5px 0; border-style: solid; border-color: rgba(0, 0, 0, 0.8) transparent transparent transparent;
      }

      input[type=range].ios-slider { 
        width: 100%; -webkit-appearance: none; background: transparent; height: 30px; 
        --value: 48.27%; 
      }
      input[type=range].ios-slider::-webkit-slider-thumb {
        -webkit-appearance: none; height: 26px; width: 26px; border-radius: 50%;
        background: #fff; box-shadow: 0 1px 5px rgba(0,0,0,0.3); cursor: pointer; margin-top: -11px;
      }
      input[type=range].ios-slider::-webkit-slider-runnable-track { 
        width: 100%; height: 4px; border-radius: 2px;
        background: linear-gradient(to right, #34c759 0%, #34c759 var(--value), #D1D1D6 var(--value), #D1D1D6 100%); 
      }

      /* 自定义 iOS Switch */
      .ios-switch { position: relative; display: inline-block; width: 51px; height: 31px; }
      .ios-switch input { opacity: 0; width: 0; height: 0; }
      .ios-slider-toggle { position: absolute; cursor: pointer; top: 0; left: 0; right: 0; bottom: 0; background-color: #e9e9ea; transition: .4s; border-radius: 31px; }
      .ios-slider-toggle:before { position: absolute; content: ''; height: 27px; width: 27px; left: 2px; bottom: 2px; background-color: white; transition: .4s; border-radius: 50%; box-shadow: 0 2px 4px rgba(0,0,0,0.2); }
      .ios-switch input:checked + .ios-slider-toggle { background-color: #34c759; }
      .ios-switch input:checked + .ios-slider-toggle:before { transform: translateX(20px); }

      /* ==================================================== */
      /* 主界面悬浮按钮、搜索条、与物理位移动画                  */
      /* ==================================================== */
      
      .risk-indicator {
        position: fixed; top: 16px; left: 12px; width: 46px; height: 46px; z-index: 999999 !important; 
        background: var(--ios-glass-bg) !important; 
        backdrop-filter: var(--ios-glass-blur); -webkit-backdrop-filter: var(--ios-glass-blur);
        border: var(--ios-glass-border); border-radius: 14px; box-shadow: var(--ios-glass-shadow);
        display: flex; justify-content: center; align-items: center; padding: 0; outline: none; border: none; cursor: pointer; transform: translateZ(0); 
        transition: transform 0.6s cubic-bezier(0.19, 1, 0.22, 1);
      }
      .risk-indicator.panel-open { 
        transform: translateY(-100px) scale(0.8); pointer-events: none; 
        transition: transform 0.4s cubic-bezier(0.19, 1, 0.22, 1);
      }
      
      .fg-bottombar {
        position: fixed; bottom: 32px; left: 0; right: 0; z-index: 999998 !important; 
        display: flex; align-items: center; justify-content: center; gap: 14px; pointer-events: none; 
        transition: transform 0.6s cubic-bezier(0.19, 1, 0.22, 1);
      }
      .fg-bottombar.panel-open { 
        transform: translateY(150px); pointer-events: none;
        transition: transform 0.4s cubic-bezier(0.19, 1, 0.22, 1);
      }

      .fg-iconbtn, .fg-search-trigger { 
        pointer-events: auto; background: var(--ios-glass-bg) !important; 
        backdrop-filter: var(--ios-glass-blur); -webkit-backdrop-filter: var(--ios-glass-blur); 
        border: var(--ios-glass-border); box-shadow: var(--ios-glass-shadow); transform: translateZ(0); 
      }
      .fg-iconbtn { width: 46px; height: 46px; border-radius: 23px; display: flex; align-items: center; justify-content: center; border:none; outline: none;}
      
      /* 修改：将搜索触发器变为两端对齐，为放大镜留出空间 */
      .fg-search-trigger { 
        width: 65%; max-width: 320px; height: 46px; border-radius: 23px; display: flex; 
        justify-content: space-between; align-items: center; padding: 0 16px 0 20px; 
        color: #666; font-size: 16px; font-weight: 500; cursor: pointer; 
      }

      /* 搜索面板输入框自定义样式 */
      .search-input-wrapper { position: relative; width: 100%; margin-top: 5px; }
      .search-input-wrapper .form-group { margin-bottom: 0; width: 100%; }
      .search-input-wrapper input {
        background: rgba(0,0,0,0.05); border: none; border-radius: 14px;
        height: 44px; padding: 0 40px 0 16px; font-size: 16px; color: #000; width: 100%;
        box-sizing: border-box; outline: none; box-shadow: none; transition: background 0.3s;
      }
      .search-input-wrapper input:focus { background: rgba(0,0,0,0.08); }
      .search-icon-right {
        position: absolute; right: 14px; top: 50%; transform: translateY(-50%);
        color: #8E8E93; font-size: 24px; pointer-events: none; z-index: 10;
      }

      #global_overlay {
        position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; background: rgba(0, 0, 0, 0.05); backdrop-filter: blur(8px);
        z-index: 10000; opacity: 0; pointer-events: none; transition: opacity 0.4s ease;
      }
      #global_overlay.active { opacity: 1; pointer-events: auto; }
    "))
  ),

  # 1. 地图层
  div(id = "fg-map", leafletOutput("map", height = "100%")),

  # 2. 全局遮罩
  div(id = "global_overlay", onclick = "Shiny.setInputValue('close_all', Math.random())"),

  # 3. 警告面板
  div(id = "risk_panel", class = "ios-panel",
    div(class = "settings-view",
      div(class = "panel-header", style = "margin-bottom: 12px;",
        tags$h2(class = "panel-title", "Alerts"),
        tags$button(id="c1", class="action-button panel-close-btn", "✕")
      ),
      
      div(class = "ios-section-title", "LIVE MAP"),
      div(class = "ios-group",
        div(class = "ios-row no-click",
          div(class = "ios-row-title", "Traffic Stop Heatmap"),
          tags$label(class = "ios-switch",
            tags$input(type = "checkbox", onchange = "Shiny.setInputValue('show_heatmap', this.checked)"),
            tags$span(class = "ios-slider-toggle")
          )
        )
      ),
      
      div(class = "ios-section-title", "AI DRIVING ASSISTANT"),
      div(class = "ios-group",
        div(style = "padding: 20px; min-height: 80px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
          div(style = "color: #8E8E93; font-size: 15px; text-align: center; font-style: italic;", 
              "AI Speed Recommendation will appear here...")
        )
      )
    )
  ),

  # 4. 设置面板 
  div(id = "settings_panel", class = "ios-panel",
    
    # --- 主视图 ---
    div(id = "settings_main", class = "settings-view",
      div(class = "panel-header", style = "margin-bottom: 12px;",
        tags$h2(class = "panel-title", "Settings"),
        tags$button(id="c2", class="action-button panel-close-btn", "✕")
      ),
      
      div(class = "ios-group",
        div(class = "ios-row", onclick = "showSubView('settings_sub_alerts')",
          div(class = "ios-row-title", "Alerts"),
          div(class = "ios-row-val", tags$span(id="val_alerts", "Sound, Vibrate"), tags$span(class="ios-chevron", "›"))
        )
      ),
      
      div(class = "ios-section-title", "DATA TRACEBACK DAYS"),
      div(class = "ios-group", style = "overflow: visible;",
        div(class = "slider-row",
          tags$span(class = "slider-label", "1"),
          div(class = "slider-container",
              div(id = "slider_tooltip", class = "slider-tooltip", "15"), 
              tags$input(type = "range", class = "ios-slider", min = "1", max = "30", value = "15", 
                         oninput = "updateSliderColor(this)", 
                         onmouseup = "hideSliderTooltip()", 
                         ontouchend = "hideSliderTooltip()")
          ),
          tags$span(class = "slider-label", "30")
        )
      ),
      
      div(class = "ios-group",
        div(class = "ios-row no-click",
          div(class = "ios-row-title", "Show Traffic Stops"),
          tags$label(class = "ios-switch",
            tags$input(type = "checkbox", onchange = "Shiny.setInputValue('individual_case', this.checked)"),
            tags$span(class = "ios-slider-toggle")
          )
        )
      ),
      
      div(class = "ios-group",
        div(class = "ios-row", onclick = "showSubView('settings_sub_risk')",
          div(class = "ios-row-title", "Risk Tolerance"),
          div(class = "ios-row-val", tags$span(id="val_risk", "Moderate"), tags$span(class="ios-chevron", "›"))
        )
      )
    ),

    # --- 子视图 1: Alerts ---
    div(id = "settings_sub_alerts", class = "settings-view settings-subview",
      div(class = "subview-header",
        tags$button(class = "back-btn", onclick = "hideSubViews()", "‹ Settings"),
        tags$h2(class = "subview-title", "Alerts")
      ),
      div(class = "ios-group",
        div(class = "ios-row selected", `data-val` = "Sound", onclick = "toggleCheck(this, 'alerts')",
          div(class = "ios-row-title", "Sound"), div(class = "ios-checkmark", "✓")
        ),
        div(class = "ios-row selected", `data-val` = "Vibrate", onclick = "toggleCheck(this, 'alerts')",
          div(class = "ios-row-title", "Vibrate"), div(class = "ios-checkmark", "✓")
        )
      )
    ),

    # --- 子视图 2: Risk Tolerance ---
    div(id = "settings_sub_risk", class = "settings-view settings-subview",
      div(class = "subview-header",
        tags$button(class = "back-btn", onclick = "hideSubViews()", "‹ Settings"),
        tags$h2(class = "subview-title", "Risk Tolerance")
      ),
      
      div(class = "ios-section-title", style = "text-transform: none; margin: 0 16px 16px 16px; font-size: 13.5px; line-height: 1.5;",
        "Choose your preferred routing behavior:", tags$br(), tags$br(),
        tags$b("Aggressive:"), " High risk tolerance, willing to accept a higher risk of traffic stops.", tags$br(),
        tags$b("Moderate:"), " Medium risk tolerance, balances routing efficiency with safety.", tags$br(),
        tags$b("Conservative:"), " Low risk tolerance, strictly prefers safe routes over speed."
      ),
      
      div(class = "ios-group",
        div(class = "ios-row", onclick = "selectRadio(this, 'risk', 'Aggressive')",
          div(class = "ios-row-title", "Aggressive"), div(class = "ios-checkmark", "✓")
        ),
        div(class = "ios-row selected", onclick = "selectRadio(this, 'risk', 'Moderate')",
          div(class = "ios-row-title", "Moderate"), div(class = "ios-checkmark", "✓")
        ),
        div(class = "ios-row", onclick = "selectRadio(this, 'risk', 'Conservative')",
          div(class = "ios-row-title", "Conservative"), div(class = "ios-checkmark", "✓")
        )
      )
    )
  ),

  # 5. 搜索面板 (增加内部放大镜图标)
  div(id = "search_panel", class = "ios-panel",
    div(class = "panel-header", style = "margin-bottom: 12px;",
      tags$h2(class = "panel-title", "Search"),
      tags$button(id="c3", class="action-button panel-close-btn", "✕")
    ),
    div(class = "search-input-wrapper",
        textInput("q_input", label = NULL, placeholder = "Search Map...", width = "100%"),
        tags$span(class = "material-symbols-rounded search-icon-right", "search")
    )
  ),

  # --- UI 触发组件 ---
  tags$button(
    id = "risk_btn", class = "action-button risk-indicator",
    tags$span(class = "material-symbols-rounded", "siren_question")
  ),

  div(id = "bottom_tray", class = "fg-bottombar",
    # 底部触发器增加放大镜图标
    div(id = "search_trigger", class = "action-button fg-search-trigger", 
        tags$span("Search Map"),
        tags$span(class = "material-symbols-rounded", style = "color: #8E8E93; font-size: 24px;", "search")
    ),
    tags$button(
      id = "settings_btn", class = "action-button fg-iconbtn", 
      tags$span(class = "material-symbols-rounded", "settings")
    )
  )
)
