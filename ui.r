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
    # 引入 Google Material Symbols (Rounded 版本)
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Material+Symbols+Rounded:opsz,wght,FILL,GRAD@24,400,0,0"),
    
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

      /* Google Material Icons 样式校准 */
      .material-symbols-rounded {
        font-variation-settings: 'FILL' 0, 'wght' 400, 'GRAD' 0, 'opsz' 24;
        font-size: 28px;
        color: #000;
        display: flex;
        align-items: center;
        justify-content: center;
      }

      /* --- 弹窗面板样式 --- */
      .ios-panel {
        position: fixed; z-index: 10001;
        background: var(--ios-glass-bg) !important;
        backdrop-filter: var(--ios-glass-blur);
        -webkit-backdrop-filter: var(--ios-glass-blur);
        border: var(--ios-glass-border);
        box-shadow: 0 25px 50px rgba(0,0,0,0.2);
        opacity: 0; pointer-events: none;
        transition: transform 0.5s cubic-bezier(0.19, 1, 0.22, 1), opacity 0.3s ease;
        padding: 26px; box-sizing: border-box; display: flex; flex-direction: column;
      }
      .ios-panel.active { opacity: 1; pointer-events: auto; }
      
      #risk_panel { top: 16px; left: 12px; right: 12px; height: 45vh; border-radius: 28px; transform-origin: 23px 23px; transform: scale(0); }
      #risk_panel.active { transform: scale(1); }
      #settings_panel { top: 85px; bottom: 12px; left: 12px; right: 12px; border-radius: 30px; transform: translateY(100%) scale(0.9); }
      #settings_panel.active { transform: translateY(0) scale(1); }
      #search_panel { bottom: 90px; left: 12px; right: 12px; height: 50vh; border-radius: 28px; transform-origin: center bottom; transform: scale(0.8) translateY(20px); }
      #search_panel.active { transform: scale(1) translateY(0); }

      /* --- 左上角警告图标 (Google Siren Icon) --- */
      .risk-indicator {
        position: fixed; top: 16px; left: 12px; width: 46px; height: 46px; 
        z-index: 10005; 
        background: var(--ios-glass-bg) !important; 
        backdrop-filter: var(--ios-glass-blur);
        -webkit-backdrop-filter: var(--ios-glass-blur);
        border: var(--ios-glass-border); 
        border-radius: 14px; 
        box-shadow: var(--ios-glass-shadow);
        display: flex; justify-content: center; align-items: center; 
        padding: 0; outline: none; border: none;
        cursor: pointer; 
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
      }
      .risk-indicator.panel-open { opacity: 0; transform: scale(0.5); pointer-events: none; }
      
      /* --- 底部托盘布局 --- */
      .fg-bottombar {
        position: fixed; bottom: 32px; left: 0; right: 0; z-index: 9999;
        display: flex; align-items: center; justify-content: center; gap: 14px; pointer-events: none;
        transition: transform 0.4s ease, opacity 0.4s ease;
      }
      .fg-bottombar.panel-open { opacity: 0; transform: translateY(60px); }

      .fg-iconbtn, .fg-search-trigger { 
        pointer-events: auto; background: var(--ios-glass-bg) !important; 
        backdrop-filter: var(--ios-glass-blur); border: var(--ios-glass-border); box-shadow: var(--ios-glass-shadow); 
      }
      .fg-iconbtn { width: 46px; height: 46px; border-radius: 23px; display: flex; align-items: center; justify-content: center; border:none;}
      .fg-search-trigger { width: 65%; max-width: 320px; height: 46px; border-radius: 23px; display: flex; align-items: center; padding: 0 18px; color: #666; font-size: 16px; font-weight: 500; cursor: pointer; }

      #global_overlay {
        position: fixed; top: 0; left: 0; width: 100vw; height: 100vh;
        background: rgba(0, 0, 0, 0.05); backdrop-filter: blur(8px);
        z-index: 10000; opacity: 0; pointer-events: none; transition: opacity 0.4s ease;
      }
      #global_overlay.active { opacity: 1; pointer-events: auto; }

      .panel-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 22px; }
      .panel-title { font-size: 24px; font-weight: 800; color: #000; margin: 0; }
    "))
  ),

  # 1. 地图层
  div(id = "fg-map", leafletOutput("map", height = "100%")),

  # 2. 全局遮罩
  div(id = "global_overlay", onclick = "Shiny.setInputValue('close_all', Math.random())"),

  # 3. 警告面板
  div(id = "risk_panel", class = "ios-panel",
    div(class = "panel-header",
      tags$h2(class = "panel-title", "Alerts"),
      tags$button(id="c1", class="action-button", style="background:rgba(0,0,0,0.06); border:none; width:30px; height:30px; border-radius:15px; color:#555;", "✕")
    ),
    f7Toggle(inputId = "t1", label = "Enable Sound", checked = TRUE),
    f7Toggle(inputId = "t2", label = "Haptic Vibration", checked = TRUE)
  ),

  # 4. 设置面板
  div(id = "settings_panel", class = "ios-panel",
    div(class = "panel-header",
      tags$h2(class = "panel-title", "Settings"),
      tags$button(id="c2", class="action-button", style="background:rgba(0,0,0,0.06); border:none; width:30px; height:30px; border-radius:15px; color:#555;", "✕")
    ),
    f7Segment(container = "segment",
      f7Button(label = "Safe", color = "green"),
      f7Button(label = "Normal", color = "blue", active = TRUE),
      f7Button(label = "Alert", color = "red")
    )
  ),

  # 5. 搜索面板
  div(id = "search_panel", class = "ios-panel",
    div(class = "panel-header",
      tags$h2(class = "panel-title", "Search"),
      tags$button(id="c3", class="action-button", style="background:rgba(0,0,0,0.06); border:none; width:30px; height:30px; border-radius:15px; color:#555;", "✕")
    ),
    div(style="background:rgba(0,0,0,0.05); border-radius:15px; padding:10px 15px;",
        textInput("q_input", label = NULL, placeholder = "Search for a destination...")
    )
  ),

  # --- UI 触发组件 (已更新图标) ---
  
  # 顶部警告按钮 (使用 Google Icon: siren)
  tags$button(
    id = "risk_btn", 
    class = "action-button risk-indicator",
    tags$span(class = "material-symbols-rounded", "siren_question")
  ),

  div(id = "bottom_tray", class = "fg-bottombar",
    # 1. 搜索触发器 (现在位于左侧)
    div(id = "search_trigger", class = "action-button fg-search-trigger", "Search Map"),
    
    # 2. 设置按钮 (现在位于右侧，使用 Google Icon: settings)
    tags$button(
      id = "settings_btn", 
      class = "action-button fg-iconbtn", 
      tags$span(class = "material-symbols-rounded", "settings")
    )
  )
)