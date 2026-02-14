library(shiny)
library(shinyMobile)
library(leaflet)

ui <- f7Page(
  title = "FlowGuard",
  options = list(theme = "auto", dark = FALSE, filled = FALSE),

  tags$head(
    tags$style(HTML("
      /* Fullscreen background map */
      #fg-map {
        position: fixed;
        top: 0;
        bottom: 0;
        left: 0;
        right: 0;
        z-index: 1;
      }

      /* Bottom tray (search + settings) */
      .fg-bottombar {
        position: fixed;
        bottom: 16px;            /* move to bottom */
        left: 12px;
        right: 12px;
        z-index: 9999;
        display: flex;
        align-items: center;
        gap: 10px;
        max-width: 720px;
        margin: 0 auto;
        pointer-events: none;    /* allow map gestures except on controls */
      }
      .fg-bottombar > * { pointer-events: auto; }

      /* Round icon button */
      .fg-iconbtn {
        width: 44px;
        height: 44px;
        border-radius: 22px;
        display: flex;
        align-items: center;
        justify-content: center;
        background: rgba(255,255,255,0.95);
        backdrop-filter: blur(12px);
        box-shadow: 0 10px 28px rgba(0,0,0,0.20);
        border: 0;
        padding: 0;
      }

      /* Search bar */
      .fg-search {
        flex: 1 1 auto;
        height: 44px;
        border-radius: 22px;
        background: rgba(255,255,255,0.95);
        backdrop-filter: blur(12px);
        box-shadow: 0 10px 28px rgba(0,0,0,0.20);
        display: flex;
        align-items: center;
        padding: 0 12px;
      }
      .fg-search .shiny-input-container {
        margin: 0 !important;
        width: 100%;
      }
      .fg-search input {
        border: none !important;
        outline: none !important;
        background: transparent !important;
        height: 34px !important;
        font-size: 15px !important;
      }
      .fg-search label { display: none !important; }
    "))
  ),

  # Background map
  div(
    id = "fg-map",
    leafletOutput("map", height = "100%")
  ),

  # Bottom tray: settings button + search bar
  div(
    class = "fg-bottombar",

    actionButton(
      inputId = "btn_settings",
      label = NULL,
      class = "fg-iconbtn",
      icon = icon("gear")
    ),

    div(
      class = "fg-search",
      textInput("q", label = NULL, placeholder = "Search Maps")
    )
  ),

  # Optional overlay content controlled by server
  uiOutput("panel_ui")
)
