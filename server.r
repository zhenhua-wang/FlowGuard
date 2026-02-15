library(shiny)
library(shinyMobile)
library(leaflet)
library(leaflet.extras)
library(shinyjs)
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      setView(lng = -92.3341, lat = 38.9517, zoom = 13)
  })

  observeEvent(input$risk_btn, {
    shinyjs::addClass("risk_panel", "active")
    shinyjs::addClass("global_overlay", "active")
    shinyjs::addClass("risk_btn", "panel-open")
    
    # 【改动】：打开警告页面时，同样隐藏底部导航条 (包含 Search 和 Settings)
    shinyjs::addClass("bottom_tray", "panel-open") 
  })

  observeEvent(input$settings_btn, {
    shinyjs::addClass("settings_panel", "active")
    shinyjs::addClass("global_overlay", "active")
    shinyjs::addClass("bottom_tray", "panel-open")
    shinyjs::addClass("risk_btn", "panel-open")
  })

  observeEvent(input$search_trigger, {
    shinyjs::addClass("search_panel", "active")
    shinyjs::addClass("global_overlay", "active")
    shinyjs::addClass("bottom_tray", "panel-open")
    shinyjs::addClass("risk_btn", "panel-open")
  })

  observeEvent(c(input$close_all, input$c1, input$c2, input$c3), {
    shinyjs::removeClass("risk_panel", "active")
    shinyjs::removeClass("settings_panel", "active")
    shinyjs::removeClass("search_panel", "active")
    shinyjs::removeClass("global_overlay", "active")
    shinyjs::removeClass("risk_btn", "panel-open")
    shinyjs::removeClass("bottom_tray", "panel-open")
    
    shinyjs::runjs("setTimeout(hideSubViews, 300);")
  }, ignoreInit = TRUE)
  
  observeEvent(input$show_heatmap, {
    print(paste("Heatmap toggle status:", input$show_heatmap))
  })
}
