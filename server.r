library(shiny)
library(shinyMobile)
library(leaflet)
library(leaflet.extras)
library(shinyjs)

# ==========================================
# 2. 后端 Server 逻辑 
# ==========================================
geocode_osm <- function(query) {
  safe_query <- URLencode(query)
  url <- paste0("https://nominatim.openstreetmap.org/search?q=", safe_query, "&format=json&limit=1&viewbox=-93.0,39.5,-91.5,38.0")
  req <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  if (!is.null(req) && length(req) > 0 && is.data.frame(req) && nrow(req) > 0) {
    return(c(lon = as.numeric(req$lon[1]), lat = as.numeric(req$lat[1])))
  }
  return(NULL)
}

get_osrm_route <- function(start_pt, end_pt) {
  url <- sprintf("https://router.project-osrm.org/route/v1/driving/%f,%f;%f,%f?overview=full&geometries=geojson",
                 start_pt["lon"], start_pt["lat"], end_pt["lon"], end_pt["lat"])
  req <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  if (!is.null(req) && !is.null(req$code) && req$code == "Ok") {
    coords <- req$routes$geometry$coordinates[[1]]
    dist_meters <- req$routes$distance[1]
    time_seconds <- req$routes$duration[1]
    if(is.matrix(coords) || is.data.frame(coords)){ 
      return(list(coords = coords, dist = dist_meters, time = time_seconds)) 
    }
  }
  return(NULL)
}

server <- function(input, output, session) {
  current_location <- c(lon = -92.3341, lat = 38.9517)
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      setView(lng = current_location["lon"], lat = current_location["lat"], zoom = 14) %>%
      addCircleMarkers(lng = current_location["lon"], lat = current_location["lat"], 
                       radius = 10, color = "#fff", weight = 3, fillColor = "#007AFF", 
                       fillOpacity = 1, layerId = "current_loc")
  })

  # 【核心修改区：监听点击 -> 移动地图标记 -> 弹窗等待用户确认】
  observeEvent(input$map_click, {
    click <- input$map_click
    req(click)
    
    # 获取点击位置的地名
    url <- sprintf("https://nominatim.openstreetmap.org/reverse?format=json&lat=%f&lon=%f", click$lat, click$lng)
    req_data <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
    
    dest_name <- "Selected Location"
    if (!is.null(req_data) && !is.null(req_data$display_name)) {
      dest_name <- trimws(strsplit(req_data$display_name, ",")[[1]][1])
      dest_name <- gsub("'", "", dest_name)
    }
    
    # 移动地图并打上红色的目的地 Pin
    leafletProxy("map") %>% 
      clearGroup("search_res") %>% 
      setView(lng = click$lng, lat = click$lat, zoom = 15) %>%
      addCircleMarkers(
        lng = click$lng, lat = click$lat, group = "search_res",
        radius = 10, color = "#fff", weight = 3, fillColor = "#FF3B30", fillOpacity = 1,
        label = dest_name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, className = "ios-map-label", direction = "top", offset = c(0, -10))
      )
    
    # 执行 JS：填入目的地输入框，藏入精确坐标，并打开含有 Directions 按钮的面板
    js_code <- sprintf("
      $('#q_input').val('%s');
      $('#q_input').attr('data-clicked-lat', %f);
      $('#q_input').attr('data-clicked-lon', %f);
      
      // 滑出 Search / Directions 面板
      $('#search_panel').addClass('active');
      $('#global_overlay').addClass('active');
      $('#bottom_tray').addClass('panel-open');
      $('#risk_btn').addClass('panel-open');
      $('.loc-btn').addClass('panel-open');
    ", dest_name, click$lat, click$lng)
    
    shinyjs::runjs(js_code)
  })

  observeEvent(input$risk_btn, { shinyjs::addClass("risk_panel", "active"); shinyjs::addClass("global_overlay", "active"); shinyjs::addClass("risk_btn", "panel-open"); shinyjs::addClass("bottom_tray", "panel-open"); shinyjs::addClass("my_loc_btn", "panel-open") })
  observeEvent(input$settings_btn, { shinyjs::addClass("settings_panel", "active"); shinyjs::addClass("global_overlay", "active"); shinyjs::addClass("bottom_tray", "panel-open"); shinyjs::addClass("risk_btn", "panel-open"); shinyjs::addClass("my_loc_btn", "panel-open") })
  observeEvent(input$search_trigger, { shinyjs::addClass("search_panel", "active"); shinyjs::addClass("global_overlay", "active"); shinyjs::addClass("bottom_tray", "panel-open"); shinyjs::addClass("risk_btn", "panel-open"); shinyjs::addClass("my_loc_btn", "panel-open") })
  observeEvent(c(input$close_all, input$c1, input$c2, input$c3), {
    shinyjs::removeClass("risk_panel", "active"); shinyjs::removeClass("settings_panel", "active"); shinyjs::removeClass("search_panel", "active"); shinyjs::removeClass("global_overlay", "active"); shinyjs::removeClass("risk_btn", "panel-open"); shinyjs::removeClass("bottom_tray", "panel-open"); shinyjs::removeClass("my_loc_btn", "panel-open")
    shinyjs::runjs("setTimeout(hideSubViews, 300);")
  }, ignoreInit = TRUE)
  
  observeEvent(input$trigger_loc_btn, { leafletProxy("map") %>% setView(lng = current_location["lon"], lat = current_location["lat"], zoom = 15) })
  
  observeEvent(input$trigger_end_nav, {
    leafletProxy("map") %>% clearGroup("search_res") %>% setView(lng = current_location["lon"], lat = current_location["lat"], zoom = 14)
    session$sendCustomMessage("end_nav_ui", list())
  })
  
  observeEvent(input$fly_to_loc, {
    req(input$fly_to_loc)
    shinyjs::runjs("Shiny.setInputValue('close_all', Math.random());") 
    
    lon <- as.numeric(input$fly_to_loc$lon)
    lat <- as.numeric(input$fly_to_loc$lat)
    name <- input$fly_to_loc$name
    is_dest <- input$fly_to_loc$type == "q_input"
    fill_color <- ifelse(is_dest, "#FF3B30", "#007AFF") 
    
    leafletProxy("map") %>% clearGroup("search_res") %>%
      addCircleMarkers(
        lng = lon, lat = lat, group = "search_res",
        radius = 10, color = "#fff", weight = 3, fillColor = fill_color, fillOpacity = 1,
        label = name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, className = "ios-map-label", direction = "top", offset = c(0, -10))
      ) %>% setView(lng = lon, lat = lat, zoom = 15)
  })
  
  observeEvent(input$do_custom_route, {
    req(input$do_custom_route)
    start_str <- trimws(input$do_custom_route$start)
    end_str <- trimws(input$do_custom_route$end)
    
    shinyjs::runjs("Shiny.setInputValue('close_all', Math.random());") 
    
    start_loc <- NULL
    if (tolower(start_str) %in% c("current location", "my location", "")) {
      start_loc <- current_location; start_name <- "Current Location"
    } else {
      start_loc <- geocode_osm(start_str); start_name <- start_str
      if (is.null(start_loc)) { start_loc <- current_location; start_name <- "Current Location" }
    }
    
    # 优先读取 JS 绑定的地图点击精确坐标
    dest_loc <- NULL
    if (!is.null(input$do_custom_route$dest_lat) && !is.null(input$do_custom_route$dest_lon)) {
      dest_loc <- c(lon = as.numeric(input$do_custom_route$dest_lon), lat = as.numeric(input$do_custom_route$dest_lat))
    } else {
      dest_loc <- geocode_osm(end_str)
    }
    
    if (is.null(dest_loc)) { shinyjs::runjs("alert('Could not find Destination address.');"); return() }
    
    route_data <- get_osrm_route(start_loc, dest_loc)
    map_proxy <- leafletProxy("map") %>% clearGroup("search_res")
    
    map_proxy %>% addCircleMarkers(
      lng = dest_loc["lon"], lat = dest_loc["lat"], group = "search_res",
      radius = 10, color = "#fff", weight = 3, fillColor = "#FF3B30", fillOpacity = 1,
      label = end_str, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, className = "ios-map-label", direction = "top", offset = c(0, -10))
    )
    
    if (!identical(start_loc, current_location)) {
       map_proxy %>% addCircleMarkers(
         lng = start_loc["lon"], lat = start_loc["lat"], group = "search_res",
         radius = 10, color = "#fff", weight = 3, fillColor = "#007AFF", fillOpacity = 1,
         label = start_name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, className = "ios-map-label", direction = "top", offset = c(0, -10))
       )
    }
    
    if (!is.null(route_data)) {
      map_proxy %>% addPolylines(lng = route_data$coords[,1], lat = route_data$coords[,2], color = "#007AFF", weight = 6, opacity = 0.8, group = "search_res") %>%
        fitBounds(lng1 = min(start_loc["lon"], dest_loc["lon"]), lat1 = min(start_loc["lat"], dest_loc["lat"]), lng2 = max(start_loc["lon"], dest_loc["lon"]), lat2 = max(start_loc["lat"], dest_loc["lat"]))
      session$sendCustomMessage("start_nav_ui", list(dist = round(route_data$dist / 1609.34, 1), time = max(1, round(route_data$time / 60))))
    } else {
      shinyjs::runjs("alert('Could not generate driving route between these locations.');")
      map_proxy %>% setView(lng = dest_loc["lon"], lat = dest_loc["lat"], zoom = 15)
    }
  })
}
