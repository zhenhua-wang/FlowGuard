library(shiny)
library(shinyMobile)
library(leaflet)
library(leaflet.extras)
library(shinyjs)
library(jsonlite)
library(sf)
library(terra)
library(lubridate)
library(dplyr)

# 导入底层模型与风险计算工具
source("./lgcp_utils.r")

# ==========================================
# 2. 外部调用的API辅助函数 
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

# ==========================================
# 3. 后端 Server 逻辑
# ==========================================
server <- function(input, output, session) {
  # --- 参数配置 ---
  current_location <- c(lon = -92.3341, lat = 38.9517)
  current_date <- mdy("1/24/2021") 
  
  high_risk_threshold <- 1.5   
  mod_risk_threshold  <- 1.0   
  mean_lambda <- 5.819679e-06
  buffer_radius  <- 20     
  n_fade_rings   <- 10     
  max_ring_alpha <- 0.35   
  min_ring_alpha <- 0.00   

  col_red    <- "#FF3B30"  
  col_yellow <- "#FFCC00"  
  col_green  <- "#34C759"  

  model_epsg <- 26915
  model_crs  <- sf::st_crs(model_epsg)

  # --- 动态数据追溯与模型防抖 ---
  date_lags_rv <- reactive({
    if (is.null(input$traffic_days)) return(15) 
    as.numeric(input$traffic_days)
  }) %>% debounce(800)

  model_fit <- reactiveVal(NULL)
  
  observeEvent(date_lags_rv(), {
    lags <- date_lags_rv()
    shinyjs::runjs(sprintf("showToast('Fetching data for past %d days...');", lags))
    fit <- fit_traffic_stop(current_date, date_lags = lags)
    model_fit(fit)
    shinyjs::runjs("showToast('Model Data Updated.');")
  }, ignoreInit = FALSE)

  pp_rv  <- reactive({ req(model_fit()); model_fit()$pp })
  dat_rv <- reactive({ req(model_fit()); model_fit()$dat })

  is_navigating <- reactiveVal(FALSE)
  current_route_sf <- reactiveVal(NULL)

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      setView(lng = current_location["lon"], lat = current_location["lat"], zoom = 14) %>%
      addCircleMarkers(lng = current_location["lon"], lat = current_location["lat"], 
                       radius = 10, color = "#fff", weight = 3, fillColor = "#007AFF", 
                       fillOpacity = 1, layerId = "current_loc")
  })

  # --- 独立控制：Show Traffic Stops (具体事件红点散布) ---
  observeEvent(list(input$individual_case, dat_rv()), {
    map_proxy <- leafletProxy("map")
    
    if (isTruthy(input$individual_case)) {
      req(dat_rv())
      pts_ll <- st_transform(st_as_sf(dat_rv(), coords = c("x", "y"), crs = model_crs), 4326)
      
      map_proxy %>%
        clearGroup("traffic_events") %>%
        addCircleMarkers(
          data = pts_ll, group = "traffic_events",
          radius = 3.5, stroke = FALSE,
          fillOpacity = 0.6, fillColor = col_red
        )
    } else {
      map_proxy %>% clearGroup("traffic_events")
    }
  }, ignoreInit = FALSE)

  # --- 面板交互与点击拦截逻辑 ---
  observeEvent(input$map_click, {
    if (is_navigating()) {
      shinyjs::runjs("showToast('Please END navigation to select a new location.');")
      return()
    }
    
    click <- input$map_click; req(click)
    url <- sprintf("https://nominatim.openstreetmap.org/reverse?format=json&lat=%f&lon=%f", click$lat, click$lng)
    req_data <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
    dest_name <- "Selected Location"
    if (!is.null(req_data) && !is.null(req_data$display_name)) {
      dest_name <- trimws(strsplit(req_data$display_name, ",")[[1]][1]); dest_name <- gsub("'", "", dest_name)
    }
    
    leafletProxy("map") %>% 
      clearGroup("search_res") %>% 
      setView(lng = click$lng, lat = click$lat, zoom = 15) %>%
      addCircleMarkers(lng = click$lng, lat = click$lat, group = "search_res", radius = 10, color = "#fff", weight = 3, fillColor = "#FF3B30", fillOpacity = 1, label = dest_name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, className = "ios-map-label", direction = "top", offset = c(0, -10)))
    
    js_code <- sprintf("
      $('#q_input').val('%s'); $('#q_input').attr('data-clicked-lat', %f); $('#q_input').attr('data-clicked-lon', %f);
      $('#search_panel').addClass('active'); $('#global_overlay').addClass('active'); $('#bottom_tray').addClass('panel-open'); $('#risk_btn').addClass('panel-open'); $('.loc-btn').addClass('panel-open');
    ", dest_name, click$lat, click$lng)
    shinyjs::runjs(js_code)
  })

  observeEvent(input$risk_btn, { shinyjs::addClass("risk_panel", "active"); shinyjs::addClass("global_overlay", "active"); shinyjs::addClass("risk_btn", "panel-open"); shinyjs::addClass("bottom_tray", "panel-open"); shinyjs::addClass("my_loc_btn", "panel-open") })
  observeEvent(input$settings_btn, { shinyjs::addClass("settings_panel", "active"); shinyjs::addClass("global_overlay", "active"); shinyjs::addClass("bottom_tray", "panel-open"); shinyjs::addClass("risk_btn", "panel-open"); shinyjs::addClass("my_loc_btn", "panel-open") })
  observeEvent(input$search_trigger, { 
    if (is_navigating()) return()
    shinyjs::addClass("search_panel", "active"); shinyjs::addClass("global_overlay", "active"); shinyjs::addClass("bottom_tray", "panel-open"); shinyjs::addClass("risk_btn", "panel-open"); shinyjs::addClass("my_loc_btn", "panel-open") 
  })
  observeEvent(c(input$close_all, input$c1, input$c2, input$c3), {
    shinyjs::removeClass("risk_panel", "active"); shinyjs::removeClass("settings_panel", "active"); shinyjs::removeClass("search_panel", "active"); shinyjs::removeClass("global_overlay", "active"); shinyjs::removeClass("risk_btn", "panel-open"); shinyjs::removeClass("bottom_tray", "panel-open"); shinyjs::removeClass("my_loc_btn", "panel-open")
    shinyjs::runjs("setTimeout(hideSubViews, 300);")
  }, ignoreInit = TRUE)
  
  observeEvent(input$trigger_loc_btn, { leafletProxy("map") %>% setView(lng = current_location["lon"], lat = current_location["lat"], zoom = 15) })
  
  observeEvent(input$trigger_end_nav, {
    is_navigating(FALSE)
    current_route_sf(NULL) 
    leafletProxy("map") %>% clearGroup("search_res") %>% clearGroup("risk_halos") %>% setView(lng = current_location["lon"], lat = current_location["lat"], zoom = 14)
    session$sendCustomMessage("end_nav_ui", list())
  })
  
  observeEvent(input$fly_to_loc, {
    req(input$fly_to_loc)
    shinyjs::runjs("Shiny.setInputValue('close_all', Math.random());") 
    lon <- as.numeric(input$fly_to_loc$lon); lat <- as.numeric(input$fly_to_loc$lat)
    name <- input$fly_to_loc$name; is_dest <- input$fly_to_loc$type == "q_input"
    fill_color <- ifelse(is_dest, "#FF3B30", "#007AFF") 
    leafletProxy("map") %>% clearGroup("search_res") %>% addCircleMarkers(lng = lon, lat = lat, group = "search_res", radius = 10, color = "#fff", weight = 3, fillColor = fill_color, fillOpacity = 1, label = name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, className = "ios-map-label", direction = "top", offset = c(0, -10))) %>% setView(lng = lon, lat = lat, zoom = 15)
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
    
    dest_loc <- NULL
    if (!is.null(input$do_custom_route$dest_lat) && !is.null(input$do_custom_route$dest_lon)) {
      dest_loc <- c(lon = as.numeric(input$do_custom_route$dest_lon), lat = as.numeric(input$do_custom_route$dest_lat))
    } else {
      dest_loc <- geocode_osm(end_str)
    }
    if (is.null(dest_loc)) { shinyjs::runjs("alert('Could not find Destination address.');"); return() }
    
    route_data <- get_osrm_route(start_loc, dest_loc)
    map_proxy <- leafletProxy("map") %>% clearGroup("search_res") %>% clearGroup("risk_halos")
    
    map_proxy %>% addCircleMarkers(lng = dest_loc["lon"], lat = dest_loc["lat"], group = "search_res", radius = 10, color = "#fff", weight = 3, fillColor = "#FF3B30", fillOpacity = 1, label = end_str, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, className = "ios-map-label", direction = "top", offset = c(0, -10)))
    if (!identical(start_loc, current_location)) { map_proxy %>% addCircleMarkers(lng = start_loc["lon"], lat = start_loc["lat"], group = "search_res", radius = 10, color = "#fff", weight = 3, fillColor = "#007AFF", fillOpacity = 1, label = start_name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, className = "ios-map-label", direction = "top", offset = c(0, -10))) }
    
    if (!is.null(route_data)) {
      is_navigating(TRUE) 
      
      map_proxy %>% addPolylines(lng = route_data$coords[,1], lat = route_data$coords[,2], color = "#007AFF", weight = 6, opacity = 0.8, group = "search_res") %>%
        fitBounds(lng1 = min(start_loc["lon"], dest_loc["lon"]), lat1 = min(start_loc["lat"], dest_loc["lat"]), lng2 = max(start_loc["lon"], dest_loc["lon"]), lat2 = max(start_loc["lat"], dest_loc["lat"]))
      
      session$sendCustomMessage("start_nav_ui", list(dist = round(route_data$dist / 1609.34, 1), time = max(1, round(route_data$time / 60))))
      
      shinyjs::runjs("$('#chk_risk_spot').prop('checked', true); Shiny.setInputValue('show_heatmap', true);")
      
      rc <- data.frame(lng = route_data$coords[,1], lat = route_data$coords[,2])
      ln_ll <- st_sfc(st_linestring(as.matrix(rc[, c("lng", "lat")])), crs = 4326)
      ln_m <- st_transform(ln_ll, model_crs)
      current_route_sf(ln_m) 
      
    } else {
      shinyjs::runjs("alert('Could not generate driving route between these locations.');")
      map_proxy %>% setView(lng = dest_loc["lon"], lat = dest_loc["lat"], zoom = 15)
    }
  })

  # =================================================================================
  # 核心渲染与状态更新 (控制 Risk Spot 的波纹和路线管状光晕，不再带有任何散点)
  # =================================================================================
  observeEvent(list(input$show_heatmap, current_route_sf(), pp_rv()), {
    if (is.null(input$show_heatmap) || !input$show_heatmap) {
      session$sendCustomMessage("update_risk_level", list(level = "Off"))
      leafletProxy("map") %>% 
        clearGroup("risk_halos") %>% 
        clearGroup("current_risk_halo")
      return()
    }
    
    req(pp_rv())
    map_proxy <- leafletProxy("map")
    
    curr_pt <- st_sfc(st_point(c(current_location["lon"], current_location["lat"])), crs=4326) %>% st_transform(model_crs)
    
    if (is_navigating() && !is.null(current_route_sf())) {
      road_m <- current_route_sf()
      bb_route <- st_bbox(st_buffer(road_m, dist = buffer_radius + 10))
      bb_curr <- st_bbox(st_buffer(curr_pt, 500))
      bb <- c(
        xmin = min(bb_route["xmin"], bb_curr["xmin"]),
        ymin = min(bb_route["ymin"], bb_curr["ymin"]),
        xmax = max(bb_route["xmax"], bb_curr["xmax"]),
        ymax = max(bb_route["ymax"], bb_curr["ymax"])
      )
    } else {
      bb <- st_bbox(st_buffer(curr_pt, 500))
    }
    
    pred <- predict_lgcp(pp_rv(), xmin = as.numeric(bb["xmin"]), xmax = as.numeric(bb["xmax"]), ymin = as.numeric(bb["ymin"]), ymax = as.numeric(bb["ymax"]), npred = 200)
    r_m <- terra::rast(pred[, c("x", "y", "p")], type = "xyz")
    terra::crs(r_m) <- model_crs$wkt
    rr_m <- r_m / mean_lambda
    
    curr_rr <- terra::extract(rr_m, terra::vect(curr_pt))[, 2]
    
    if (is.na(curr_rr)) curr_level <- "Low"
    else if (curr_rr > high_risk_threshold) curr_level <- "High"
    else if (curr_rr > mod_risk_threshold) curr_level <- "Medium"
    else curr_level <- "Low"
    
    session$sendCustomMessage("update_risk_level", list(level = curr_level))
    
    curr_color <- if (curr_level == "High") col_red else if (curr_level == "Medium") col_yellow else col_green
    
    map_proxy %>% clearGroup("current_risk_halo")
    spot_radius <- buffer_radius 
    radii_curr <- seq(spot_radius, 1, length.out = n_fade_rings)
    alpha_step_curr <- 1.0 / n_fade_rings 
    
    for (j in seq_len(n_fade_rings)) {
      map_proxy %>% addCircles(
        lng = current_location["lon"], lat = current_location["lat"],
        radius = radii_curr[j], group = "current_risk_halo", stroke = FALSE, fill = TRUE,
        fillColor = curr_color, fillOpacity = alpha_step_curr, options = pathOptions(clickable = FALSE)
      )
    }

    map_proxy %>% clearGroup("risk_halos")
    
    if (is_navigating() && !is.null(current_route_sf())) {
      road_linestring <- st_cast(road_m, "LINESTRING")
      sample_pts_m <- st_line_sample(road_m, density = 1/30) 
      sample_pts_sf <- st_as_sf(st_cast(sample_pts_m, "POINT"))
      extracted_rr <- terra::extract(rr_m, terra::vect(sample_pts_sf))
      pts_ll <- st_transform(sample_pts_sf, 4326)
      coords <- st_coordinates(pts_ll)
      
      radii  <- seq(1, buffer_radius, length.out = n_fade_rings)
      alphas <- seq(max_ring_alpha, min_ring_alpha, length.out = n_fade_rings)

      risk_data <- data.frame(lng = coords[, 1], lat = coords[, 2], rr = extracted_rr[, 2]) %>%
        filter(!is.na(rr) & rr > mod_risk_threshold) 

      for (i in seq_len(nrow(risk_data))) {
        p_lng <- risk_data$lng[i]
        p_lat <- risk_data$lat[i]
        p_rr  <- risk_data$rr[i]
        
        is_high <- p_rr > high_risk_threshold
        p_color <- if (is_high) col_red else col_yellow
        
        for (j in seq_len(n_fade_rings)) {
          if (alphas[j] <= 0) next
          map_proxy %>% addCircles(
            lng = p_lng, lat = p_lat, radius = radii[j], group = "risk_halos", 
            stroke = FALSE, fill = TRUE, fillColor = p_color, fillOpacity = alphas[j]
          )
        }
      }
    }
    
  }, ignoreInit = TRUE)
}