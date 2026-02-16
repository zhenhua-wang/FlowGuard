# ==========================================
# server.r - 后端业务逻辑与模型处理
# ==========================================
library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinyjs)
library(jsonlite)
library(sf)
library(terra)
library(lubridate)
library(dplyr)
library(httr) 

source("./lgcp_utils.r")

# ==========================================
# 辅助服务与计算
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

get_bearing <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  dLon <- (lon2 - lon1) * rad
  lat1 <- lat1 * rad
  lat2 <- lat2 * rad
  y <- sin(dLon) * cos(lat2)
  x <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(dLon)
  bearing <- atan2(y, x) / rad
  return((bearing + 360) %% 360)
}

get_weather <- function(lat, lon) {
  url <- sprintf("https://api.open-meteo.com/v1/forecast?latitude=%f&longitude=%f&current_weather=true", lat, lon)
  res <- tryCatch({
    req <- GET(url, timeout(3))
    if (status_code(req) == 200) {
      data <- fromJSON(content(req, "text", encoding = "UTF-8"))
      code <- data$current_weather$weathercode
      temp <- data$current_weather$temperature
      
      desc <- "Clear/Cloudy"
      if (code %in% c(45,48)) desc <- "Fog"
      if (code >= 51 && code <= 67) desc <- "Rain"
      if (code >= 71 && code <= 86) desc <- "Snow"
      if (code >= 95) desc <- "Thunderstorm"
      
      return(sprintf("%s, %.1f C", desc, temp))
    }
    return("Unknown Weather")
  }, error = function(e) { return("Unknown Weather") })
  return(res)
}

get_road_info <- function(lat, lon) {
  url <- paste0("https://overpass-api.de/api/interpreter?data=[out:json];way(around:30,", lat, ",", lon, ")[\"highway\"];out tags;")
  res <- tryCatch({
    req <- GET(url, timeout(3))
    if (status_code(req) == 200) {
      data <- fromJSON(content(req, "text", encoding = "UTF-8"))
      if (length(data$elements) > 0 && "tags" %in% names(data$elements)) {
        tags <- data$elements$tags
        maxspeed <- if ("maxspeed" %in% names(tags)) tags$maxspeed[1] else "Unknown"
        road_name <- if ("name" %in% names(tags)) tags$name[1] else "Local Road"
        return(sprintf("Road: %s, Speed Limit: %s", road_name, maxspeed))
      }
    }
    return("Road Info: Unknown")
  }, error = function(e) { return("Road Info: Unknown") })
  return(res)
}

call_archia_agent <- function(prompt_text) {
  archia_api_key <- "ask_6aHeqetNVu285mInrck-QgybeJK14AcZGOQNlSlZ7XE=" 
  archia_endpoint <- "https://registry.archia.app/v1/responses" 
  agent_name <- "TrafficCopilot" 
  
  res <- tryCatch({
    req <- POST(
      url = archia_endpoint,
      add_headers(
        Authorization = paste("Bearer", archia_api_key), 
        `Content-Type` = "application/json"
      ),
      body = toJSON(list(
        model = paste0("agent:", agent_name),
        input = prompt_text
      ), auto_unbox = TRUE),
      timeout(8) 
    )
    
    resp_text <- content(req, "text", encoding = "UTF-8")
    
    if (status_code(req) != 200) {
       print(paste("Archia HTTP Error:", status_code(req)))
       return("Risk: Unknown\nSpeed: N/A\nSuggestion: API Server Error.")
    }
    
    data <- fromJSON(resp_text)
    
    if (!is.null(data$status)) {
       if (data$status == "failed") {
           err_msg <- if(!is.null(data$error$message)) data$error$message else "Unknown error"
           print(paste("Archia Returned Failed Status:", err_msg))
           return("Risk: Unknown\nSpeed: N/A\nSuggestion: Agent processing failed.")
       } else if (data$status != "completed") {
           return("Risk: Unknown\nSpeed: N/A\nSuggestion: Agent status not completed.")
       }
    }
    
    out_str <- NULL
    if (!is.null(data$output)) {
       if (is.data.frame(data$output) && length(data$output$content) > 0 && is.data.frame(data$output$content[[1]])) {
           out_str <- data$output$content[[1]]$text[1]
       } else if (is.list(data$output) && !is.null(data$output[[1]]$content[[1]]$text)) {
           out_str <- data$output[[1]]$content[[1]]$text
       }
    }
    
    if (is.null(out_str) || is.na(out_str) || out_str == "") {
      return("Risk: Unknown\nSpeed: N/A\nSuggestion: Empty response from AI.")
    }
    return(as.character(out_str))
  }, error = function(e) { 
    return("Risk: Unknown\nSpeed: N/A\nSuggestion: Network connection failed.") 
  })
  return(res)
}

# ==========================================
# 核心 Server 逻辑
# ==========================================
server <- function(input, output, session) {
  
  current_location <- reactiveVal(c(lon = -92.3341, lat = 38.9517))
  current_date <- mdy("1/24/2021") 
  
  buffer_radius  <- 45     
  n_fade_rings   <- 10     
  
  col_red    <- "#FF3B30"  
  col_yellow <- "#FFCC00"  
  col_green  <- "#34C759"  

  model_epsg <- 26915
  model_crs  <- sf::st_crs(model_epsg)
  
  is_navigating <- reactiveVal(FALSE)
  current_route_sf <- reactiveVal(NULL)
  view_mode <- reactiveVal("centered") 
  route_bearing <- reactiveVal(0)
  route_bounds <- reactiveVal(NULL)

  observeEvent(input$user_location, {
    req(input$user_location)
    loc <- c(lon = as.numeric(input$user_location$lon), lat = as.numeric(input$user_location$lat))
    current_location(loc)
    
    current_zoom <- if(is_navigating() && view_mode() == "centered") 18 else 15
    
    leafletProxy("map") %>%
      setView(lng = loc["lon"], lat = loc["lat"], zoom = current_zoom) %>%
      removeMarker(layerId = "current_loc") %>%
      addCircleMarkers(lng = loc["lon"], lat = loc["lat"],
                       radius = 10, color = "#fff", weight = 3, fillColor = "#007AFF",
                       fillOpacity = 1, layerId = "current_loc")
  })

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

  output$map <- renderLeaflet({
    loc <- isolate(current_location())
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      setView(lng = loc["lon"], lat = loc["lat"], zoom = 14) %>%
      addCircleMarkers(lng = loc["lon"], lat = loc["lat"], 
                       radius = 10, color = "#fff", weight = 3, fillColor = "#007AFF", 
                       fillOpacity = 1, layerId = "current_loc")
  })

  observeEvent(list(input$individual_case, dat_rv()), {
    map_proxy <- leafletProxy("map")
    if (isTruthy(input$individual_case)) {
      req(dat_rv())
      pts_ll <- st_transform(st_as_sf(dat_rv(), coords = c("x", "y"), crs = model_crs), 4326)
      map_proxy %>% clearGroup("traffic_events") %>%
        addCircleMarkers(data = pts_ll, group = "traffic_events", radius = 3.5, stroke = FALSE, fillOpacity = 0.6, fillColor = col_red)
    } else {
      map_proxy %>% clearGroup("traffic_events")
    }
  }, ignoreInit = FALSE)

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
    
    leafletProxy("map") %>% clearGroup("search_res") %>% setView(lng = click$lng, lat = click$lat, zoom = 15) %>%
      addCircleMarkers(lng = click$lng, lat = click$lat, group = "search_res", radius = 10, color = "#fff", weight = 3, fillColor = "#FF3B30", fillOpacity = 1, label = dest_name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, className = "ios-map-label", direction = "top", offset = c(0, -10)))
    
    js_code <- sprintf("$('#q_input').val('%s'); $('#q_input').attr('data-clicked-lat', %f); $('#q_input').attr('data-clicked-lon', %f); $('#search_panel').addClass('active'); $('#global_overlay').addClass('active'); $('#bottom_tray').addClass('panel-open'); $('#risk_btn').addClass('panel-open'); $('#view_capsule').addClass('panel-open');", dest_name, click$lat, click$lng)
    shinyjs::runjs(js_code)
  })

  observeEvent(input$risk_btn, { shinyjs::addClass("risk_panel", "active"); shinyjs::addClass("global_overlay", "active"); shinyjs::addClass("risk_btn", "panel-open"); shinyjs::addClass("bottom_tray", "panel-open"); shinyjs::addClass("view_capsule", "panel-open") })
  observeEvent(input$settings_btn, { shinyjs::addClass("settings_panel", "active"); shinyjs::addClass("global_overlay", "active"); shinyjs::addClass("bottom_tray", "panel-open"); shinyjs::addClass("risk_btn", "panel-open"); shinyjs::addClass("view_capsule", "panel-open") })
  observeEvent(input$search_trigger, { 
    if (is_navigating()) return()
    shinyjs::addClass("search_panel", "active"); shinyjs::addClass("global_overlay", "active"); shinyjs::addClass("bottom_tray", "panel-open"); shinyjs::addClass("risk_btn", "panel-open"); shinyjs::addClass("view_capsule", "panel-open") 
  })
  observeEvent(c(input$close_all, input$c1, input$c2, input$c3), {
    shinyjs::removeClass("risk_panel", "active"); shinyjs::removeClass("settings_panel", "active"); shinyjs::removeClass("search_panel", "active"); shinyjs::removeClass("global_overlay", "active"); shinyjs::removeClass("risk_btn", "panel-open"); shinyjs::removeClass("bottom_tray", "panel-open"); shinyjs::removeClass("view_capsule", "panel-open")
    shinyjs::runjs("setTimeout(hideSubViews, 300);")
  }, ignoreInit = TRUE)
  
  observeEvent(input$trigger_loc_btn, { 
    loc <- current_location()
    if (is_navigating()) {
      view_mode("centered")
      session$sendCustomMessage("set_map_rotation", list(deg = -route_bearing(), mode = "centered"))
      leafletProxy("map") %>% setView(lng = loc["lon"], lat = loc["lat"], zoom = 18)
    } else {
      leafletProxy("map") %>% setView(lng = loc["lon"], lat = loc["lat"], zoom = 15)
    }
  })
  
  observeEvent(input$trigger_toggle_view, {
    req(is_navigating())
    if (view_mode() == "centered") {
      view_mode("global")
      b <- route_bounds()
      session$sendCustomMessage("set_map_rotation", list(deg = 0, mode = "global", bounds = b))
    } else {
      view_mode("centered")
      session$sendCustomMessage("set_map_rotation", list(deg = -route_bearing(), mode = "centered"))
      loc <- current_location()
      leafletProxy("map") %>% setView(lng = loc["lon"], lat = loc["lat"], zoom = 18)
    }
  })
  
  observeEvent(input$trigger_end_nav, {
    is_navigating(FALSE)
    current_route_sf(NULL) 
    route_bounds(NULL)
    view_mode("centered")
    
    session$sendCustomMessage("set_map_rotation", list(deg = 0, mode = "global"))
    
    loc <- current_location()
    leafletProxy("map") %>% clearGroup("search_res") %>% clearGroup("risk_halos") %>% setView(lng = loc["lon"], lat = loc["lat"], zoom = 15)
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
    
    start_loc <- NULL
    if (tolower(start_str) %in% c("current location", "my location", "")) {
      start_loc <- current_location(); start_name <- "Current Location"
    } else {
      start_loc <- geocode_osm(start_str); start_name <- start_str
      if (is.null(start_loc)) { start_loc <- current_location(); start_name <- "Current Location" }
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
    
    if (!identical(start_loc, current_location())) { 
      map_proxy %>% addCircleMarkers(lng = start_loc["lon"], lat = start_loc["lat"], group = "search_res", radius = 10, color = "#fff", weight = 3, fillColor = "#007AFF", fillOpacity = 1, label = start_name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, className = "ios-map-label", direction = "top", offset = c(0, -10))) 
    }
    
    if (!is.null(route_data)) {
      is_navigating(TRUE) 
      view_mode("centered")
      
      coords <- route_data$coords
      pt1 <- coords[1, ]
      pt2_idx <- min(10, nrow(coords)) 
      pt2 <- coords[pt2_idx, ]
      
      bearing <- get_bearing(pt1[1], pt1[2], pt2[1], pt2[2])
      route_bearing(bearing)
      
      bnd <- list(
        lng1 = min(start_loc["lon"], dest_loc["lon"]), lat1 = min(start_loc["lat"], dest_loc["lat"]), 
        lng2 = max(start_loc["lon"], dest_loc["lon"]), lat2 = max(start_loc["lat"], dest_loc["lat"])
      )
      route_bounds(bnd)
      
      map_proxy %>% 
        addPolylines(lng = coords[,1], lat = coords[,2], color = "#007AFF", weight = 6, opacity = 0.8, group = "search_res") %>%
        setView(lng = start_loc["lon"], lat = start_loc["lat"], zoom = 18)
      
      session$sendCustomMessage("start_nav_ui", list(dist = round(route_data$dist / 1609.34, 1), time = max(1, round(route_data$time / 60))))
      session$sendCustomMessage("set_map_rotation", list(deg = -bearing, mode = "centered"))
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

  observeEvent(list(input$show_heatmap, current_route_sf(), pp_rv(), current_location()), {
    if (is.null(input$show_heatmap) || !input$show_heatmap) {
      session$sendCustomMessage("update_risk_level", list(level = "Off"))
      session$sendCustomMessage("update_ai_advice", list(text = "Please turn on 'Risk Spot' or start navigation to view AI suggestions."))
      leafletProxy("map") %>% clearGroup("risk_halos") %>% clearGroup("current_risk_halo")
      return()
    }
    
    req(pp_rv())
    map_proxy <- leafletProxy("map")
    session$sendCustomMessage("update_ai_advice", list(text = "TraffIQ is analyzing environment context..."))
    
    loc <- current_location()
    curr_pt <- st_sfc(st_point(c(loc["lon"], loc["lat"])), crs=4326) %>% st_transform(model_crs)
    
    if (is_navigating() && !is.null(current_route_sf())) {
      road_m <- current_route_sf()
      bb_route <- st_bbox(st_buffer(road_m, dist = buffer_radius + 10))
      bb_curr <- st_bbox(st_buffer(curr_pt, 500))
      bb <- c(xmin = min(bb_route["xmin"], bb_curr["xmin"]), ymin = min(bb_route["ymin"], bb_curr["ymin"]), xmax = max(bb_route["xmax"], bb_curr["xmax"]), ymax = max(bb_route["ymax"], bb_curr["ymax"]))
    } else {
      bb <- st_bbox(st_buffer(curr_pt, 500))
    }
    
    pred <- predict_lgcp(pp_rv(), xmin = as.numeric(bb["xmin"]), xmax = as.numeric(bb["xmax"]), ymin = as.numeric(bb["ymin"]), ymax = as.numeric(bb["ymax"]), npred = 200)
    r_m <- terra::rast(pred[, c("x", "y", "p")], type = "xyz")
    terra::crs(r_m) <- model_crs$wkt
    
    curr_rr_raw <- terra::extract(r_m, terra::vect(curr_pt))
    curr_raw_val <- 0.0
    if (!is.null(curr_rr_raw) && nrow(curr_rr_raw) > 0) {
       val <- curr_rr_raw[1, 2]
       if (!is.na(val)) curr_raw_val <- val
    }
    
    current_time_str <- format(Sys.time(), "%I:%M %p")
    weather_cond <- get_weather(loc["lat"], loc["lon"])
    road_info <- get_road_info(loc["lat"], loc["lon"])
    
    user_risk_tolerance <- if (is.null(input$risk)) "Moderate" else input$risk
    
    agent_input <- sprintf(
      "System Input:\n- Raw Spatial Risk Score: %e\n- User Risk Tolerance: %s\n- Location: [%f, %f]\n- Current Time: %s\n- Weather: %s\n- %s\n\nTask: Evaluate driving risk factoring in the user's risk tolerance. Output EXACTLY 3 lines:\nRisk: [Low/Medium/High]\nSpeed: [X mph]\nSuggestion: [1 brief sentence]",
      curr_raw_val, user_risk_tolerance, loc["lat"], loc["lon"], current_time_str, weather_cond, road_info
    )
    
    ai_response <- call_archia_agent(agent_input)
    
    if (length(ai_response) == 0 || is.na(ai_response) || is.null(ai_response)) {
      ai_response <- "Risk: Unknown\nSpeed: N/A\nSuggestion: Unrecognized Agent output."
    }
    ai_response <- paste(ai_response, collapse = "\n")
    
    parsed_level <- "Low"
    if (is.character(ai_response) && length(ai_response) > 0) {
      if (grepl("Risk:\\s*High", ai_response, ignore.case = TRUE)) parsed_level <- "High"
      else if (grepl("Risk:\\s*Medium", ai_response, ignore.case = TRUE)) parsed_level <- "Medium"
    }
    
    ui_text <- gsub("\\*\\*(?i)Risk:\\*\\*|(?i)Risk:", "<b>Risk:</b>", ai_response)
    ui_text <- gsub("\\*\\*(?i)Speed:\\*\\*|(?i)Speed:", "<b>Speed:</b>", ui_text)
    ui_text <- gsub("\\*\\*(?i)Suggestion:\\*\\*|(?i)Suggestion:|\\*\\*(?i)Advice:\\*\\*|(?i)Advice:", "<b>Suggestion:</b>", ui_text)
    ui_text <- gsub("\\n", "<br/>", ui_text)
    
    session$sendCustomMessage("update_ai_advice", list(text = ui_text))
    session$sendCustomMessage("update_risk_level", list(level = parsed_level))
    
    curr_color <- if (parsed_level == "High") col_red else if (parsed_level == "Medium") col_yellow else col_green
    
    map_proxy %>% clearGroup("current_risk_halo")
    radii_curr <- seq(buffer_radius, 1, length.out = n_fade_rings)
    alpha_step_curr <- 1.5 / n_fade_rings 
    for (j in seq_len(n_fade_rings)) {
      map_proxy %>% addCircles(
        lng = loc["lon"], lat = loc["lat"], radius = radii_curr[j], group = "current_risk_halo", 
        stroke = FALSE, fill = TRUE, fillColor = curr_color, fillOpacity = alpha_step_curr, options = pathOptions(clickable = FALSE)
      )
    }

    map_proxy %>% clearGroup("risk_halos")
    if (is_navigating() && !is.null(current_route_sf())) {
      sample_pts_m <- tryCatch({
        st_line_sample(road_m, density = 1/30)
      }, error = function(e) NULL)
      
      if (!is.null(sample_pts_m) && length(sample_pts_m) > 0) {
        sample_pts_sf <- st_as_sf(st_cast(sample_pts_m, "POINT"))
        extracted_raw <- terra::extract(r_m, terra::vect(sample_pts_sf))[,2]
        
        valid_raw <- extracted_raw[!is.na(extracted_raw)]
        if (length(valid_raw) > 0) {
          min_r <- min(valid_raw, na.rm=TRUE)
          max_r <- max(valid_raw, na.rm=TRUE)
          range_r <- max_r - min_r
          if(range_r == 0) range_r <- 1e-9
          
          norm_rr <- (extracted_raw - min_r) / range_r
          pts_ll <- st_transform(sample_pts_sf, 4326)
          coords <- st_coordinates(pts_ll)
          
          risk_data <- data.frame(lng = coords[, 1], lat = coords[, 2], norm_val = norm_rr) %>% 
            filter(!is.na(norm_val) & norm_val > 0.4) %>%
            mutate(color = ifelse(norm_val > 0.75, col_red, col_yellow))

          route_buffer_radius <- buffer_radius * 0.75  
          radii_route <- seq(route_buffer_radius, 1, length.out = n_fade_rings)
          alpha_step_route <- 1.2 / n_fade_rings  
          
          for (j in seq_len(n_fade_rings)) {
            map_proxy %>% addCircles(
              lng = risk_data$lng, lat = risk_data$lat, 
              radius = radii_route[j], group = "risk_halos", 
              stroke = FALSE, fill = TRUE, 
              fillColor = risk_data$color, 
              fillOpacity = alpha_step_route, 
              options = pathOptions(clickable = FALSE)
            )
          }
        }
      }
    }
  }, ignoreInit = TRUE)
}