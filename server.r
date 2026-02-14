library(shiny)
library(shinyMobile)
library(leaflet)

server <- function(input, output, session) {

  # ---- Base map (NO zoom +/- buttons) ----
  output$map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        zoomControl = FALSE,        # removes + / − buttons
        attributionControl = FALSE
      )
    ) %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      setView(lng = -92.36, lat = 38.95, zoom = 12)
  })

  # ---- Settings popup ----
  observeEvent(input$btn_settings, {
    f7Popup(
      id = "settings_popup",
      title = "Settings",

      f7Block(strong = TRUE, "Search Distance"),

      f7Slider(
        inputId = "search_distance",
        label = NULL,
        min = 5,
        max = 100,
        value = 20,
        step = 5
      ),

      f7BlockFooter("Used to limit nearby search results and alerts.")
    )
  })

  # ---- Search (geocode + circle) ----
  observeEvent(input$q, {
    req(nzchar(trimws(input$q)))

    if (!requireNamespace("tidygeocoder", quietly = TRUE)) {
      showNotification("Install 'tidygeocoder' to enable search.", type = "warning")
      return()
    }

    query <- trimws(input$q)

    res <- tryCatch({
      tidygeocoder::geocode(
        data.frame(address = query),
        address = address,
        method = "osm",
        quiet = TRUE
      )
    }, error = function(e) NULL)

    if (is.null(res) || anyNA(res$lat) || anyNA(res$long)) {
      showNotification("No results found.", type = "error")
      return()
    }

    lat <- res$lat[1]
    lon <- res$long[1]
    dist <- input$search_distance %||% 20

    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      setView(lng = lon, lat = lat, zoom = 15) %>%
      addCircleMarkers(lng = lon, lat = lat, radius = 6) %>%
      addCircles(
        lng = lon, lat = lat,
        radius = dist,
        fillOpacity = 0.15
      )
  })

  # No overlay panel for now
  output$panel_ui <- renderUI(NULL)
}
