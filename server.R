function(input, output,session){
  
  selected_area <- reactive({
    req(input$area)
    readWKT(input$area)
  })
  
  output$selection <- renderUI({
    if (input$selection_mode == "wkt"){
      textInput(inputId = "area", 
                label = "Area of interest :")
    }
    else if (input$selection_mode == "mapclick"){
      checkboxInput(inputId = "click_activation",
                             label = "Enable area selection",
                             value = TRUE)}
  })
  
  # Create a reactive value to store position clicked
  click_selection <- reactiveValues(x = NULL, y = NULL)
  selected_area <- reactiveValues(selection = NULL,
                                  downloadarea = NULL,
                                  filter = NULL)
  occ_data <- reactiveValues(data = NULL)
  
  
  # When the user click on the map, store the positions in the object click_selection
  observeEvent(input$map_click, {
    if (input$selection_mode == "mapclick" && input$click_activation == TRUE) {
      tempx <- c(click_selection$x, input$map_click$lng)
      tempy <- c(click_selection$y, input$map_click$lat)
      click_selection$x <- tempx
      click_selection$y <- tempy
    }
  })
  
  # When the user click on the map, show the polygon being created
  observe({
    if (length(click_selection$x) > 1) {
        leafletProxy("map") %>%
          removeMarker(layerId = "single_marker") %>%
          addPolylines(
            lng = c(click_selection$x),
            lat = c(click_selection$y),
            stroke = TRUE,
            weight = 2,
            color = "#d95f02",
            group = "region_line"
          )
    }
    if (length(click_selection$x) == 1) {
      leafletProxy("map") %>%
        addMarkers(lng = click_selection$x,
                   lat = click_selection$y,
                   layerId = "single_marker")
    }
  })
  
  # When user push erase selection:
  observeEvent(input$eraseselection,{
    leafletProxy("map") %>%
      removeMarker(layerId = "single_marker") %>%
      clearGroup("region_line") %>%
      clearGroup("download_area") %>%
      clearGroup("filter_area")
    if (!is.null(click_selection$x)){
      click_selection$x = NULL
      click_selection$y = NULL
    }
  }
  )
  
  # When user push select button:
  observeEvent(input$selectbutton,{
    if (input$selection_mode == "mapclick" & length(click_selection$x) >=3){
      tempx <- c(click_selection$x, click_selection$x[1])
      tempy <- c(click_selection$y, click_selection$y[1])
      click_selection$x <- tempx
      click_selection$y <- tempy
      extent <- Polygon(cbind(click_selection$x, click_selection$y)) %>%
        list() %>% 
        Polygons(ID = 1) %>% 
        list() %>%
        SpatialPolygons(proj4string=CRS(as.character("+proj=longlat +ellps=WGS84")))
      updateCheckboxInput(session, "click_activation", value = FALSE)
      selected_area$selection <- extent
    }
    else if (input$selection_mode == "wkt" & input$area != ""){
      wkt <- readWKT(input$area)
      proj4string(wkt) = CRS("+proj=longlat +ellps=WGS84")
      selected_area$selection <- wkt
    }
    if (!is.null(selected_area$selection)){
      leafletProxy("map") %>%
        removeMarker(layerId = "single_marker") %>%
        addPolygons(
          data = selected_area$selection,
          stroke = TRUE,
          weight = 2,
          color = "#d95f02",
          group = "region_line"
        )
    }
  }
  )
  
  
  # Create reactive of the area to download as sp object
  download_geometry <- reactive({
    req(selected_area$selection)
    extent <- spTransform(selected_area$selection, CRS("+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")) %>%
      # Buffer
      gBuffer(width = 1*input$max_buffer*1000)
    spTransform(extent, CRS("+proj=longlat +ellps=WGS84"))
  })
  
  # Get gbif's data according to specification when the Get button is pushec
  observeEvent(input$get_data, {
    withProgress(message = "Data extraction...",{
    occ_data$data <- occ_search(geometry = writeWKT(download_geometry()), limit=input$max_occ)$data
    if (!is.null(download_geometry())){
      leafletProxy("map") %>%
        removeMarker(layerId = "single_marker") %>%
        addPolygons(
          data = download_geometry(),
          stroke = TRUE,
          weight = 2,
          color = "#7570b3",
          fillOpacity = 0,
          group = "download_area"
        )
    }
    })
  })
  
  # Transform extent into a sp_Polygon object, execute de buffer
  filter_area <- reactive({
    req(selected_area$selection)
      extent <- spTransform(selected_area$selection, CRS("+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")) %>%
        gBuffer(width = 1*input$buffer*1000)
      extent <- spTransform(extent, CRS("+proj=longlat +ellps=WGS84"))
      extent
      }
  )
    
  observeEvent(filter_area(),{
    leafletProxy("map") %>%
      removeMarker(layerId = "single_marker") %>%
      clearGroup("filter_area") %>%
      addPolygons(
        data = filter_area(),
        stroke = TRUE,
        weight = 2,
        color = "#1b9e77",
        fillOpacity = 0,
        group = "filter_area"
      )
  }
  )
  
  # Create a table with the species present in the buffered extent with their conservation
  # statuses
  species_table <- reactive({
    req(occ_data$data)
    # Remove occurrence with fuzzy taxon name
    occ_clean <- occ_data$data %>% 
      filter(!str_detect(issues, "txmatnon|txmathi|txmatfuz")) %>%
      filter(!is.na(eventDate))
    # Transform into a spatial object
    occ_sp <- SpatialPointsDataFrame(
      coords = occ_clean[, c("decimalLongitude", "decimalLatitude")],
      data = occ_clean[, c("name", "basisOfRecord", "kingdom", "phylum", "order", "family", "genus", "species", "year", "eventDate", "class", "country")],
      proj4string = CRS("+proj=longlat +ellps=WGS84"))
    # Keep only occurrences in the buffered extent
    # There is a bug in the function intersect. We need to dupplicate the occurrences
    occ_intersect <- as.data.frame(raster::intersect(bind(occ_sp,occ_sp), filter_area()))
    # Summarise for species, keep some info, count the number of observation
    species_table <- occ_intersect %>%
      group_by(species) %>%
      summarise(last_record=str_sub(max(eventDate),1,10),
                kingdom = kingdom[1],
                class = class[1],
                # Divide by 2 to correct the dupplication we did when calling the function intersect
                No_records=n()/2) %>%
      filter(!is.na(species)) %>%
      filter(kingdom %in% input$kingdom) %>%
      filter(as.numeric(substring(last_record,1,4)) >= input$max_year) %>%
      # Add conservation statuses from the database tbl_status
      left_join(tbl_status, by="species")
    species_table$QC_status <- species_table$QC_status %>%
      as.character() %>%
      replace_na("SNR")
    species_table$iucn <- species_table$iucn %>%
      as.character() %>%
      replace_na("NE")
    return(species_table)
  })
  
  # Show downloaded data
  output$data <- DT::renderDataTable({
    occ_data$data
  })
  
  output$downloadData <- downloadHandler(
    filename = "Occurence_data.csv",
    content = function(file) {
      write.csv(occ_data$data, file, row.names = FALSE)
    }
  )
  
  # Datatable with conservation status
  output$species <- DT::renderDataTable({
    species_table() %>%
      filter(kingdom %in% input$kingdom) %>%
      filter(as.numeric(substring(last_record,1,4)) >= input$max_year)
  })
  
  # Download datatable of species
  output$downloadspecies <- downloadHandler(
    filename = "Species_Table.csv",
    content = function(file) {
      write.csv(species_table(), file, row.names = FALSE)
    },
    contentType="text/csv"
  )
  
  # Map with selected extent
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      addLegend("topright",
                title = "Legend",
                colors = c("#d95f02", "#7570b3", "#1b9e77"),
                labels = c("Area Selected", "Data extraction area", "Filter Area"),
                group = c("region_line","download_area","filter_area")
      ) %>%
      setView(-68, 53, zoom = 5)
  })
  
  observe({
    leafletProxy("map") %>%
      clearShapes()
  })
  
  # Barplot for conservation status
  output$plot <- renderPlotly({
    data <- species_table()
    heighcol <- input$barheight
    data$QC_status [data$QC_status %in% c("S1B", "S1M")] <- "S1"
    data$QC_status[data$QC_status %in% c("S2B", "S2M")] <- "S2"
    data$QC_status[data$QC_status %in% c("S3B", "S3M")] <- "S3"
    data$QC_status[data$QC_status %in% c("S4B", "S4M")] <- "S4"
    data$QC_status[data$QC_status %in% c("S5B", "S5M")] <- "S5"
    data$QC_status[data$QC_status %in% c("SH", "SX")] <- "SH-SX"
    data$QC_status <- ordered(data$QC_status, levels = c("SNA", "SH-SX", "S1", "S2", "S3", "S4", "S5", "SNR"))
    data$iucn <- ordered(data$iucn, levels = c("DD", "EW", "CR", "EN", "VU", "NT", "LC", "NE"))
    if (input$conserv_system == "iucn") {
      data$Status <- data$iucn
      p <- ggplot(data) +
        geom_bar(aes(class, fill = Status,
                     text = paste("Class:", class,
                                  "\nConservation status:", Status))) +
        ylab("Number of species") +
        coord_flip() +
        scale_fill_manual(limits = c( "DD", "EW", "CR", "EN", "VU", "NT", "LC", "NE"), name = "Status", 
                          values = c("#878787", "black", "#a50026", "#d73027", "#fdae61", "#fee08b", "#66bd63", "#006837")) +
        theme_bw() +
        theme(text = element_text(size=15), axis.title.y=element_blank())
      ggplotly(p, tooltip = c("text","count")) %>% config(displayModeBar = F)
     }
    else {
      data$Status <- data$QC_status
      p <- ggplot(data) +
        geom_bar(aes(class, fill = Status,
                     text = paste("Class:", class,
                                  "\nConservation status:", Status))) +
        ylab("Number of species") +
        coord_flip() +
        scale_fill_manual(limits = c("SNA", "SH-SX", "S1", "S2", "S3", "S4", "S5", "SNR"), name = "Status", 
                          values = c("#878787", "black", "#a50026", "#d73027", "#fdae61", "#fee08b", "#66bd63", "#006837")) +
        theme_bw() +
        theme(text = element_text(size=15), axis.title.y=element_blank())
      ggplotly(p, tooltip = c("text","count")) %>% config(displayModeBar = F)
    }
  })
}