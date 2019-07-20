library(shiny)
library(shinythemes)
library(rgbif)
library(dplyr)
library(stringr)
library(DT)
library(rredlist)
library(leaflet)
library(sp)
library(rgeos)
library(raster)
library(RColorBrewer)
library(ggplot2)
library(natserv)
library(rgdal)
library(tidyr)
tbl_status <- read.csv("species_status.csv", row.names = 1)

function(input, output){
  # Create reactive of the area to download as sp object
  download_geometry <- reactive({
    wkt <- readWKT(input$area)
    proj4string(wkt) = CRS("+proj=longlat +ellps=WGS84")
    extent <- spTransform(wkt, CRS("+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")) %>%
      # Buffer
      gBuffer(width = 1*input$max_buffer*1000)
    spTransform(extent, CRS("+proj=longlat +ellps=WGS84"))
  })
  
  # Get gbif's data according to specification when the Get button is pushec
  occ <- eventReactive(input$get_data, {
    req(input$area, input$max_occ)
    occ_search(geometry = writeWKT(download_geometry()), limit=input$max_occ)$data
  })
  
  # Transform extent into a sp_Polygon object, execute de buffer
  extent <- reactive({
    req(input$area)
    wkt <- readWKT(input$area)
    proj4string(wkt) = CRS("+proj=longlat +ellps=WGS84")
    # Project on Quebec Lambert
    extent <- spTransform(wkt, CRS("+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")) %>%
      # Buffer
      gBuffer(width = 1*input$buffer*1000)
    # Retransform into geographic coordinates
    spTransform(extent, CRS("+proj=longlat +ellps=WGS84"))
  })
  
  # Create a table with the species present in the buffered extent with their conservation
  # statuses
  species_table <- reactive({
    req(occ())
    # Remove occurrence with fuzzy taxon name
    occ_clean <- occ() %>% 
      filter(!str_detect(issues, "txmatnon|txmathi|txmatfuz")) %>%
      filter(!is.na(eventDate))
    # Transform into a spatial object
    occ_sp <- SpatialPointsDataFrame(
      coords = occ_clean[, c("decimalLongitude", "decimalLatitude")],
      data = occ_clean[, c("name", "basisOfRecord", "kingdom", "phylum", "order", "family", "genus", "species", "year", "eventDate", "class", "country")],
      proj4string = CRS("+proj=longlat +ellps=WGS84"))
    # Keep only occurrences in the buffered extent
    # There is a bug in the function intersect. We need to dupplicate the occurrences
    occ_intersect <- as.data.frame(raster::intersect(bind(occ_sp,occ_sp), extent()))
    # Summarise for species, keep some info, count the number of observation
    species_table <- occ_intersect %>%
      group_by(species) %>%
      summarise(last_record=str_sub(max(eventDate),1,10),
                kingdom = kingdom[1],
                class=class[1],
                # Divide by 2 to correct the dupplication we did when calling the function intersect
                No_records=n()/2) %>%
      filter(!is.na(species)) %>%
      # Add conservation statuses from the database tbl_status
      left_join(tbl_status, by="species")
    species_table$QC_status <- species_table$QC_status %>%
      as.character() %>%
      replace_na("taxon_notlisted")
    species_table$iucn <- species_table$iucn %>%
      as.character() %>%
      replace_na("taxon_notlisted")
    return(species_table)
  })
  
  # Show downloaded data
  output$data <- DT::renderDataTable({
    occ()
  })
  
  # Datatable with conservation status
  output$species <- DT::renderDataTable({
    species_table() %>%
      filter(kingdom %in% input$kingdom) %>%
      filter(as.numeric(substring(last_record,1,4)) >= input$max_year)
  })
  
  # Download datatable of species
  output$downloadspecies <- downloadHandler(
    filename = "Species_Table.csv", content = function(file) {
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
      addPolygons(data = extent(), color="#4daf4a") %>%
      addPolygons(data=download_geometry(), color="#525252", fillColor = "transparent") %>%
      addPolygons(data = readWKT(input$area), color="#e41a1c")
  })
  
  # Barplot for conservation status
  output$plot <- renderPlot({
    data <- species_table() %>%
      filter(kingdom %in% input$kingdom) %>%
      filter(as.numeric(substring(last_record,1,4)) >= input$max_year)
    if (input$conserv_system == "iucn") {
      ggplot(data, aes(iucn))+
        geom_bar(aes(fill=kingdom)) +
        xlab("Conservation Status") +
        ylab("Number of species") +
        scale_fill_brewer(name = "Class", palette = "Set2")
    }
    else {
      ggplot(data, aes(QC_status))+
        geom_bar(aes(fill=kingdom)) +
        xlab("Conservation Status") +
        ylab("Number of species") +
        scale_fill_brewer(name = "Class", palette = "Set2")
    }
  })
}