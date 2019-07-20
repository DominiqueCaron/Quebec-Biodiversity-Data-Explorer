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
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(natserv)
library(tidyr)

# Database with a lot of species and their conservation status
tbl_status <- read.csv("species_status.csv", row.names = 1)

# User interface of the shiny app
ui <- fluidPage(theme=shinytheme("flatly"),
  titlePanel("Québec Biodiversity Data Explorer"),
  br(),
  sidebarLayout(
    sidebarPanel(
      h3("Download options"),
      # Text input for defining the maximum extent of the territory we want to study
      # downloading gbif's data can be time consuming, we want to get the records
      # only at the beginning
      textInput(inputId = "area", 
                label = "Area of interest :"),
      
      # The user needs to choose how far from this area he wants to download the data
      numericInput(inputId = "max_buffer",
                   label = "Area to download (maximum 10 km) :",
                   min = 0,
                   max = 10,
                   value = 5),
  
      # What is the maximum number of records we want to download, the absolute max is
      # 200 000. The more we download, the longer it takes to get the data.
      numericInput(inputId = "max_occ",
                   label = "Maximum number of records :",
                   min = 1,
                   max = 200000,
                   value = 1000),
  
      # Action button. It will be used, when we will want to get data from gbif
      actionButton(inputId = "get_data",
                   label = "Get"),
      br(),
      h3("Filter options"),
       # Determine to what maximum year we want to keep the data
       sliderInput(inputId = "max_year",
                   label = "Recorded after :",
                   min = 1800,
                   max = as.integer(format(Sys.Date(), "%Y")),
                   value = 1800,
                   sep = "",
                   step = 1),
      # Determine the extent we want to explore
      sliderInput(inputId = "buffer",
                  label = "Change Extent (km) :",
                  min = -5,
                  max = 5,
                  value = 0,
                  step = 0.1),
      checkboxGroupInput("kingdom", "Choose kingdom :",
                   c("Animals" = "Animalia",
                     "Fungi" = "Fungi",
                     "Plants" = "Plantae"),
                   selected = c("Animalia", "Fungi", "Plantae"))
      ),
    mainPanel(
      tabsetPanel(
        tabPanel("Raw data",
                 br(),
                 # Table for the raw data downloaded from gbif
                 dataTableOutput(outputId = "data")
                 ),
        tabPanel("Species list",
                 br(),
                 # Table of the species list
                 dataTableOutput(outputId = "species"),
                 # Button to download
                 downloadButton("downloadspecies", "Download")
                 ),
        tabPanel("Map",
                 br(),
                 # Map to show the selected extent
                 leafletOutput("map")
                 ),
        tabPanel("Plot",
                 br(),
                 radioButtons("conserv_system", "Choose the conservation status system",
                              c("Quebec (NatureServe)" = "QC_status",
                                "IUCN" = "iucn"),
                              inline = T),
                 # Plot of the distribution of conservation status
                 plotOutput("plot"),
                 br(),
                 h4("Information on conservation status and critera :"),
                 helpText(a("NatureServe conservation status definition",
                            href = "http://explorer.natureserve.org/nsranks.htm")),
                 helpText(a("IUCN conservation status categories",
                            href = "http://www.iucnredlist.org/static/categories_criteria_3_1#categories"))
                 )
        )
      )
    )
  )

server <- function(input, output){
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

shinyApp (ui = ui, server = server)
