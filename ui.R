fluidPage(theme=shinytheme("flatly"),
          titlePanel("Québec Biodiversity Data Explorer"),
          br(),
          tabsetPanel(
            tabPanel("Map",
                     br(),
                     sidebarLayout(
                       sidebarPanel(
                         h3("Download options"),
                         # Text input for defining the maximum extent of the territory we want to study
                         # downloading gbif's data can be time consuming, we want to get the records
                         # only at the beginning
                         radioButtons("selection_mode", label = "Selection mode: ", choices = c("WKT format" = "wkt", "Click on the map" = "mapclick")),
                         uiOutput("selection"),
                         actionButton(inputId = "selectbutton",
                                             label = "Select"),
                         bsTooltip("selectbutton", "Complete and display the selection on the map",
                                   "bottom", options = list(container = "body")),
                         actionButton(inputId = "eraseselection",
                                             label = "Erase Selection"),
                         bsTooltip("eraseselection", "Erase the area selected", "bottom", options = list(container = "body")),
                         br(),
                         br(),
                         
                         # The user needs to choose how far from this area he wants to download the data
                         numericInput(inputId = "max_buffer",
                                      label = "Area to download (maximum 10 km) :",
                                      min = 0,
                                      max = 10,
                                      value = 5),
                         bsTooltip("max_buffer", "Maximum distance from the selection for which data will be downloaded", "right", options = list(container = "body")),
                         
                         # What is the maximum number of records we want to download, the absolute max is
                         # 200 000. The more we download, the longer it takes to get the data.
                         numericInput(inputId = "max_occ",
                                      label = "Maximum number of records :",
                                      min = 1,
                                      max = 200000,
                                      value = 1000),
                         bsTooltip("max_occ", "Maximum number of observations that will be downloaded", "right", options = list(container = "body")),
                         # Action button. It will be used, when we will want to get data from gbif
                         actionButton(inputId = "get_data",
                                      label = "Get"),
                         bsTooltip("get_data", "Sart downloading data from GBIF", "right", options = list(container = "body")),
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
                         bsTooltip("max_year", "Older observations will be filtered", "right", options = list(container = "body")),
                         # Determine the extent we want to explore
                         sliderInput(inputId = "buffer",
                                     label = "Change Extent (km) :",
                                     min = -5,
                                     max = 5,
                                     value = 0,
                                     step = 0.1),
                         bsTooltip("buffer", "The distance from the selection for which downloaded data will be filered", "right", options = list(container = "body")),
                         checkboxGroupInput("kingdom", "Choose kingdom :",
                                            c("Animals" = "Animalia",
                                              "Fungi" = "Fungi",
                                              "Plants" = "Plantae"),
                                            selected = c("Animalia", "Fungi", "Plantae"))
                       ),
                       mainPanel(
                     # Map to show the selected extent
                     leafletOutput("map", width = 1200, height = 780)
                       )
                     )
            ),
            tabPanel("Raw data",
                     br(),
                     p(tags$b("This table shows all the records downloaded from GBIF. None of the filters (maximum year
                       and distance from selected area) are applied on this datatable.")),
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