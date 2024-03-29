# Needed libraries
library(shiny)
library(shinythemes)
library(shinyBS)
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
library(plotly)

# Database of special status species
tbl_status <- read.csv("species_status.csv", row.names = 1)[,-1]
