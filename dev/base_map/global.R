##### global.R #####

# ---- Libraries ----
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(lubridate)
library(ggmap)
library(htmltools)

# ---- Load Data ----
source("helpers/data_load.R")


# ---- 埼玉全域のバウンディングボックス ----
saitama_all <- st_bbox(admin)



# ---- Color Palette ----
pal_decade <- colorFactor(
  palette = c("blue", "green", "orange", "red", "purple"),
  domain = plant_dist$decade
)
