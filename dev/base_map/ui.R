##### ui.R #####

library(shiny)
library(shinydashboard)
source("modules/mod_distance.R")
source("modules/mod_plant_distribution.R")

dashboardPage(
  dashboardHeader(title = "Plant Distribution Map"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
      menuItem("Distance App", tabName = "tab_distance", icon = icon("ruler")),
      menuItem("Plant Distribution", tabName = "tab_plant", icon = icon("leaf"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("tab_dashboard", h3("Shiny app for Plant Distribution")),
      tabItem("tab_distance", mod_distance_ui("distance")),
      tabItem("tab_plant", mod_plant_distribution_ui("plant"))
    )
  )
)
