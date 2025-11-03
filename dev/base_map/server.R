##### server.R #####

source("modules/mod_distance.R")
source("modules/mod_plant_distribution.R")

server <- function(input, output, session) {
  mod_distance_server("distance")
  mod_plant_distribution_server("plant")
}
