##### modules/mod_distance.R #####

mod_distance_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Distance Measurement"),
    sidebarLayout(
      sidebarPanel(
        textInput(ns("word1"), "Word 1", value = "東京"),
        textInput(ns("word2"), "Word 2", value = "埼玉"),
        actionButton(ns("submit"), "Show on Map")
      ),
      mainPanel(leafletOutput(ns("map"), height = "600px"))
    )
  )
}

mod_distance_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    values <- reactiveValues(geocodes = rbind(
      c(139.6917, 35.68949),
      c(140.1233, 35.60506)
    ))

    observeEvent(input$submit, {
      geo1 <- geocode(input$word1)
      geo2 <- geocode(input$word2)

      if (is.na(geo1[1, 1]) || is.na(geo2[1, 1])) {
        showModal(modalDialog(
          title = "エラー", "検索条件に該当するデータがありません",
          easyClose = TRUE, footer = modalButton("OK")
        ))
      } else {
        values$geocodes <- rbind(geo1, geo2)
      }
    })

    output$map <- renderLeaflet({
      geo1 <- values$geocodes[1, ]
      geo2 <- values$geocodes[2, ]

      leaflet() %>%
        addTiles() %>%
        setView(
          lng = mean(c(geo1[1], geo2[1])),
          lat = mean(c(geo1[2], geo2[2])),
          zoom = 6
        ) %>%
        addMarkers(lng = geo1[1], lat = geo1[2], label = input$word1) %>%
        addMarkers(lng = geo2[1], lat = geo2[2], label = input$word2) %>%
        addMeasure(position = "topright", primaryLengthUnit = "meters")
    })
  })
}
