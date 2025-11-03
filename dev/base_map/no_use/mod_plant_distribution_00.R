##### modules/mod_plant_distribution.R #####


admin_all <- get_saitama_all()


mod_plant_distribution_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- 種リスト選択 ----
    output$species <- renderUI({
      req(input$type)
      species_list <- sort(unique(plant_dist[[input$type]]))
      selectInput(ns("selected"), "種を選択：", choices = species_list)
    })

    # ---- フィルタリング ----
    filtered_data <- reactive({
      req(input$selected)
      filter(plant_dist, !!sym(input$type) == input$selected)
    })


    # ---- 地図初期描画 ----
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles(group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addPolygons(
          data = watershed, color = "#0099FF", weight = 1,
          fillOpacity = 0.1, group = "Watershed", dashArray = "10, 6"
        ) %>%
        addPolylines(
          data = stream, color = "blue", weight = 2, group = "River"
        ) %>%
        addPolygons(
          data = admin, color = "red", weight = 0.75,
          fill = FALSE, group = "Admin"
        ) %>%
        addControl(
          html = "<img src='north_arrow.png' style='width:50px;'>",
          position = "topright"
        ) %>%
        addScaleBar(position = "bottomleft") %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "Satellite"),
          overlayGroups = c("Watershed", "River", "Admin", "Plant Distribution", "Highlight"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        fitBounds(
          lng1 = saitama_all["xmin"],
          lat1 = saitama_all["ymin"],
          lng2 = saitama_all["xmax"],
          lat2 = saitama_all["ymax"]
        )
    })


    # # ---- 地図初期描画 ----
    # output$map <- renderLeaflet({
    #   # bboxを算出（saitama_allが存在すれば使用、なければフォールバック）
    #   if (exists("saitama_all", envir = .GlobalEnv) &&
    #     inherits(get("saitama_all", envir = .GlobalEnv), "sf")) {
    #     bbox <- sf::st_bbox(get("saitama_all", envir = .GlobalEnv))
    #     lng1 <- bbox["xmin"]
    #     lat1 <- bbox["ymin"]
    #     lng2 <- bbox["xmax"]
    #     lat2 <- bbox["ymax"]
    #   } else {
    #     # フォールバック範囲（埼玉県全体）
    #     lng1 <- 138.8
    #     lat1 <- 35.0
    #     lng2 <- 140.2
    #     lat2 <- 36.3
    #   }
    #
    #   leaflet() %>%
    #     addTiles(group = "OpenStreetMap") %>%
    #     addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    #     addPolygons(
    #       data = watershed, color = "#0099FF", weight = 1,
    #       fillOpacity = 0.1, group = "Watershed", dashArray = "10, 6"
    #     ) %>%
    #     addPolylines(
    #       data = stream, color = "blue", weight = 2, group = "River"
    #     ) %>%
    #     addPolygons(
    #       data = admin, color = "red", weight = 0.75,
    #       fill = FALSE, group = "Admin"
    #     ) %>%
    #     addControl(
    #       html = "<img src='north_arrow.png' style='width:50px;'>",
    #       position = "topright"
    #     ) %>%
    #     addScaleBar(position = "bottomleft") %>%
    #     addLayersControl(
    #       baseGroups = c("OpenStreetMap", "Satellite"),
    #       overlayGroups = c("Watershed", "River", "Admin", "Plant Distribution", "Highlight"),
    #       options = layersControlOptions(collapsed = FALSE)
    #     ) %>%
    #     fitBounds(
    #       lng1 = saitama_all["xmin"],
    #       lat1 = saitama_all["ymin"],
    #       lng2 = saitama_all["xmax"],
    #       lat2 = saitama_all["ymax"]
    #     )
    #   # setView(lng = 139.4, lat = 35.9, zoom = 9) #後で削除
    # })

    # ---- 地図更新（フィルター変更時）----
    observe({
      data <- filtered_data()
      if (nrow(data) == 0) {
        return()
      }

      leafletProxy(ns("map"), session) %>%
        clearGroup("Plant Distribution") %>%
        removeControl("legend") %>%
        addCircleMarkers(
          data = data,
          color = ~ pal_decade(decade),
          radius = 4,
          stroke = FALSE,
          fillOpacity = 0.7,
          layerId = ~occurrenceID,
          label = ~ paste0(
            "<b>", vernacularName, "</b><br>",
            scientificName, "<br>",
            "観察年: ", year, "<br>",
            "記録者: ", recordedBy
          ) %>% lapply(htmltools::HTML),
          group = "Plant Distribution"
        ) %>%
        addLegend(
          "bottomright",
          pal = pal_decade,
          values = data$decade,
          title = "観察年代",
          opacity = 1,
          layerId = "legend"
        )
    })

    # ---- ✅ 埼玉県全域表示ボタン ----
    observeEvent(input$zoom_all, {
      # helpers/data_load.R で読み込んだ saitama_all() 関数を使用
      if (exists("saitama_all", envir = .GlobalEnv)) {
        admin_all <- saitama_all # 関数saitama_all()をカッコをとってオブジェクトsaitama_allに変更
      } else {
        admin_all <- NULL
      }

      if (inherits(admin_all, "sf")) {
        bbox <- sf::st_bbox(admin_all)
        leafletProxy(ns("map"), session) %>%
          clearGroup("Highlight") %>%
          clearGroup("Plant Distribution") %>%
          addPolygons(
            data = admin_all,
            color = "blue",
            weight = 2,
            fill = FALSE,
            group = "Admin All"
          ) %>%
          fitBounds(
            lng1 = bbox["xmin"],
            lat1 = bbox["ymin"],
            lng2 = bbox["xmax"],
            lat2 = bbox["ymax"]
          )
      } else {
        leafletProxy(ns("map"), session) %>%
          fitBounds(138.8, 35.0, 140.2, 36.3)
      }
    })



    # ---- テーブル描画 ----
    output$table <- DT::renderDataTable(
      {
        filtered_data() %>% st_drop_geometry()
      },
      selection = "single"
    )

    # ---- テーブル → 地図の連動 ----
    observeEvent(input$table_rows_selected, {
      selected_row <- input$table_rows_selected
      if (length(selected_row) == 0) {
        return()
      }

      data <- filtered_data()
      selected_point <- data[selected_row, ]
      coords <- st_coordinates(selected_point)

      leafletProxy(ns("map")) %>%
        clearGroup("Highlight") %>%
        addCircleMarkers(
          data = selected_point,
          lng = coords[, 1], lat = coords[, 2],
          color = "red", radius = 8, fillOpacity = 1,
          group = "Highlight"
        ) %>%
        flyTo(lng = coords[, 1], lat = coords[, 2], zoom = 14)
    })

    # ---- 地図 → テーブルの連動 ----
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (is.null(click$id)) {
        return()
      }

      data <- filtered_data()
      row_index <- which(data$occurrenceID == click$id)
      if (length(row_index) == 0) {
        return()
      }

      DT::dataTableProxy(ns("table")) %>%
        DT::selectRows(row_index)
    })
  })
}
