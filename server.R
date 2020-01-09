import_dialog <- function() {
    modalDialog(
        div(
            style = "text-align: center;",
            radioGroupButtons(
                "data_source", "Select Data Source",
                choiceNames = c("Raw JSON", "Database"),
                choiceValues = c("json", "db"),
                selected = "json"
            ),
            uiOutput("data_import")
        ),
        footer = NULL,
        size = "s",
        easyClose = T
    )
}

setting_dialog <- function() {
    modalDialog(
        div(style = "text-align: center;", "Work in Progress"),
        footer = NULL,
        size = "s",
        easyClose = T
    )
}

help_dialog <- function() {
    modalDialog(
        div(style = "text-align: center;", "Work in Progress"),
        footer = NULL,
        size = "s",
        easyClose = T
    )
}

server <- function(input, output, session) {
    
    # Sidebar button toggling
    
    onclick("btn_import", showModal(import_dialog()))
    
    onclick("btn_view", toggle("track_panel"))
    
    onclick("btn_filter", toggle("selection_panel"))
    
    onclick("btn_camera", click("screenshot"))
    
    observeEvent(input$btn_clear, leafletProxy("map") %>% clearGroup("Tracks"))
    
    onclick("btn_settings", showModal(setting_dialog()))
    
    onclick("btn_question", showModal(help_dialog()))
    
    # Loading spinner
    
    output$spinner <- renderUI({
        htmltools::HTML('<div class="loader"></div>')
    })
    
    # Render map
    
    map <- reactiveValues(
        dat = {
            x <- leaflet(options = leafletOptions(zoomControl = F, preferCanvas = T)) %>%
                setView(lng = -1.85, lat = 50.81, zoom = 8)
            tile_providers <- list(
                `Esri Satellite` = "Esri.WorldImagery",
                `Esri Terrain` = "Esri.WorldTerrain",
                `Esri Ocean` = "Esri.OceanBasemap",
                `CartoDB Light` = "CartoDB.Positron",
                `CartoDB Light 2` = "CartoDB.PositronNoLabels",
                `CartoDB Dark` = "CartoDB.DarkMatter",
                `CartoDB Dark 2` = "CartoDB.DarkMatterNoLabels",
                `OSM Mapnik` = "OpenStreetMap.Mapnik"
            )
            for (i in 1:length(tile_providers)) {
                x <- x %>% addProviderTiles(providers[[tile_providers[[i]]]], options = providerTileOptions(noWrap = T), group = names(tile_providers)[i])
            }
            x <- x %>% addLayersControl(baseGroups = names(tile_providers), options = layersControlOptions(collapsed = T))
        }
    )
    
    output$map <- renderLeaflet(map$dat)
    
    # Import dialog logic
    
    observeEvent(input$data_source, {
        if (input$data_source == "json") {
            output$data_import <- renderUI({
                tagList(
                    div(
                        style = "padding-left: 25px;",
                        pickerInput(
                            "imported_files", "Choose JSON Files",
                            choices = list.files("data", pattern = ".json"),
                            multiple = T,
                            options = list(`actions-box` = T, `live-search` = T),
                            width = "220px"
                        )
                    ),
                    actionButton("data_confirm", "Confirm")
                )
            })
        } else if (input$data_source == "db") {
            output$data_import <- renderUI({
                "Option Not Available"
            })
        }
    })
    
    observeEvent(input$data_confirm, {
        removeModal()
        updatePickerInput(
            session, "track_dates",
            choices = unique(gsub("^([0-9]{4}-[0-9]{2}-[0-9]{2})\\s.*$", "\\1", json()$Timestamp)),
            selected = unique(gsub("^([0-9]{4}-[0-9]{2}-[0-9]{2})\\s.*$", "\\1", json()$Timestamp))
        )
    })
    
    # Display imported data table
    
    observeEvent(input$data_confirm, {
        output$track_table <- renderDataTable(
            datatable(
                json(),
                rownames = F,
                selection = "none",
                options = list(
                    pageLength = 10,
                    lengthChange = F,
                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                    scrollX = T
                )
            )
        )
    })
    
    # Track filtering
    
    observeEvent(input$track_dates, {
        updatePickerInput(
            session, "track_callsigns",
            choices = unique(json()[grepl(paste0("^", input$track_dates, " .*$"), Timestamp)]$flight),
            selected = unique(json()[grepl(paste0("^", input$track_dates, " .*$"), Timestamp)]$flight)[1:11]
        )
    })
    
    json <- eventReactive(input$data_confirm, {
        rbindlist(lapply(file.path("data", input$imported_files), function(i) as.data.table(fromJSON(i)$aircraft)), fill = T)
    })
    
    json_filtered <- eventReactive(input$track_update, {
        json()[grepl(paste0("^", input$track_dates, " .*$"), Timestamp) & flight %in% input$track_callsigns]
    })
    
    observe({
        output$track_table <- renderDataTable(
            datatable(
                json(),
                rownames = F,
                selection = "none",
                options = list(
                    pageLength = 10,
                    lengthChange = F,
                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                    scrollX = T
                )
            )
        )
    })
    
    lab <- reactive({
        if (dim(json_filtered())[1] != 0 & dim(json_filtered())[2] != 0) {
            sprintf("
              <b>Callsign</b>: %s <font size='1'><b>Squawk</b> %s</font><br/>
              <b>Timestamp</b>: %s<br/>
              <b>Alt (baro)</b>: %s <b>(geom)</b>: %s<br/>
              <b>IAS</b>: %s <b>TAS</b>: %s<br/>
              <b>Mag Heading</b>: %s<br/>
              <b>Roll</b>: %s<br/>
              ",
                    json_filtered()$flight, json_filtered()$squawk,
                    json_filtered()$Timestamp,
                    json_filtered()$alt_baro, json_filtered()$alt_geom,
                    json_filtered()$ias, json_filtered()$tas,
                    json_filtered()$mag_heading,
                    json_filtered()$roll
            ) %>% lapply(htmltools::HTML)
        } else {
            NULL
        }
    })
    
    observeEvent(json_filtered(),{
        if (!is.null(json_filtered())) {
            
            pal <- colorFactor(palette = "Spectral", domain = input$track_callsigns)
            
            p <- leafletProxy("map") %>% clearGroup("Tracks")
            
            p %>% addCircleMarkers(
                data = json_filtered(),
                lng = ~lon,
                lat = ~lat,
                color = ~pal(flight),
                label = lab(),
                labelOptions = labelOptions(textsize = "13px", direction = "auto"),
                radius = 5,
                stroke = F,
                fillOpacity = 0.5,
                group = "Tracks"
            )
            
        }
    })
    
    # Screenshot
    
    output$screenshot <- downloadHandler(
        filename = "map.png",
        
        content = function(file) {
            mapshot(map$dat, file = file, delay = 0)
        }
    )
    
}
