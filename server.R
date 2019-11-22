import_dialog <- function() {
    modalDialog(
        div(
            style = "display: flex; justify-content: flex-start",
            prettyRadioButtons(
                "data_source", "Select Data Source",
                choiceNames = c("Raw JSON", "Database"),
                choiceValues = c("json", "db"),
                shape = "round",
                animation = "smooth",
                inline = T
            )
        ),
        uiOutput("data_import"),
        footer = NULL,
        size = "m",
        easyClose = T
    )
}

# test <- rbindlist(lapply(list.files("data", full.names=T)[1:10], function(i) as.data.table(fromJSON(i)$aircraft)), fill = T)

# leaflet(quakes) %>% addTiles() %>% addCircleMarkers(
#     radius = 5, stroke = FALSE, fillOpacity = 0.25
# )

server <- function(input, output, session) {
    
    onclick("btn_import", showModal(import_dialog()))
    onclick("btn_view", toggle("track_panel"))
    onclick("btn_slider", toggle("selection_panel"))
    
    output$map <- renderLeaflet({
        x <- leaflet(options = leafletOptions(zoomControl = F, preferCanvas = T)) %>%
            setView(lng = -1.85, lat = 50.81, zoom = 8)
        tile_providers <- list(
            `Esri Satellite` = "Esri.WorldImagery",
            `Esri Terrain` = "Esri.WorldTerrain",
            `Esri Relief` = "Esri.WorldShadedRelief",
            `Esri Physical` = "Esri.WorldPhysical",
            `Esri Ocean` = "Esri.OceanBasemap",
            `Esri Nat Geo` = "Esri.NatGeoWorldMap",
            `CartoDB Light` = "CartoDB.Positron",
            `CartoDB Light 2` = "CartoDB.PositronNoLabels",
            `CartoDB Dark` = "CartoDB.DarkMatter",
            `CartoDB Dark 2` = "CartoDB.DarkMatterNoLabels",
            `OSM Mapnik` = "OpenStreetMap.Mapnik",
            `OSM B&W` = "OpenStreetMap.BlackAndWhite"
        )
        for (i in 1:length(tile_providers)) {
            x <- x %>% addProviderTiles(providers[[tile_providers[[i]]]], options = providerTileOptions(noWrap = T), group = names(tile_providers)[i])
        }
        x <- x %>% addLayersControl(baseGroups = names(tile_providers), options = layersControlOptions(collapsed = T))
    })
    
    observeEvent(input$data_source, {
        if (input$data_source == "json") {
            output$data_import <- renderUI({
                tagList(
                    multiInput(
                        "imported_files", "Choose JSON Files",
                        choices = list.files("data", pattern = ".json"),
                        width = "100%"
                    ),
                    div(
                        style = "text-align: center",
                        actionButton("data_confirm", "Confirm")
                    )
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
    
}