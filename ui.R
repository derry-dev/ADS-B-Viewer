library(shiny)
library(data.table)
library(jsonlite)
library(DT)
library(leaflet)
library(RColorBrewer)
library(sp)
library(shinyWidgets)
library(shinyjs)

fillPage(
    useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    leafletOutput("map", height = "100%"),
    div(
        class = "sidenav",
        actionBttn("btn_import", label = NULL, icon = icon("file-import"), style = "stretch", size = "lg", block = T),
        actionBttn("btn_view", label = NULL, icon = icon("plane"), style = "stretch", size = "lg", block = T),
        actionBttn("btn_slider", label = NULL, icon = icon("sliders-h"), style = "stretch", size = "lg", block = T),
        div(
            style = "position: fixed; bottom: 0; width: 49.5px;",
            actionBttn("btn_settings", label = NULL, icon = icon("cog"), style = "stretch", size = "lg", block = T),
            actionBttn("btn_question", label = NULL, icon = icon("question-circle"), style = "stretch", size = "lg", block = T)
        )
    ),
    hidden(
        absolutePanel(
            id = "track_panel",
            top = 5,
            left = 52,
            draggable = T,
            wellPanel(
                style = "padding-bottom: 112px; min-width: 112px",
                div(
                    style = "position: absolute; top: 15px; left: 20px;",
                    h4("Imported Data")
                ),
                dataTableOutput("track_table")
            )
        ),
        absolutePanel(
            id = "selection_panel",
            top = 0,
            left = 49,
            height = "100%",
            div(
               class = "trackopt",
               pickerInput(
                   "track_dates",
                   "Select Dates",
                   NULL,
                   multiple = T,
                   options = list(`actions-box` = T, `live-search` = T),
                   width = "220px"
               ),
               pickerInput(
                   "track_callsigns",
                   "Select Callsigns",
                   NULL,
                   multiple = T,
                   options = list(`actions-box` = T, `live-search` = T),
                   width = "220px"
               ),
               actionBttn("track_update", "Update Tracks", style = "minimal", size = "sm", block = T)
            )
        )
    )
)
