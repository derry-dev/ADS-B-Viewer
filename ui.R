req <- c(
    "shiny",
    "data.table",
    "jsonlite",
    "DT",
    "leaflet",
    "RColorBrewer",
    "sp",
    "shinyWidgets",
    "shinyjs"
)
lapply(req, library, character.only = T)

sidebar <- div(
    class = "sidenav",
    actionBttn("btn_import", label = NULL, icon = icon("folder-open"), style = "stretch", size = "lg", block = T),
    actionBttn("btn_view", label = NULL, icon = icon("search-location"), style = "stretch", size = "lg", block = T),
    div(
        style = "margin-left: -1px",
        actionBttn("btn_filter", label = NULL, icon = icon("map-marked-alt"), style = "stretch", size = "lg", block = T)
    ),
    # actionBttn("btn_play", label = NULL, icon = icon("history"), style = "stretch", size = "lg", block = T),
    actionBttn("btn_camera", label = NULL, icon = icon("camera"), style = "stretch", size = "lg", block = T),
    div(
        style = "position: fixed; bottom: 0; width: 49.5px;",
        div(
            style = "margin-left: 2px",
            actionBttn("btn_clear", label = NULL, icon = icon("trash"), style = "stretch", size = "lg", block = T)
        ),
        actionBttn("btn_settings", label = NULL, icon = icon("cog"), style = "stretch", size = "lg", block = T),
        actionBttn("btn_question", label = NULL, icon = icon("question-circle"), style = "stretch", size = "lg", block = T)
    )
)

tracks_panel <- absolutePanel(
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
)

filter_panel <- absolutePanel(
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

spinner_wrapper <- conditionalPanel(
    condition="$('html').hasClass('shiny-busy')",
    div(id="loadmessage", uiOutput("spinner"))
)

fillPage(
    useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "spinner.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "bttn.min.css"),
    leafletOutput("map", height = "100%"),
    sidebar,
    hidden(
        tracks_panel,
        filter_panel,
        downloadButton("screenshot")
    ),
    spinner_wrapper
)
