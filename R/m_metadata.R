# UI ------------------------------------------------------------------

metadataUI <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_column_wrap(
      bslib::card(
        card_header(h4("Primary variable"))|> tooltip2(
          "Set information about the variable you want to focus on in the analyses?"
        ),
        card_body(
        selectInput(
          ns("variable"),
          label = "Relevant variable",
          choices = "",
          width = "100%"
        ) |> tooltip2("please choose a variable from the dataset"),
        textInput(ns("variable_name"), label = "Variable name", width = "100%") |>
          tooltip2("This name will be used in plots and tables"),
        textInput(ns("variable_unit"), label = "Variable unit", width = "100%") |> tooltip2(
          "The variable unit may be displayed alongside the variable name. No calculations or corrections will be performed based on the unit"
        ),
        numericInput(
          ns("variable_factor"),
          label = "Correction factor",
          value = 1,
          width = "100%"
        ) |> tooltip2(
          "You can set a correction factor that will be applied to the variable. Note that all outputs will contain this factor."
        ),
        numericInput(
          ns("variable_offset"),
          label = "Variable offset",
          value = 0,
          width = "100%"
        ) |> tooltip2(
          "You can set an offset that will be applied to the variable. Note that all outputs will contain this offset."
        ),
      )
      ),
      bslib::card(
        card_header(h4("Location"))|> tooltip2(
          "Location information will be used in various functions, both for programmatic and labeling purposes."
        ),
        card_body(
        bslib::layout_column_wrap(
          fill = FALSE,
          numericInput(
            ns("latitude"),
            "Latitude",
            value = 0,
            min = -90,
            max = 90,
            width = "100%"
          ),
          numericInput(
            ns("longitude"),
            "Longitude",
            value = 0,
            min = -90,
            max = 90,
            width = "100%"
          ),
        )|> tooltip2(
          "Enter latitude and longitude in decimal form. Will be used in labels and for photoperiod and in location plots."
        ) ,
        leaflet::leafletOutput(ns("map"), height = 200),
        bslib::layout_column_wrap(
          fill = FALSE,
          textInput(ns("country"), "Country", width = "100%") |>
            tooltip2(
              "Enter the country of measurement. Will be used in labels and in location plots"
            ),
          textInput(ns("site"), "Site", width = "100%") |>
            tooltip2("Enter the site or city of measurement. Will be used in labels")
        ),
        selectizeInput(
          ns("tz"),
          "Time zone",
          choices = OlsonNames(),
          width = "100%",
          selected = "UTC"
        ) |>
          tooltip2(
            "Select the time zone of data collection. Overwrites the current time zone in the dataset! Set to a common time zone (like 'UTC') when merging data from two different time zones."
          )
      )
      ),
      bslib::card(
        card_header(
                    h4("Metadata table"))|>
          tooltip2("Contains all the medata"),
        card_body(tableOutput(ns("metadata_table"))
        )
      ),
    ),
    shiny::fluidRow(
    shiny::column(8,
      actionButton(
        ns("save_metadata"),
        div("Save metadata details ", icon("circle-right")),
        # icon = ,
        class = "btn-primary",
        width = "100%"
      ) |> tooltip2("this will overwrite the current metadata onto the dataset")),
    column(4,
      actionButton(
        ns("restore_metadata"),
        "Restore metadata details",
        icon = icon("circle-left"),
        class = "btn-secondary",
        width = "100%"
      ) |> tooltip2(
        "this will restore the metadata from the dataset (cannot restore overwritten metdata)"
      ),
    ))
  )
}

# Server ------------------------------------------------------------------

metadata.Variables <- c("variable", "variable_name", "variable_unit",
                        "variable_factor", "variable_offset", "latitude",
                        "longitude", "country", "site", "tz") |> rlang::set_names()

metadataServer <- function(id, datasets, selected_dataset, ignoreInit = TRUE) {
  stopifnot(is.reactive(selected_dataset),
            is.reactivevalues(datasets)
            )
  moduleServer(id, function(input, output, session) {

    ds <- reactive({
      req(!is.null(selected_dataset()))
      datasets[[selected_dataset()]]
    })

    #map output
    output$map <- leaflet::renderLeaflet({
      validate(
        need(input$latitude, "Please provide a latitude"),
        need(input$longitude, "Please provide a longitude")
      )
      lng <- input$longitude
      lat <- input$latitude
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::setView(lng, lat, zoom = 4) |>
        leaflet::addMiniMap(width = 100, height = 100) |>
        leaflet::addAwesomeMarkers(lng, lat,
                                   popup = format_coordinates(c(lat, lng))
                                   )
    })

    #print output
    output$metadata_table <- renderTable({
      ds()$metadata |>
        purrr::map_chr(as.character) |>
        tibble::enframe("Metadata variable", "Value")
    })

    #fill variable selection with dataset variables and update inputs based on metadata
    observe({
      dataset <- ds()$data
      md <- ds()$metadata
      updateSelectInput(inputId = "variable",
                        choices = names(dataset),
                        selected = md$variable)
      purrr::walk(metadata.Variables[c(2:3,8:10)],
                  \(x) updateTextInput(inputId = x, value = md[[x]]))
      purrr::walk(metadata.Variables[4:7],
                  \(x) updateTextInput(inputId = x, value = md[[x]]))
      })

    #save metadata
    observe({
      dataset <- ds()
      md <- purrr::map(metadata.Variables,\(x) {input[[x]]})
      dataset$metadata[names(md)] <- md
      datasets[[selected_dataset()]] <- dataset
      showNotification("Variable details saved.", type = "message")
    }) |> bindEvent(input$save_metadata)

    #restore metadata
    observe({
      updateSelectInput(inputId = "variable",
                        selected = ds()$metadata$variable)
      purrr::walk(metadata.Variables[c(2:3,8:10)],
                  \(x) updateTextInput(inputId = x, value = ds()$metadata[[x]]))
      purrr::walk(metadata.Variables[4:7],
                  \(x) updateTextInput(inputId = x, value = ds()$metadata[[x]]))
    }) |> bindEvent(input$restore_metadata)
  })
}
