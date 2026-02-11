# UI ------------------------------------------------------------------

metadataUI <- function(id) {
  ns <- NS(id)
  tagList(
    # fluidRow(
    layout_column_wrap(
      card(
        h5("Primary variable") |> tooltip2(
          "Set information about the variable you want to focus on in the analyses?"
        ),
        # card_body(
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
      ),
      card(
        h5("Location") |>  tooltip2(
          "Location information will be used in various functions, both for programmatic and labeling purposes."
        ),
        # card_body(
        selectizeInput(
            ns("tz"),
            "Time zone",
            choices = OlsonNames(),
            width = "100%",
            selected = "UTC"
          ) |>
            tooltip2(
              "Select the time zone of data collection. Overwrites the current time zone in the dataset! Set to a common time zone (like 'UTC') when merging data from two different time zones."
            ),
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
      # )
      ),
      card(
        h5("Metadata table") |> tooltip2("Shows the current medata. Only what is listed here is relevant for calculations. Change options by setting the relevant fields and clicking the 'Save metadata details' button."),
        # card_body(
          tableOutput(ns("metadata_table"))
        # )
      )
    # )
    ),
      shiny::fluidRow(
        column(width = 8,
      actionButton(
        ns("save_metadata"),
        div("Save metadata details ", icon("circle-right")),
        # icon = ,
        class = "btn-primary",
        width = "100%"
      ) |> tooltip2("this will overwrite the current metadata onto the dataset")),
      column(width = 4,
      actionButton(
        ns("restore_metadata"),
        "Restore metadata details",
        icon = icon("circle-left"),
        class = "btn-secondary",
        width = "100%"
      ) |> tooltip2(
        "this will restore the metadata from the dataset (cannot restore overwritten metdata)"
    )
      )
  )
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
        reactiveValuesToList() |>
        purrr::map_chr(as.character) |>
        tibble::enframe("Metadata variable", "Value")
    })

    #fill variable selection with dataset variables and update inputs based on metadata
    observe({
      dataset <- ds()$data
      md <- ds()$metadata
      updateSelectInput(inputId = "variable",
                        choices = setdiff(names(dataset),
                                          c("Datetime",
                                            dplyr::group_vars(dataset))),
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
      purrr::imap(md, \(x, idx) {
        dataset$metadata[[idx]] <- x
      }
                  )
      showNotification("Metadata details saved.", type = "message")
    }) |> bindEvent(input$save_metadata)

    #restore metadata
    observe({
      updateSelectInput(inputId = "variable",
                        selected = ds()$metadata$variable)
      purrr::walk(metadata.Variables[c(2:3,8:10)],
                  \(x) updateTextInput(inputId = x, value = ds()$metadata[[x]]))
      purrr::walk(metadata.Variables[4:7],
                  \(x) updateTextInput(inputId = x, value = ds()$metadata[[x]]))
      showNotification("Metadata details restored.", type = "message")
    }) |> bindEvent(input$restore_metadata)

  #adjust time zone if set
  observe({
    dataset <- ds()
    dataset$data <-
      dataset$data |>
      dplyr::mutate(Datetime = lubridate::force_tz(Datetime, dataset$metadata$tz))
  }) |> bindEvent(ds()$data, ds()$metadata$tz, ignoreInit = TRUE)

  #adjust coordinates if changing
  observe({
    dataset <- ds()
    req(dataset$metadata$latitude, dataset$metadata$longitude)
    dataset$summaries$overview <-
      summary_overview(dataset$data,
                       !!rlang::sym(dataset$metadata$variable),
                       threshold.missing =
                         dataset$metadata$threshold.missing
      )
    }) |> bindEvent(ds()$metadata$latitude, ds()$metadata$longitude, ignoreInit = TRUE)

  })
}
