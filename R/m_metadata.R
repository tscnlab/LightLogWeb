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
        layout_column_wrap(
        textInput(ns("variable_name"), label = "Variable name", width = "100%") |>
          tooltip2("This name will be used in plots and tables"),
        textInput(ns("variable_unit"), label = "Variable unit", width = "100%") |> tooltip2(
          "The variable unit may be displayed alongside the variable name. No calculations or corrections will be performed based on the unit"
        )
        ),
        layout_column_wrap(
        numericInput(
          ns("variable_min"),
          label = "Variable minimum",
          value = 0,
          width = "100%"
        ) |> tooltip2(
          "Choose the lowest sensible value that the variable can take."
        ),
        numericInput(
          ns("variable_max"),
          label = "Variable maximum",
          value = 10^4,
          width = "100%"
        ) |> tooltip2(
          "Choose the highest sensible value that the variable can take."
        ),
      ),
        layout_column_wrap(
        selectizeInput(
          ns("variable_scaling"),
          label = "Variable scaling",
          choices = c(`linear` = "identity",
                      `logarithmic` = "log10",
                      `logarithmic with zeros` = "symlog"),
          width = "100%"
        ) |> tooltip2(
          "What describes the variable best in terms of distribution?"
        ),
        numericInput(
          ns("threshold.missing"),
          label = "Daily acceptable missingness",
          value = 0.2,
          min = 0,
          max = 1,
          width = "100%"
        ) |> tooltip2(
          "How much data can be missing within a given day, before that day is considered invalid and should be discarded for analysis?"
        ),
      ),
        layout_column_wrap(
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
                        "variable_factor", "variable_offset", "variable_min",
                        "variable_scaling", "threshold.missing",
                        "variable_max", "latitude",
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

    #save metadata
    observe({
      dataset <- ds()
      md <- purrr::map(metadata.Variables,\(x) {input[[x]]})
      purrr::imap(md, \(x, idx) {
        if(!identical(dataset$metadata[[idx]], x)) {
        dataset$metadata[[idx]] <- x
        }
      }
                  )
      showNotification("Metadata details saved.", type = "message")
    }) |> bindEvent(input$save_metadata)

    #adjust time zone if necessary
    observe({
      dataset <- ds()
      if(dataset$metadata$tz != lubridate::tz(dataset$data_processed$Datetime)){
      dataset$data_processed <-
        dataset$data_processed |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::where(
              lubridate::is.POSIXct),
            \(x) lubridate::force_tz(x, dataset$metadata$tz)
            )
          )
      }
      if(dataset$metadata$tz != lubridate::tz(dataset$data$Datetime)){
      dataset$data <-
        dataset$data |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::where(
              lubridate::is.POSIXct),
            \(x) lubridate::force_tz(x, dataset$metadata$tz)
            )
          )
      }
    }) |> bindEvent(ds()$data, ds()$metadata$tz, ignoreInit = TRUE)

    #restore metadata
    observe({
      updateSelectInput(inputId = "variable",
                        choices = setdiff(names(ds()$data),
                                          c("Datetime",
                                            dplyr::group_vars(ds()$data))),
                        selected = ds()$metadata$variable)
      updateSelectInput(inputId = "variable_scaling",
                        selected = ds()$metadata$variable_scaling)
      purrr::walk(metadata.Variables[-1],
                  \(x) updateTextInput(inputId = x, value = ds()$metadata[[x]]))
      updateSelectInput(inputId = "tz",
                        selected = ds()$metadata$tz)
      showNotification("Metadata details restored.", type = "message")
    }) |> bindEvent(input$restore_metadata, ds()$data, ds()$metadata)

  #adjust time zone if set
  # observe({
  #   dataset <- ds()
  #   dataset$data <-
  #     dataset$data |>
  #     dplyr::mutate(Datetime = lubridate::force_tz(Datetime, dataset$metadata$tz))
  # }) |> bindEvent(ds()$data, ds()$metadata$tz, ignoreInit = TRUE)

  #adjust coordinates if changing
  observe({
    dataset <- ds()
    req(dataset$metadata$latitude, dataset$metadata$longitude)
    dataset$summaries$overview <-
      summary_overview(dataset$data_processed,
                       !!rlang::sym(dataset$metadata$variable),
                       coordinates = c(dataset$metadata$latitude, dataset$metadata$longitude),
                       threshold.missing =
                         dataset$metadata$threshold.missing
      )
    # #add photoperiod to data
    # dataset$data <-
    #   dataset$data |>
    #   add_photoperiod(c(dataset$metadata$latitude, dataset$metadata$longitude),
    #                   overwrite = TRUE)
    }) |> bindEvent(ds()$metadata$latitude, ds()$metadata$longitude, ds()$metadata$threshold.missing, ignoreInit = TRUE)

  })
}
