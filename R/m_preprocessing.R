# UI ------------------------------------------------------------------

preprocessingUI <- function(id) {
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
               div("Save metadata details & reset preprocessing", icon("circle-right")),
               # icon = ,
               class = "btn-primary",
               width = "100%"
             ) |> tooltip2("this will overwrite the current metadata onto the dataset & reset all preprocessing")),
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

preprocessingServer <- function(id, datasets, selected_dataset, ignoreInit = TRUE) {
  stopifnot(is.reactive(selected_dataset),
            is.reactivevalues(datasets)
  )
  moduleServer(id, function(input, output, session) {

    ds <- reactive({
      req(!is.null(selected_dataset()))
      datasets[[selected_dataset()]]
    })

  })
}
