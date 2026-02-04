# UI ------------------------------------------------------------------

metadataUI <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_column_wrap(
    bslib::card(
      h4("Primary variable"),
      selectInput(
        ns("variable"),
        label = "Relevant variable",
        choices = "",
      )|> tooltip2("please choose a variable from the dataset"),
      textInput(
        ns("variable_name"),
        label = "Variable name")|>
        tooltip2("This name will be used in plots and tables"),
      textInput(
        ns("variable_unit"),
        label = "Variable unit"
      )|> tooltip2("The variable unit may be displayed alongside the variable name. No calculations or corrections will be performed based on the unit"),
      numericInput(
        ns("variable_factor"),
        label = "Correction factor",
        value = 1
      ) |> tooltip2("You can set a correction factor that will be applied to the variable. Note that all outputs will contain this factor."),
      numericInput(
        ns("variable_offset"),
        label = "Variable offset",
        value = 0
      )|> tooltip2("You can set an offset that will be applied to the variable. Note that all outputs will contain this offset."),
    )|> tooltip2("Set information about the variable you want to focus on in the analyses?"),
    bslib::card(
      h4("Location"),
      bslib::layout_column_wrap(fill = FALSE,
      numericInput(ns("latitude"), "Latitude", value = 0,  min = -90, max = 90),
      numericInput(ns("longitude"), "Longitude", value = 0, min = -90, max = 90),
      ) |> tooltip2("Enter latitude and longitude in decimal form. Will be used in labels and for photoperiod and in location plots."),
      bslib::layout_column_wrap(fill = FALSE,
        textInput(ns("country"), "Country") |>
          tooltip2("Enter the country of measurement. Will be used in labels and in location plots"),
        textInput(ns("site"), "Site") |>
          tooltip2("Enter the site or city of measurement. Will be used in labels")),
      selectizeInput(ns("tz"), "Time zone", choices = OlsonNames(), selected = "UTC") |>
        tooltip2("Select the time zone of data collection. Overwrites the current time zone in the dataset! Set to a common time zone (like 'UTC') when merging data from two different time zones.")
      ) |>tooltip2("Location information will be used in various functions, both for programmatic and labeling purposes."),
    bslib::card(fill = FALSE, full_screen = TRUE,
      h4("Metadata table"),
      verbatimTextOutput(ns("metadata_table"))
      )|>
      tooltip2(
        "Contains all the medata"
      )),

    bslib::layout_column_wrap(
      actionButton(
        ns("save_metadata"),
        "Save metadata details",
        icon = icon("download"),
        class = "btn-primary"
      ) |> tooltip2("this will overwrite the current metadata onto the dataset"),
      actionButton(
        ns("restore_metadata"),
        "Restore metadata details",
        icon = icon("upload"),
        class = "btn-secondary"
      ) |> tooltip2("this will restore the metadata from the dataset (cannot restore overwritten metdata)"),
    )
  )
}

# Server ------------------------------------------------------------------

metadata.Variables <- c("variable", "variable_name", "variable_unit",
                        "variable_factor", "variable_offset", "latitude",
                        "longitude", "country", "site", "tz") |> rlang::set_names()

metadataServer <- function(id, ds) {
  moduleServer(id, function(input, output, session) {

    #print output
    output$metadata_table <- renderPrint({
      req(ds$get())
      ds$get()$metadata
    })

    #fill variable selection with dataset variables and update inputs based on metadata
    observe({
      dataset <- ds$get()$data
      md <- ds$get()$metadata
      updateSelectInput(inputId = "variable",
                        choices = names(dataset),
                        selected = md$variable)
      purrr::walk(metadata.Variables[c(2:3,8:10)],
                  \(x) updateTextInput(inputId = x, value = md[[x]]))
      purrr::walk(metadata.Variables[4:7],
                  \(x) updateTextInput(inputId = x, value = md[[x]]))
    }) |> bindEvent(ds$get())

    #save metadata
    observe({
      req(ds$get())
      dataset <- ds$get()
      md <- purrr::map(metadata.Variables,\(x) {input[[x]]})
      dataset$metadata[names(md)] <- md
      ds$set(dataset)
      showNotification("Variable details saved.", type = "message")
    }) |> bindEvent(input$save_metadata)

    #restore metadata
    observe({
      updateSelectInput()

    }) |> bindEvent(input$restore_metadata)
  })
}
