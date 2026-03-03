# UI ------------------------------------------------------------------

preprocessingUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      input_switch(ns("switch_interval"), h5("Adjust interval")) |> tooltip2(
        "Set an arbitrary interval for the data. Should be less granular then the imported data. Datapoints within the new interval will be averaged (with NA's removed)"
      ),
      layout_column_wrap(
      numericInput(ns("new_interval"), "New interval", min = 0.5, value = 1),
      shinyWidgets::sliderTextInput(ns("new_unit"), "Interval unit",
                     choices = c(Seconds = "secs",
                                 Minutes = "mins",
                                 Hours = "hours",
                                 Days = "days",
                                 Weeks = "weeks",
                                 Months = "months"
                                 ), selected = "mins",
                     grid = TRUE),


    )
    ),
    p(
      input_task_button(
        ns("preproc"),
        span(strong("Apply preprocessing")),
        icon = icon("cog"),
        width = "50%",
        class = "btn-primary btn-lg",
        style = "width: 50%;"
      ),
      style = "text-align:center;"
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

    #apply preprocessing
    observe({
      unit <- paste(input$new_interval, input$new_unit)
      dataset <- ds()

      if(input$switch_interval) {
        dataset$data_processed <-
          dataset$data_processed |>
          aggregate_Datetime(unit = unit,
                             numeric.handler = \(x) mean(x, na.rm = TRUE))
      }

      dataset$summaries$dominantEpoch <-
        dataset$data_processed |> dominant_epoch()

      dataset$summaries$has_gaps <-
        dataset$data |> has_gaps()

      dataset$summaries$has_irregulars <-
        dataset$data |> has_irregulars()
    }) |> bindEvent(input$preproc, ignoreInit = TRUE)

  })
}
