# UI ------------------------------------------------------------------

summariesUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      fill = FALSE,
      # max_height = "200px",
      uiOutput(ns("gaps"), fill = TRUE),
      uiOutput(ns("irregular"), fill = TRUE),
      value_box(
        title = "IDs",
        value = textOutput(ns("n_participants")),
        showcase = icon("users"),
        theme = "text-secondary",
        class = "bg-light",
        textOutput(ns("n_participant_days"))
      ) |>
        tooltip2("Based on the extracted IDs. Days refer to all calendar dates for which there are timepoints, no matter how few."),
      value_box(
        title = "Dominant epoch",
        value = textOutput(ns("dominant_epoch")),
        showcase = icon("hourglass"),
        theme = "text-secondary",
        class = "bg-light",
        textOutput(ns("dominant_epoch_comment"))
      ) |> tooltip2("The most prominent interval between timepoints. If it varies between IDs, other dominant epochs will be listed."),
      value_box(
        title = "Data coverage",
        value = textOutput(ns("missingness")),
        showcase = icon("circle-notch"),
        theme = "text-warning",
        class = "bg-light",
        textOutput(ns("missingness_basis"))
      ) |> tooltip2("Percentage of data that is available from the full timeseries (full days). I.e., if there are data from 1pm on day 1 until 1pm on day 2 this indicator will show 50% available, as there is only 24 hours of data in the two relevant calendar dates. This does not necessarily indicate an issue, but is relevant for metrics that are calculated for calendar dates."),
      value_box(
        title = textOutput(ns("valid_days")),
        value = textOutput(ns("n_valid_participant_days")),
        showcase = icon("user-check"),
        theme = "text-primary",
        class = "bg-light",
        textOutput(ns("valid_participants_range"))
      ) |> tooltip2("Valid days are defined as days with more (non NA) data points than the user-defined threshold"),
    )
  )
}

# Server ------------------------------------------------------------------

summariesServer <- function(id,
                            datasets,
                            selected_dataset) {
  stopifnot(is.reactive(selected_dataset),
            is.reactivevalues(datasets)
            )
  moduleServer(id, function(input, output, session) {

    dataset <- reactive({
      req(!is.null(selected_dataset()))
      datasets[[selected_dataset()]]
    })

    overview <- reactive({
      req(dataset()$summaries$overview)
      dataset()$summaries$overview
    })

    #Participant summary
    output$n_participants <- renderText({
      overview()[1, "mean"][[1]]
    })

    output$n_participant_days <- renderText({
      req(overview())
      data <- overview()[2,]
      if(data[1,3] == data[1,4]){
        paste0(data[1,2],
               " days (",
               data[1,3],
               " per ID)"
        )
      } else {
      paste0(data[1,2],
             " days (min: ",
             data[1,3],
             ", max: ",
             data[1,4],
             ")"
             )
      }
    })

    epochs <- reactive({
      req(dataset()$summaries$dominantEpoch)
      dataset()$summaries$dominantEpoch |>
                         dplyr::count(dominant.epoch, sort = TRUE) |>
                         dplyr::pull(dominant.epoch)
      })

    output$dominant_epoch <- renderText({
      req(epochs())
      epochs()[1] |> format()
    })

    output$dominant_epoch_comment <- renderText({
      req(epochs())
      if(length(epochs()) > 1) {
        others <-
          epochs()[-1] |>
          format() |>
          unique() |>
          paste(collapse = ", ")
        paste("+", others)
      }
    })

    output$n_valid_participant_days <- renderText({
      req(overview())
      paste0(
        overview()[3,2],
             " days "
      )
      })

    output$valid_participants_range <- renderText({
      validate(
        need(dataset()$metadata$variable, "Please set and save a primary variable")
      )
      req(overview())
      if(overview()[3,3] ==
         overview()[3,4]) {
        paste0(overview()[3,3],
               " per ID")
      } else {
      paste0("from ",
             overview()[3,3],
             " to ",
             overview()[3,4],
             " per ID"
      ) }
      })

    output$valid_days <- renderText({
      paste0("Valid days (>",
             (1 - dataset()$metadata$threshold.missing) |> scales::percent(),
             ")")
    })

    output$gaps <- renderUI({
      theme <- ifelse(dataset()$summaries$has_gaps,
                      "text-danger", "text-success")
      value <- ifelse(dataset()$summaries$has_gaps,
                      "detected", "none")
      value_box(
        title = "Implicit gaps",
        value = value,
        showcase = icon("uncharted"),
        theme = theme,
        class = "bg-light",
      ) |>
        tooltip2("Missing timepoints based on a regular series from start to finish. Aggregation to a coarser interval can help, but a robust solution is filling in missing timepoints with `NA` values.")
    }) |> bindEvent(dataset()$summaries$has_gaps)

    output$irregular <- renderUI({
      theme <- ifelse(dataset()$summaries$has_irregulars,
                      "text-danger", "text-success")
      value <- ifelse(dataset()$summaries$has_irregulars,
                      "detected", "none")
      value_box(
        title = "Irregular data",
        value = value,
        showcase = icon("arrows-left-right-to-line"),
        theme = theme,
        class = "bg-light",
      )|>
        tooltip2("Timepoints that do not fall within the regular time series cause issues. Rounding timepoints or aggregation to a coarser interval can resolve this.")
    })|> bindEvent(dataset()$summaries$has_irregulars)

    #Preparing data for value boxes

    output$missingness <- renderText({
      (1 - overview()$mean[4]) |> scales::percent()
    })

    output$missingness_basis <- renderText({
      if(overview()$min[4] == overview()$max[4]) {
        paste("overall & per ID")
      } else {
      paste0("from ",
      (1 - overview()$max[4]) |> scales::percent(),
      " to ",
      (1 - overview()$min[4]) |> scales::percent(),
      " per ID")
      }
    })
  })
}
