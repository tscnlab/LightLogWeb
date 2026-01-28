datasetSidebarUI <- function(id) {
  ns <- shiny::NS(id)

  bslib::sidebar(
    title = "Imported datasets",
    shiny::uiOutput(ns("dataset_list")),
    open = "desktop",
    width = 300
  )
}

datasetDetailUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(ns("dataset_detail"))
}

datasetManagerServer <- function(id, datasets) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_dataset <- shiny::reactiveVal(NULL)

    dataset_names <- shiny::reactive({
      names(shiny::reactiveValuesToList(datasets))
    })

    shiny::observeEvent(dataset_names(), {
      if (length(dataset_names()) == 0) {
        selected_dataset(NULL)
      } else if (is.null(selected_dataset()) || !selected_dataset() %in% dataset_names()) {
        selected_dataset(dataset_names()[1])
      }
    })

    output$dataset_list <- shiny::renderUI({
      if (length(dataset_names()) == 0) {
        return(shiny::p("No datasets imported yet."))
      }

      shiny::radioButtons(
        session$ns("dataset_select"),
        label = NULL,
        choices = dataset_names(),
        selected = selected_dataset(),
        width = "100%"
      )
    })

    shiny::observeEvent(input$dataset_select, {
      selected_dataset(input$dataset_select)
    }, ignoreInit = TRUE)

    output$dataset_detail <- shiny::renderUI({
      if (is.null(selected_dataset())) {
        return(
          bslib::card(
            bslib::card_header("Dataset overview", container = htmltools::h4),
            shiny::p("Select a dataset from the sidebar to manage it.")
          )
        )
      }

      dataset <- datasets[[selected_dataset()]]
      if (is.null(dataset)) {
        return(
          bslib::card(
            bslib::card_header("Dataset overview", container = htmltools::h4),
            shiny::p("The selected dataset is no longer available.")
          )
        )
      }

      metadata <- rlang::`%||%`(
        dataset$metadata,
        list(variable = NULL, variable_name = "", variable_unit = "")
      )

      bslib::card(
        bslib::card_header("Dataset overview", container = htmltools::h4),
        shiny::h4(selected_dataset()),
        bslib::navset_pill(
          bslib::nav_panel(
            "Adjust data",
            shiny::p("Prepare or clean the dataset here.")
          ),
          bslib::nav_panel(
            "Visualise",
            shiny::p("Explore the dataset through visual summaries.")
          ),
          bslib::nav_panel(
            "Metrics",
            shiny::p("Calculate metrics for the selected dataset.")
          )
        ),
        shiny::hr(),
        shiny::selectInput(
          session$ns("variable_select"),
          label = "Relevant variable",
          choices = names(dataset$data),
          selected = metadata$variable
        ),
        shiny::textInput(
          session$ns("variable_name"),
          label = "Variable name",
          value = metadata$variable_name
        ),
        shiny::textInput(
          session$ns("variable_unit"),
          label = "Variable unit",
          value = metadata$variable_unit
        ),
        shiny::p(
          shiny::actionButton(
            session$ns("save_variable"),
            "Save variable details",
            icon = shiny::icon("save"),
            class = "btn-primary"
          ),
          shiny::actionButton(
            session$ns("delete_dataset"),
            "Delete dataset",
            icon = shiny::icon("trash"),
            class = "btn-outline-danger"
          )
        )
      )
    })

    shiny::observeEvent(input$save_variable, {
      shiny::req(selected_dataset())
      dataset <- datasets[[selected_dataset()]]
      shiny::req(dataset)

      dataset$metadata <- list(
        variable = input$variable_select,
        variable_name = input$variable_name,
        variable_unit = input$variable_unit
      )
      datasets[[selected_dataset()]] <- dataset
      shiny::showNotification("Variable details saved.", type = "message")
    })

    shiny::observeEvent(input$delete_dataset, {
      shiny::req(selected_dataset())
      datasets[[selected_dataset()]] <- NULL
      shiny::showNotification("Dataset deleted.", type = "warning")
      if (length(dataset_names()) == 0) {
        selected_dataset(NULL)
      } else {
        selected_dataset(dataset_names()[1])
      }
    })
  })
}
