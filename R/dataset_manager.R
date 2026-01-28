datasetSidebarUI <- function(id) {
  ns <- shiny::NS(id)

  bslib::sidebar(
    shiny::h5("Imported datasets"),
    shiny::uiOutput(ns("dataset_list")),
    shiny::hr(),
    shiny::h5("Add datasets"),
    shiny::actionButton(
      ns("import_newdata"),
      "Start new import" |>
        tooltip("Takes you to the import module"),
      icon = shiny::icon("file-import"),
    ),
    shiny::actionButton(
      ns("import_testdata"),
      "Load test data" |>
        tooltip("Testdata are two one week long datasets, one containing participant data, one environmental data (collected on the rooftop in the same general area as the participant)"),
      icon = shiny::icon("file-medical"),
    ),
    shiny::hr(),
    shiny::h5("Danger zone"),
    shiny::actionButton(
      ns("rename_dataset"),
      "Rename dataset"|>
        tooltip("This button will allow you to rename the current dataset"),
      icon = shiny::icon("file-signature"),
    ),
    shiny::actionButton(
      ns("merge_dataset"),
      "Merge dataset" |>
        tooltip("This button will allow you to merge the current dataset with another"),
      icon = shiny::icon("code-merge"),
      class = "btn-outline-warning"
    ),
    shiny::actionButton(
      ns("delete_dataset"),
      "Delete dataset" |>
        tooltip("Clicking this button will delete the selected dataset permanently! No additional warning will be given."),
      icon = shiny::icon("trash"),
      class = "btn-outline-danger"
    ),
    open = "desktop",
    width = 300
  )
}

datasetManagerServer <- function(id, datasets) {
  shiny::moduleServer(id, function(input, output, session) {

    # Imported datasets -------------

    #create a hull for selected dataset names
    selected_dataset <- shiny::reactiveVal(NULL)

    #collect all dataset names
    dataset_names <- shiny::reactive({
      names_datasets <-
        shiny::reactiveValuesToList(datasets) |>
        purrr::map_lgl(is.null)
      names_datasets |> subset(subset = !names_datasets) |> names()
    })

    #if dataset_names updates, make certain a valid dataset name is chosen
    shiny::observeEvent(dataset_names(), {
      if (length(dataset_names()) == 0) {
        selected_dataset(NULL)
      } else if (is.null(selected_dataset()) || !selected_dataset() %in% dataset_names()) {
        selected_dataset(dataset_names()[1])
      }
    })

    #create the dataset list
    output$dataset_list <- shiny::renderUI({
      #initial state
      if (length(dataset_names()) == 0) {
        return(shiny::p("No datasets imported yet."))
      }

      #create the list
      shiny::radioButtons(
        session$ns("dataset_select"),
        label = NULL,
        choices = dataset_names(),
        selected = selected_dataset(),
        width = "100%"
      )
    })

    #update the selected dataset based on the radio buttons
    shiny::observeEvent(input$dataset_select, {
      selected_dataset(input$dataset_select)
    }, ignoreInit = TRUE)

    # Add datasets -------------

    shiny::observe({


    }) |> shiny::bindEvent(input$import_newdata)

    # Danger zone -------------

    #remove dataset
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

    # Return -------------

    selected_dataset

  })
}
