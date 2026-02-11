# UI ------------------------------------------------------------------

datasetSidebarUI <- function(id) {
  ns <- NS(id)

  sidebar(
    h5("Imported datasets"),
    uiOutput(ns("dataset_list")),
    hr(),
    h5("Add datasets"),
    actionButton(
      ns("import_newdata"),
      "Start new import" |>
        tooltip2("Takes you to the import module"),
      icon = icon("file-import"),
    ),
    actionButton(
      ns("import_testdata"),
      "Load test data" |>
        tooltip2("Testdata are two one week long datasets, one containing participant data, one environmental data (collected on the rooftop in the same general area as the participant)"),
      icon = icon("file-medical"),
    ),
    hr(),
    h5("Danger zone"),
    actionButton(
      ns("rename_dataset"),
      "Rename dataset"|>
        tooltip2("This button will allow you to rename the current dataset"),
      icon = icon("file-signature"),
    ),
    actionButton(
      ns("merge_dataset"),
      "Merge dataset" |>
        tooltip2("This button will allow you to merge the current dataset with another"),
      icon = icon("code-pull-request"),
      class = "btn-outline-warning"
    ),
    actionButton(
      ns("delete_dataset"),
      "Delete dataset" |>
        tooltip2("Clicking this button will delete the selected dataset permanently!"),
      icon = icon("trash"),
      class = "btn-outline-danger"
    ),
    open = "desktop",
    width = 300
  )
}

# Server ------------------------------------------------------------------

datasetManagerServer <- function(id, datasets, newest_set) {
  moduleServer(id, function(input, output, session) {

    # Imported datasets -------------

    #create a hull for selected dataset names
    selected_dataset <- reactiveVal(NULL)

    #collect all dataset names and make certain a valid dataset is chosen
    dataset_names <- reactive({
      collect_dataset_names(datasets, selected_dataset)
    })

    #create the dataset list
    output$dataset_list <- renderUI({
      #initial state
      if (length(dataset_names()) == 0) {
        return(p("No datasets imported yet."))
      }

      #create the list
      radioButtons(
        session$ns("dataset_select"),
        label = NULL,
        choices = dataset_names(),
        selected = selected_dataset(),
        width = "100%"
      )
    })

    #update the selected dataset based on the radio buttons
    observeEvent(input$dataset_select, {
      selected_dataset(input$dataset_select)
    }, ignoreInit = TRUE)

    # Add datasets -------------

    #load testdata
    observe({
      load_testdata(datasets, selected_dataset)
    }) |> bindEvent(input$import_testdata)

    #update name with new addition
    observe({
     selected_dataset(newest_set())
    }) |> bindEvent(newest_set())

    # Danger zone -------------

    #rename dataset
    observe({
      req(selected_dataset())
      rename_dataset_modal(datasets, selected_dataset, session)
    }) |> bindEvent(input$rename_dataset)

    observe({
      rename_dataset(datasets, selected_dataset, dataset_names, input)
    }) |> bindEvent(input$rename_dataset_real, ignoreInit = TRUE)

    #remove dataset
    observe({
      req(selected_dataset())
      delete_dataset_modal(selected_dataset, session)
    }) |> bindEvent(input$delete_dataset, ignoreInit = TRUE)

    observe(
      delete_dataset(datasets, dataset_names, selected_dataset)
     ) |> bindEvent(input$delete_dataset_real, ignoreInit = TRUE)

    # Return -------------

    selected_dataset

  })
}
