
datasetDetailUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
  shiny::uiOutput(ns("dataset_detail")),
  shiny::selectInput(
    ns("variable_select"),
    label = "Relevant variable",
    choices = "",
    # selected = metadata$variable
  ),
  shiny::textInput(
    ns("variable_name"),
    label = "Variable name",
    # value = metadata$variable_name
  ),
  shiny::textInput(
    ns("variable_unit"),
    label = "Variable unit",
    # value = metadata$variable_unit
  ),
  shiny::p(
    shiny::actionButton(
      ns("save_variable"),
      "Save variable details",
      icon = shiny::icon("save"),
      class = "btn-primary"
    )
  )
  )

}

datasetDetailServer <- function(id,
                                datasets,
                                selected_dataset,
                                active_panel) {
  shiny::moduleServer(id, function(input, output, session) {

    #collect all dataset names
    shiny::observe({
      # browser()
      shiny::req(active_panel() == "prepare")
      if (is.null(selected_dataset())) {
        shiny::showModal(
          shiny::modalDialog(
            title = shiny::div(
              shiny::icon("cat", style = "font-size: 60px;"),
              shiny::icon("question", style = "font-size: 60px;"),
              shiny::icon("bowl-food", style = "font-size: 30px;"),
              ),
            easy_close = TRUE,
            shiny::strong("Missing dataset!"),
            shiny::p("No dataset is available/selected. Please import a dataset."
            ),
            shiny::actionButton(session$ns("to_import"), "Go to import",
                                icon = shiny::icon("file-import"),
                                width = "100%"),
            footer = NULL
            )
        )
      }
      })

    #create the ui of the dataset detail
    output$dataset_detail <- shiny::renderUI({
      req(selected_dataset())
     shiny::h4("Dataset: ", selected_dataset())
    })

    # #create a list of metadata
    # metadata <- rlang::`%||%`(
    #   dataset$metadata,
    #   list(variable = NULL, variable_name = "", variable_unit = "")
    # )

    #update relevant variable
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

  })
}
