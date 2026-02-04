no_dataset_modal <- function(selected_dataset, session){
  if (is.null(selected_dataset())) {
    showModal(
      modalDialog(
        title = div(
          icon("cat", style = "font-size: 60px;"),
          icon("question", style = "font-size: 60px;"),
          icon("bowl-food", style = "font-size: 30px;"),
        ),
        easyClose = TRUE,
        strong("Missing dataset!"),
        p("No dataset is available/selected. Please import a dataset."
        ),
        actionButton(session$ns("to_import"), "Buckle up and go to import",
                            icon = icon("file-import"),
                            width = "100%"),
        footer = NULL
      )
    )
  }
}
