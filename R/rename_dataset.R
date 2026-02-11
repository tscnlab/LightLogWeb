rename_dataset_modal <- function(dataset, selected_dataset, session){
  showModal(
    modalDialog(
      title = "Rename the dataset",
      easyClose = TRUE,
      footer = modalButton("Keep the dataset as is"),
      p("Please choose a new name for the dataset",
               strong(selected_dataset()),
               ":"
      ),
      textInput(session$ns("rename_name"),
                       "New name:",
                       value = "",
                       width = "100%",
                       placeholder = "Enter a new, unique name",
                       updateOn = "blur"),
      actionButton(session$ns("rename_dataset_real"),
                          "Rename",
                          class = "btn-primary",
                          icon = icon("file-signature"),
                          width = "100%"),
    )
  )
}

rename_dataset <- function(datasets, selected_dataset, dataset_names, input){
  if(input$rename_name == "" || is.na(input$rename_name)) {
    showNotification(
      "Please set a valid name for the dataset",
      type = "error"
    )
    return()
  } else if(input$rename_name %in% dataset_names()) {
    showNotification(
      "Chosen name is already part of the datasets. Please choose a unique name",
      type = "error"
    )
    return()
  }

  datasets[[input$rename_name]] <- datasets[[selected_dataset()]]
  datasets[[input$rename_name]]$metadata$ds.name <- input$rename_name
  datasets[[selected_dataset()]] <- NULL

  removeModal()
  showNotification(
    div("Dataset renamed from:",
               strong(selected_dataset()),
               "to",
               strong(input$rename_name)),
    type = "warning")
  if (length(dataset_names()) == 0) {
    selected_dataset(NULL)
  } else {
    selected_dataset(input$rename_name)
  }

}
