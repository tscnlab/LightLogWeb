delete_dataset_modal <- function(selected_dataset, session) {
  showModal(
    modalDialog(
      title = "Are you certain you want to delete a dataset?",
      easyClose = TRUE,
      footer = modalButton("Keep the dataset as is"),
      p("Clicking the button below will delete the dataset ",
               strong(selected_dataset()),
               " without further prompting."
      ),
      actionButton(session$ns("delete_dataset_real"),
                          p("Delete ",
                                   strong(selected_dataset())
                          ),
                          icon = icon("trash"),
                          width = "100%"),
    )
  )
}

delete_dataset <- function(datasets, dataset_names, selected_dataset) {
  datasets[[selected_dataset()]] <- NULL
  removeModal()
  showNotification("Dataset deleted.", type = "warning")
  if (length(dataset_names()) == 0) {
    selected_dataset(NULL)
  } else {
    selected_dataset(dataset_names()[1])
  }
}
