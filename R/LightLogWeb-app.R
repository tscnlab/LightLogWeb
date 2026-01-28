#' Bring LightLogR to the web with shiny
#'
#' @returns Open a viewer with the shiny app
#' @export
#'
#' @examples
#' if(interactive()) {
#' LightLogWeb()
#' }
LightLogWeb <- function(...) {

  #setting the upload limit to 100 MB
  options(shiny.maxRequestSize=100*1024^2)
  #add a resource path to the www folder
  shiny::addResourcePath(
    "extr", system.file("app/www", package = "LightLogWeb"))
  # on.exit(shiny::removeResourcePath("extr"), add = TRUE)

  ui <- bslib::page_navbar(
    # App title ----
    title = shiny::h1("LightLogWeb"),
    footer = footer,
    selected = "Import",
    fillable = FALSE,
    bslib::nav_spacer(),
    bslib::nav_panel("Import",
                     bslib::layout_sidebar(
                       sidebar = datasetSidebarUI("datasets"),
                       importUI("import"),
                       datasetDetailUI("datasets")
                     )
    )
  )

  # Server ------------------------------------------------------------------

  #Server
  server <- function(input, output, session) {

    #allow reconnect
    session$allowReconnect(TRUE)

    #Import
    datasets <- shiny::reactiveValues()
    light <- importServer("import")
    datasetManagerServer("datasets", datasets)

    shiny::observeEvent(light$add_dataset(), {
      new_dataset <- light$add_dataset()
      shiny::req(new_dataset)
      dataset_name <- new_dataset$name

      if (is.null(dataset_name) || dataset_name == "") {
        shiny::showNotification("Please provide a dataset name before saving.", type = "error")
        return()
      }

      if (!is.null(datasets[[dataset_name]])) {
        shiny::showNotification("A dataset with this name already exists.", type = "error")
        return()
      }

      datasets[[dataset_name]] <- list(
        data = new_dataset$data,
        metadata = list(variable = NULL, variable_name = "", variable_unit = "")
      )
      shiny::showNotification("Dataset added to the library.", type = "message")
    })

    #close the waiting screen
    # waiter::waiter_hide()

  }

  # App ---------------------------------------------------------------------
  shiny::shinyApp(ui, server, ...)
}
