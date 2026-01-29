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
    id = "main_nav",
    footer = footer,
    selected = "Import",
    fillable = FALSE,
    sidebar = datasetSidebarUI("datasets"),
    bslib::nav_spacer(),
    bslib::nav_panel("Import",
                       importUI("import")
                     ),
    bslib::nav_panel("Prepare", value = "prepare",
                       datasetDetailUI("details")
                       ),
    bslib::nav_panel("Report",
                     reportUI("report")
                     )
  )

  # Server ------------------------------------------------------------------

  #Server
  server <- function(input, output, session) {

    #allow reconnect
    session$allowReconnect(TRUE)

    #Import
    light <- importServer("import")
    datasets <- shiny::reactiveValues()

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
        metadata = list(variable = NULL,
                        variable_name = "",
                        variable_unit = "",
                        tz = new_dataset$tz,
                        device = new_dataset$device),
        import_specs = new_dataset$import_specs,
        import_call = new_dataset$import_call
      )
      shiny::showNotification(
      shiny::p("Dataset ", shiny::strong(dataset_name), " added to the library."),
      type = "message")
    })

    #Dataset handling
    selected_dataset <- datasetManagerServer("datasets", datasets)

    #Dataset Detail
    datasetDetailServer("details",
                        datasets = datasets,
                        selected_dataset =selected_dataset,
                        active_panel = shiny::reactive(input$main_nav))

    reportServer("report", datasets)

    #close the waiting screen
    # waiter::waiter_hide()

    # UI navigation updates

    #when starting a new import
    shiny::observe({
      bslib::nav_select("main_nav", selected = "Import")
      bslib::accordion_panel_open("import-import_accordion", "import_specs")
    }) |> shiny::bindEvent(input$`datasets-import_newdata`,
                           input$`details-to_import`)

    #when data were loaded in
    shiny::observe({
      bslib::nav_select("main_nav", selected = "prepare")
    }) |> shiny::bindEvent(input$`import-add_dataset`,
                           input$`datasets-import_testdata`,
                           ignoreInit = TRUE
                           )

  }

  # App ---------------------------------------------------------------------
  shiny::shinyApp(ui, server, ...)
}
