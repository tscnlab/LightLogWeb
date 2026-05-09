#' Bring LightLogR to the web with shiny
#'
#' @param ... Arguments passed to [shinyApp()]
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
  ops <-
    options(shiny.maxRequestSize=100*1024^2,
            spinner.caption = "Calculating..."
          )
  on.exit(options(ops))
  #add a resource path to the www folder
  addResourcePath(
    "extr", system.file("app/www", package = "LightLogWeb"))
  # on.exit(removeResourcePath("extr"), add = TRUE)

  ui <- page_navbar(
    header = tags$head(
      tags$link(rel = "stylesheet", href = "extr/windows-98.css")
    ),
    # App title ----
    title = h1("LightLogWeb "),
    id = "main_nav",
    # footer = footer,
    selected = "Import",
    # fillable = FALSE,
    sidebar = datasetSidebarUI("datasets"),
    # nav_spacer(),
    nav_panel("Import",
                       importUI("import")
                     ),
    nav_panel("Dashboard", value = "dashboard",
                       datasetDashboardUI("dashboard")
                       )
  )

  # Server ------------------------------------------------------------------

  #Server
  server <- function(input, output, session) {

    #allow reconnect
    session$allowReconnect(TRUE)

    #Import
    light <- importServer("import")
    datasets <- reactiveValues()
    newest_set <- reactiveVal(NULL)

    observe({
      adding_dataset(datasets, light$add_dataset)
      newest_set(light$add_dataset()$name)
    }) |> bindEvent(light$add_dataset())

    #Dataset handling
    selected_dataset <- datasetManagerServer("datasets", datasets, newest_set)

    #Dataset Dashboard
    datasetDashboardServer("dashboard",
                        datasets = datasets,
                        selected_dataset =selected_dataset,
                        active_panel = reactive(input$main_nav))

    #close the waiting screen
    # waiter::waiter_hide()

    # UI navigation updates

    #when starting a new import
    observe({
      nav_select("main_nav", selected = "Import")
      accordion_panel_open("import-import_accordion", "import_specs")
    }) |> bindEvent(input$`datasets-import_newdata`,
                           input$`dashboard-to_import`)

    #when testdata were loaded in
    observe({
      nav_select("main_nav", selected = "dashboard")
    }) |> bindEvent(input$`datasets-import_testdata`,
                           ignoreInit = TRUE
                           )

  }

  # App ---------------------------------------------------------------------
  shinyApp(ui, server, ...)
}
