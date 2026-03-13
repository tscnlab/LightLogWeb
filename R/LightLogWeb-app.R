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
    options(shiny.maxRequestSize = 100 * 1024^2,
            spinner.caption = "Calculating..."
    )
  on.exit(options(ops))
  #add a resource path to the www folder
  addResourcePath(
    "extr", system.file("app/www", package = "LightLogWeb"))
  # on.exit(removeResourcePath("extr"), add = TRUE)

  ui <- page_navbar(
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

    dataset_pages <- reactiveVal(list())
    next_page_id <- reactiveVal(1L)

    observe({
      current_datasets <- names(reactiveValuesToList(datasets))
      pages <- dataset_pages()
      page_diff <- reconcile_dataset_pages(
        pages = pages,
        dataset_names = current_datasets,
        selected_dataset = selected_dataset()
      )

      if (!is.null(page_diff$renamed)) {
        page <- pages[[page_diff$renamed$from]]
        page$dataset_name(page_diff$renamed$to)
        pages[[page_diff$renamed$to]] <- page
        pages[[page_diff$renamed$from]] <- NULL
        page_diff$removed <- setdiff(page_diff$removed, page_diff$renamed$from)
        page_diff$added <- setdiff(page_diff$added, page_diff$renamed$to)
      }

      if (length(page_diff$removed) > 0) {
        purrr::walk(page_diff$removed, function(dataset_name) {
          nav_remove(
            id = "main_nav",
            target = pages[[dataset_name]]$nav_value,
            session = session
          )
          pages[[dataset_name]] <- NULL
        })
      }

      if (length(page_diff$added) > 0) {
        purrr::walk(page_diff$added, function(dataset_name) {
          page_id <- next_page_id()
          next_page_id(page_id + 1L)
          page <- new_dataset_page(dataset_name = dataset_name, id = page_id)

          nav_insert(
            id = "main_nav",
            nav = nav_panel_hidden(
              title = dataset_name,
              value = page$nav_value,
              datasetDashboardUI(page$module_id)
            ),
            target = "Import",
            position = "after",
            session = session
          )

          datasetDashboardServer(
            id = page$module_id,
            datasets = datasets,
            selected_dataset = reactive(page$dataset_name())
          )

          pages[[dataset_name]] <- page
        })
      }

      dataset_pages(pages)
    })

    observe({
      req(selected_dataset())
      req(selected_dataset() %in% names(dataset_pages()))
      nav_select(
        id = "main_nav",
        selected = dataset_pages()[[selected_dataset()]]$nav_value,
        session = session
      )
    })

    #close the waiting screen
    # waiter::waiter_hide()

    # UI navigation updates

    #when starting a new import
    observe({
      nav_select("main_nav", selected = "Import")
      accordion_panel_open("import-import_accordion", "import_specs")
    }) |> bindEvent(input$`datasets-import_newdata`)

  }

  # App ---------------------------------------------------------------------
  shinyApp(ui, server, ...)
}
