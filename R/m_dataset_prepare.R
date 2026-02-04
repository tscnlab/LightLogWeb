# UI ------------------------------------------------------------------

UI_accordion_quality <- function(ns) {
  #second accordion panel with quality metrics
  accordion_panel(
    h3("Quality control"),
    value = "quality",
    selectInput(
      ns("123"),
      label = "Relevant variable",
      choices = "",
      # selected = metadata$variable
    ),
    textInput(
      ns("1234"),
      label = "Variable name",
      # value = metadata$variable_name
    ),
    textInput(
      ns("12345"),
      label = "Variable unit",
      # value = metadata$variable_unit
    ),
    p(
      actionButton(
        ns("123567"),
        "Save variable details",
        icon = icon("save"),
        class = "btn-primary"
      )
    )
  )
}

datasetDetailUI <- function(id) {
  ns <- NS(id)

  tagList(
  uiOutput(ns("dataset_detail")),
  accordion(
    multiple = FALSE,
    height = "100%",
    id = ns("prepare_accordion"),
    accordion_panel(value = "metadata",
                           h3("Metadata"),
                           metadataUI(ns("metadata"))
                    ),
    UI_accordion_quality(ns)
  )
  )

}

# Server ------------------------------------------------------------------

datasetDetailServer <- function(id,
                                datasets,
                                selected_dataset,
                                active_panel) {
  stopifnot(is.reactive(selected_dataset),
            is.reactivevalues(datasets),
            is.reactive(active_panel))
  moduleServer(id, function(input, output, session) {

    #check whether a dataset is selected
    observe({
      req(active_panel() == "prepare")
      no_dataset_modal(selected_dataset, session)
      })
    observe({
      removeModal()
    }) |> bindEvent(input$to_import)

    #create the ui of the dataset detail
    output$dataset_detail <- renderUI({
      req(selected_dataset())
     h4("Dataset: ", selected_dataset())
    })

    #collect the current dataset

    #metadata module
    # dataset <- reactive({datasets[[selected_dataset()]])

    metadataServer("metadata",
                   datasets,
                   selected_dataset,
                   ignoreInit = FALSE
                   )

    #add metadata back into dataset




  })
}

# Testapp ------------------------------------------------------------------

datasetDetail <- function(...) {
  ui <-
    page_navbar(
      title = h1("datasetDetail module"),
      nav_panel_hidden("Detail",
                       datasetDetailUI("Detail")
                       )
    )
  server <- function(input, output, session) {

    datasets <- reactiveValues()

    timer <- reactiveTimer(1000)

    observe(load_testdata(datasets, notifications = FALSE))

    datasetDetailServer("Detail",
                        active_panel = reactive("prepare"),
                        selected_dataset = reactive("sample.data.environment"),
                        datasets = datasets)
  }
  shinyApp(ui, server, ...)
  }
