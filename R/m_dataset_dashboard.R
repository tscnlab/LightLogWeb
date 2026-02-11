# UI ------------------------------------------------------------------

UI_preprocessing <- function(ns) {
  #second accordion panel with quality metrics
  card(
    h3("Quality control"),
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

datasetDashboardUI <- function(id) {
  ns <- NS(id)

  tagList(
    # bslib::page_fixed(
  uiOutput(ns("dataset_name")),
  summariesUI(ns("summaries")),
  # ),
  fluidRow(
  navset_card_pill(
    nav_panel(
      title = "Metadata",
      metadataUI(ns("metadata"))
    ),
    nav_panel(
      title = "Preprocessing",
      UI_preprocessing(ns)
    ),
    nav_panel(
      title = "Plot",
      plotOutput(ns("plot"))
    ),
    nav_panel(
      title = "Summary Metrics",
      fluidRow(
      gt::gt_output(ns("summarytable"))
      )
      ),
    nav_panel(
      title = "Raw Table",
      fluidRow(
      DT::dataTableOutput(ns("table"), height = "600px")
      )
    )
  )
  )
  )

}

# Server ------------------------------------------------------------------

datasetDashboardServer <- function(id,
                                datasets,
                                selected_dataset,
                                active_panel) {
  stopifnot(is.reactive(selected_dataset),
            is.reactivevalues(datasets),
            is.reactive(active_panel))
  moduleServer(id, function(input, output, session) {

    #check whether a dataset is selected
    observe({
      req(active_panel() == "dashboard")
      no_dataset_modal(selected_dataset, session)
      })
    observe({
      removeModal()
    }) |> bindEvent(input$to_import)

    #create the ui of the dataset heading
    output$dataset_name <- renderUI({
      req(selected_dataset())
     h5("Dataset: ", selected_dataset())
    })

    output$plot <- renderPlot({
      req(selected_dataset())
      datasets[[selected_dataset()]]$data |>
        gg_days(
          y.axis = !!rlang::sym(datasets[[selected_dataset()]]$metadata$variable),
                  aes_col = Id) |>
        gg_photoperiod(c(
          datasets[[selected_dataset()]]$metadata$latitude,
          datasets[[selected_dataset()]]$metadata$longitude
        ))
    })


    output$table <- DT::renderDataTable({
      req(selected_dataset())
      datasets[[selected_dataset()]]$data
    })

    output$summarytable <- gt::render_gt({
      req(selected_dataset())
      datasets[[selected_dataset()]]$data |>
        summary_table(
          Variable.colname = !!rlang::sym(datasets[[selected_dataset()]]$metadata$variable),
          coordinates = c(
            datasets[[selected_dataset()]]$metadata$latitude,
            datasets[[selected_dataset()]]$metadata$longitude
          ),
          color = "red",
          threshold.missing = datasets[[selected_dataset()]]$metadata$threshold.missing,
          Variable.label =
            paste0(datasets[[selected_dataset()]]$metadata$variable_name,
                   " (",
                   datasets[[selected_dataset()]]$metadata$variable_unit,
                   ")"),
          location = datasets[[selected_dataset()]]$metadata$country,
          site = datasets[[selected_dataset()]]$metadata$site,
        )
    })

    #metadata module
    metadataServer("metadata",
                   datasets,
                   selected_dataset,
                   ignoreInit = FALSE
                   )

    #valueboxes module
    summariesServer("summaries",
                    datasets,
                    selected_dataset)

  })
}

# Testapp ------------------------------------------------------------------

datasetDashboard <- function(...) {
  ui <-
    page_navbar(
      # theme = bs_theme(bootswatch = "cosmo"),
      title = h1("datasetDashboard module"),
      nav_panel_hidden("Dashboard",
                       datasetDashboardUI("Dashboard")
                       )
    )
  server <- function(input, output, session) {

    datasets <- reactiveValues()
    selected_dataset <- reactiveVal("sample.data.environment")

    timer <- reactiveTimer(1000)

    observe(load_testdata(datasets, selected_dataset, notifications = FALSE))

    datasetDashboardServer("Dashboard",
                        active_panel = reactive("dashboard"),
                        selected_dataset = selected_dataset,
                        datasets = datasets)
  }
  shinyApp(ui, server, ...)
  }
