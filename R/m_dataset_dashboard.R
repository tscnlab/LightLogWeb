# UI ------------------------------------------------------------------

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
      preprocessingUI(ns("preprocessing"))
    ),
    nav_panel(
      title = "Plot",
      plotOutput(ns("plot")) |> shinycssloaders::withSpinner()
    ),
    nav_panel(
      title = "Summary Metrics",
      fluidRow(
      gt::gt_output(ns("summarytable")) |> shinycssloaders::withSpinner()
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
      datasets[[selected_dataset()]]$data_processed |>
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
      datasets[[selected_dataset()]]$data_processed
    })

    output$summarytable <- gt::render_gt({
      req(selected_dataset())
      datasets[[selected_dataset()]]$data_processed |>
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

    #preprocessing module
    preprocessingServer("preprocessing",
                   datasets,
                   selected_dataset
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
                       ),
    )
  server <- function(input, output, session) {

    datasets <- reactiveValues()
    selected_dataset <- reactiveVal("sample.data.environment")

    timer <- reactiveTimer(1000)

    observe(load_testdata(datasets, selected_dataset, notifications = FALSE),
            ) |>
      bindEvent(TRUE, once = TRUE)

    datasetDashboardServer("Dashboard",
                        active_panel = reactive("dashboard"),
                        selected_dataset = selected_dataset,
                        datasets = datasets)
  }
  shinyApp(ui, server, ...)
  }
