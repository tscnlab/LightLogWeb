# UI ------------------------------------------------------------------

UI_accordion_specification <- function(ns) {
  #first accordion panel with import specs
  accordion_panel(
    h3("Import specification"),
    value = "import_specs",
    layout_column_wrap(
      heights_equal = "row",
      fillable = FALSE,
      min_height = "300px",
      #first column of specs
      tagList(
        h4("Mandatory"),
        #Choose files
        fileInput(
          ns("file"),
          span(
            bsicons::bs_icon("1-circle"),
            strong("Choose file(s)")
          ) |>
            tooltip2(
              "Please choose only files from one type of device, with the same data format (e.g., columns), and the same time zone of data collection."
            ),
          multiple = TRUE,
          accept = c(".txt", ".csv"),
          width = "100%"
        ),
        #Choose device
        selectizeInput(
          ns("device"),
          span(
            bsicons::bs_icon("2-circle"),
            strong("Select the ",
                          a("device type",
                                   href = "https://tscnlab.github.io/LightLogR/reference/import_Dataset.html#devices",
                                   target = "_blank"),
                          "of data collection")
          ),
          choices = c("", supported_devices()),
          options = list(
            placeholder = "Select a device...",
            onInitialize = I("function() {this.removeOption('');}")
          ),
          width = "100%"
        ),
        #Choose time zone
        selectInput(
          ns("tz"),
          span(
            bsicons::bs_icon("3-circle"),
            strong("Select the time zone of data collection")
          ),
          choices = OlsonNames(),
          selected = "UTC",
          width = "100%"
        ),
        #Set a name for the dataset
        textInput(
          ns("dataset_name"),
          span(
            bsicons::bs_icon("4-circle"),
            strong("Choose a unique name for the dataset (internal reference)")
          ),
        )
      ),
      #second column of specs
      tagList(
        h4("Optional"),
        #Choose version
        selectizeInput(
          ns("version"),
          span(
            bsicons::bs_icon("5-circle"),
            strong("Specify a file format version for your device"),
          ),
          choices = c(""),
          options = list(
            placeholder = "Select a device before choosing a version"
          ),
          width = "100%"
        ) |> tooltip2(textOutput(ns("version_select"))),
        #Choose not before
        dateInput(
          ns("not_before"),
          span(
            bsicons::bs_icon("6-circle"),
            weekstart = 1,
            strong("Define a cut-off date, prior to which no data will be imported."),
          ),
          value = "2001-01-01"
        ),
        #Choose options
        checkboxGroupInput(
          ns("options"),
          span(
            bsicons::bs_icon("7-circle"),
            strong("Select the following options if applicable")
          ),
          choiceNames = list(
            tooltip2("Handle daylight savings (DST) Jumps", "If active data collection crossed a daylight savings time, AND it is not handled automatically by the device, this option shifts data by one hour forward or backward, after a daylight savings jump."),
            tooltip2("Remove duplicates", "If the files contain duplicate entries, they can be removed with this option. Otherwise, import will throw an error.")
          ),
          choiceValues = list(
            "dst_jumps",
            "remove_duplicates"
          )
        ),
        radioButtons(
          ns("id"),
          span(
            bsicons::bs_icon("8-circle"),
            strong("Participant IDs")) |> tooltip2("If the files contain a column `Id`, neither option will be used."),
          inline = TRUE,
          width = "100%",
          choiceNames = list(
            tooltip2("Automated handling", "An automatically shortened file name is used as an identifier."),
            tooltip2("Set a global ID manually", "Allows you to set an individual ID."),
            tooltip2("Extract ID from file name", "Allows you to define a regular expression to extract portion of the filename")
          ),
          choiceValues = list("automated", "manual", "extract"))
      ),
      #third column of specs
      tagList(
        h4("Conditional"),
        p("This section will show conditional options when needed"),
        conditionalPanel(
          condition = "input.device == 'VEET'",
          ns = ns,
          selectizeInput(
            inputId = ns("veet_modality"),
            label   = "Specify the VEET modality you would like to import",
            choices = c("Ambient light sensor, ALS" = "ALS",
                        "Inertial measurement unit, IMU" = "IMU",
                        "Information, INF" = "INF",
                        "Spectral sensor, PHO" = "PHO",
                        "Time of flight, TOF" = "TOF"

            ),
            width = "100%"
          )
        ),
        conditionalPanel(
          condition = "input.id == 'manual'",
          ns = ns,
          textInput(
            inputId = ns("Id_manual"),
            label   = "Specify the global ID",
            placeholder = "Give me a string",
            value = "Participant",
            updateOn = "blur",
            width = "100%"
          )
        ),
        conditionalPanel(
          condition = "input.id == 'extract'",
          ns = ns,
          textInput(
            inputId = ns("Id_extract"),
            label   = "Specify the regular expression for ID extraction",
            placeholder = "Give me a regular expression",
            value = ".*",
            updateOn = "blur",
            width = "100%"
          ) |>
            tooltip2("LLMs are exceptionally good at creating regular expressions, provided they get a few sample file names and a direction.")
        ),
      )),
    layout_column_wrap(
      #show file names and number
      value_box(
        "Number of files",
        value = textOutput(ns("n_files")),
        showcase = bsicons::bs_icon("journals"),
        theme = value_box_theme(bg =  "#d3d3d350"),
        textOutput(ns("filenames"))
      ),
      #show extracted ids
      value_box(
        span("Number of IDs") |>
          tooltip2("Based on the prior settings and file names (irrelevant if `Id` column is present in the data"),
        value = textOutput(ns("n_ids")),
        showcase = bsicons::bs_icon("search"),
        theme = value_box_theme(bg =  "#d3d3d350"),
        textOutput(ns("pattern"))
      )
    ),
    #import button
    p(
      input_task_button(
        ns("import"),
        span(strong("Import")),
        icon = icon("file-import"),
        width = "50%",
        class = "btn-primary btn-lg",
        style = "width: 50%;"
      ),
      style = "text-align:center;"
    ),
  )
}

UI_accordion_summary <- function(ns) {
  accordion_panel(
    h3("Import summary"),
    value = "import_summary",
    layout_column_wrap(
      card(
        card_header("Import message", container = h4),
        verbatimTextOutput(ns("import_msg")),
        min_height = "400px"
      ),
      card(
        card_header("Overview Plot", container = h4),
        plotOutput(ns("plot_overview")),
        min_height = "400px"
      )
    ),
    p(
      input_task_button(
        ns("add_dataset"),
        span(strong("Add dataset to library (and proceed to metadata)")),
        icon = icon("database"),
        width = "50%",
        class = "btn-primary btn-lg"
      ),
      style = "text-align:center;"
    ),
    card(
      card_header("Imported table", container = h4) |>
        tooltip2("This table shows the first and last 50 rows of the imported data"),
      gt::gt_output(ns("import_table"))
    )
  )
}

importUI <- function(id) {
  ns <- NS(id)

  tagList(
    #Import accordion
    accordion(
      multiple = FALSE,
      id = ns("import_accordion"),
      UI_accordion_specification(ns),
      UI_accordion_summary(ns)
    )
  )
}

# Server ------------------------------------------------------------------

importServer <- function(id) {

    moduleServer(id, function(input, output, session) {

      # Import specs -------------

      #update selectizeInput for file version choices
      observe(
        updateSelectizeInput(
          session, inputId = "version", choices = get_versions(input$device)
        )) |> bindEvent(input$device, ignoreInit = TRUE)

      #tooltip2 on device file version
      output$version_select <- renderText({
        if(input$device == "") {
          "Please select a device first"
        } else get_version_description(input$device, input$version)
      })

      #update to dataset name
      observe(
        updateTextInput(session,
                               "dataset_name",
                               value = create_dataset_name(input$device)
      )) |> bindEvent(input$device, ignoreInit = TRUE)

      # Value boxes -------------

      #number of files
      output$n_files <- renderText({
        if(is.null(input$file)) return("no files provided")
        paste0(input$file %>% nrow())
      })

      #filenames
      output$filenames <- renderText({
        input$file$name |> tools::file_path_sans_ext() |> paste0(collapse = ",\n")
      })

      #Ids to be used
      Id_preview <- reactiveVal()

      observe({
        req(input$file)
        if(input$id == "extract") req(input$Id_extract)
        Ids <-
          set_Id_preview(
            input$id, input$Id_manual, input$Id_extract, input$file$name
            )
        Id_preview(Ids)
      })

      #pattern to capture Ids
      output$pattern <- renderText({
        req(input$file)
        if(length(Id_preview()) == 0 ||
           any(Id_preview() == ""))
          return("no ID specification possible, please adjust settings")
        Id_preview() |> unique() |> paste0(collapse = ",\n")
      })

      #number of Ids pre import
      output$n_ids <- renderText({
        if(is.null(input$file)) return("no files provided")
        paste0(Id_preview() |> unique() |> length())
      })

      # Import -------------

      #change the filenames to the original name
      new_names <- reactive({
        req(input$file)
        rename_files(input$file)
      }) |>  bindEvent(input$file)

      # Import handling:
      # - capture the console output from import_Dataset() for the summary panel
      # - return the imported data for the table and overview plot
      import_result <- reactive({
        req(input$device,
                   input$file,
                   input$device != "",
                   input$dataset_name != "")

        import_data(input, Id_preview, new_names)
      }) |> bindEvent(input$import)

      # Import messaging -------------

      #Import notifications
      observe({
        import_notification(input$file, input$device, input$tz, input$dataset_name)
      }) |>
        bindEvent(input$import)

      #Switch to summary after import
      observe({
        accordion_panel_set("import_accordion", "import_summary")
      }) |> bindEvent(import_result())

      #Import Modal
      observe({
        req(import_result()$data)
        import_modal_successfull(import_result()$data)
      }) |>
        bindEvent(import_result()$data |> is.data.frame())

      # Import summary -------------

      #Import table
      output$import_table <-  gt::render_gt({
        gt::gt_preview(import_result()$data, top_n = 50, bottom_n = 50) |>
          gt::opt_interactive(
            use_search = TRUE,
            use_filters = TRUE, use_resizers = TRUE,
            use_highlight = TRUE
          )
      })

      #Import overview plot
      output$plot_overview <- renderPlot({
        import_result()$data %>% gg_overview()
      })

      #Import print
      output$import_msg <- renderText({
        paste(import_result()$msg, collapse = "\n")
      })

      # Return dataset -------------

      #choose a variable of interest
      observe({

        import_add_notification(import_result)

        req(import_result()$data,
            input$dataset_name,
            input$device,
            input$tz)

        showModal(
          modalDialog(
            title = "Please choose a primary variable for analysis from the dataset",
            easyClose = TRUE,
            selectizeInput(session$ns("variable"),
                           "Primary variable",
                           choices = setdiff(names(import_result()$data),
                                             c("Id", "Datetime", "file.name")),
                           selected = "MEDI",
                           width = "100%"
                           ),
            p("The primary variable can be changed later on",
            ),
            actionButton(session$ns("add_variable"),
                         ("Select this variable"),
                         class = "btn-primary btn-lg",
                         icon = icon("check"),
                         width = "100%"),
          )
        )

      }) |> bindEvent(input$add_dataset)

      observe(removeModal()) |> bindEvent(input$add_variable)

      #collect the imported data and send it to the next higher module
      add_dataset <- reactive({
        list(
          name = input$dataset_name,
          data = import_result()$data,
          device = input$device,
          tz = input$tz,
          variable = input$variable
        )
      }) |> bindEvent(input$add_variable)

      #return
      list(add_dataset = add_dataset)

    })
  }
