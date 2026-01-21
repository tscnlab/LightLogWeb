importUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    #Import accordion
    bslib::accordion(
      multiple = FALSE,
      #first accordion panel with import specs
      bslib::accordion_panel(
        shiny::h3("Import specification"),
        value = "import_specs",
        bslib::layout_column_wrap(
          heights_equal = "row",
          fillable = FALSE,
          min_height = "300px",
          #first column of specs
          shiny::tagList(
            shiny::h4("Mandatory"),
            #Choose files
            shiny::fileInput(
              ns("file"),
              shiny::span(
                bsicons::bs_icon("1-circle"),
                shiny::strong("Choose file(s)")
              ) |>
                tooltip(
                  "Please choose only files from one type of device, with the same data format (e.g., columns), and the same time zone of data collection."
                ),
              multiple = TRUE,
              accept = c(".txt", ".csv"),
              width = "100%"
            ),
            #Choose device
            shiny::selectizeInput(
              ns("device"),
              shiny::span(
                bsicons::bs_icon("2-circle"),
                shiny::strong("Select the ",
                              shiny::a("device type",
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
            shiny::selectInput(
              ns("tz"),
              shiny::span(
                bsicons::bs_icon("3-circle"),
                shiny::strong("Select the time zone of data collection")
              ),
              choices = OlsonNames(),
              selected = "UTC",
              width = "100%"
            ),
          ),
          #second column of specs
          shiny::tagList(
          shiny::h4("Optional"),
            #Choose version
            shiny::selectizeInput(
              ns("version"),
              shiny::span(
                bsicons::bs_icon("4-circle"),
                shiny::strong("Specify a file format version for your device"),
              ),
              choices = c(""),
              options = list(
                placeholder = "Select a device before choosing a version"
              ),
              width = "100%"
            ) |> tooltip(shiny::textOutput(ns("version_select"))),
            #Choose not before
            shiny::dateInput(
              ns("not_before"),
              shiny::span(
                bsicons::bs_icon("5-circle"),
                weekstart = 1,
                shiny::strong("Define a cut-off date, prior to which no data will be imported."),
              ),
              value = "2001-01-01"
            ),
            #Choose options
            shiny::checkboxGroupInput(
              ns("options"),
              shiny::span(
                bsicons::bs_icon("6-circle"),
                shiny::strong("Select the following options if applicable")
              ),
              choiceNames = list(
                tooltip("Handle daylight savings (DST) Jumps", "If active data collection crossed a daylight savings time, AND it is not handled automatically by the device, this option shifts data by one hour forward or backward, after a daylight savings jump."),
                tooltip("Remove duplicates", "If the files contain duplicate entries, they can be removed with this option. Otherwise, import will throw an error.")
              ),
              choiceValues = list(
                "dst_jumps",
                "remove_duplicates"
              )
            ),
          shiny::radioButtons(
            ns("id"),
            shiny::span(
              bsicons::bs_icon("7-circle"),
              shiny::strong("Participant IDs")) |> tooltip("If the files contain a column `Id`, neither option will be used."),
                              inline = TRUE,
                              width = "100%",
                              choiceNames = list(
                                tooltip("Automated handling", "A automatically shortened file name is used as an identifier."),
                                tooltip("Set a global ID manually", "Allows you to set an individual ID."),
                                tooltip("Extract ID from file name", "Allows you to define a regular expression to extract portion of the filename")
                              ),
                              choiceValues = list("automated", "manual", "extract"))
          ),
          #third column of specs
          shiny::tagList(
            shiny::h4("Conditional"),
            shiny::p("This section will show conditional options when needed"),
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
                tooltip("LLMs are exceptionally good at creating regular expressions, provided they get a few sample file names and a direction.")
            ),
          )),
          bslib::layout_column_wrap(
            #show file names and number
            bslib::value_box(
              "Number of files",
              value = shiny::textOutput(ns("n_files")),
              showcase = bsicons::bs_icon("journals"),
              theme = "tertiary",
              shiny::textOutput(ns("filenames"))
            ),
            #show extracted ids
            bslib::value_box(
              shiny::span("Number of IDs") |>
                tooltip("Based on the prior settings and file names (irrelevant if `Id` column is present in the data"),
              value = shiny::textOutput(ns("n_ids")),
              showcase = bsicons::bs_icon("search"),
              theme = "tertiary",
              shiny::textOutput(ns("pattern"))
            )
        ),
        #import button
        shiny::div(
          actionButton(
            ns("import"),
            shiny::span(bsicons::bs_icon("5-circle"), shiny::strong("Import")),
            icon = shiny::icon("file-import"),
            width = "50%",
            class = "btn-primary btn-lg"
          ),
          style = "text-align:center;"
        )
      ),
      bslib::accordion_panel(
        shiny::h3("Import summary"),
        value = "import_summary",
        bslib::layout_column_wrap(
          bslib::card(
            bslib::card_header("Import message"),
            shiny::verbatimTextOutput(ns("import_msg")),
            # min_height = "400px"
          ),
          bslib::card(
            bslib::card_header("Overview Plot"),
            shiny::plotOutput(ns("plot_overview")),
            # min_height = "400px"
          )
        ),
        gt::gt_output(ns("import_table"))
      )
    )
  )
}

# Server ------------------------------------------------------------------

importServer <-
  function(id, import_specs = NULL
  ) {

    shiny::moduleServer(id, function(input, output, session) {

      #Set up a container for the import specs to go into, if it isn´t already defined
      if (is.null(import_specs)){
        import_specs <-
          shiny::reactiveValues(
            file.names = NULL, device = NULL, tz = NULL, options = NULL,
            not_before = NULL, version = NULL, Id = NULL, pattern = NULL
          )
      }

      #number of files
      output$n_files <- renderText({
        if(is.null(input$file)) return("no files provided")
        paste0(input$file %>% nrow())
      })

      #filenames
      output$filenames <- renderText({
        input$file$name |> tools::file_path_sans_ext() |> paste0(collapse = ",\n")
      })


      #update selectizeInput for file version choices
      shiny::observe(
        shiny::updateSelectizeInput(
          session,
          inputId = "version",
          choices = supported_versions(input$device) |>
            dplyr::pull(Version) |>
            c("default") |>
            rev()
        )
      ) |> shiny::bindEvent(input$device, ignoreInit = TRUE)

      #tooltip on device file version
      output$version_select <- shiny::renderText({
        shiny::req(input$device != "")
        supported_versions(input$device) |>
          dplyr::filter(Version == input$version | ((input$version == "default") & Default)) |>
          dplyr::pull(Description)
      })

      #Ids to be used
      Id_preview <- shiny::reactiveVal()

      shiny::observe({
        shiny::req(input$file)
        if(input$id == "manual") {
          Id_preview(input$Id_manual)

        } else if(input$id == "extract") {
          shiny::req(input$Id_extract)
          Ids <-
            tryCatch({
            input$file$name |>
            tools::file_path_sans_ext() |>
            stringr::str_extract(pattern = input$Id_extract)
            },
            error = \(x) numeric())
          Id_preview(Ids)

        } else if(input$id == "automated") {
          min_char <-
          input$file$name |>
          tools::file_path_sans_ext() |>
          min_unique_prefix_length()
          Ids <- stringr::str_trunc(input$file$name, min_char, ellipsis = "")
          Id_preview(Ids)
        }
      })

      #pattern to capture Ids
      output$pattern <- shiny::renderText({
        shiny::req(input$file)
        if(length(Id_preview()) == 0) return("no ID specification possible, please adjust settings")
        Id_preview() |> paste0(collapse = ",\n")
      })
      output$n_ids <- shiny::renderText({
        if(is.null(input$file)) return("no files provided")
        paste0(Id_preview() |> length())
      })

      #change the filenames to the original name
      new_names <- shiny::reactive({
        shiny::req(input$file)
        #renaming the temp-files to their old filename
        new_names <- paste0(dirname(input$file$datapath), "/", input$file$name)
        file.rename(input$file$datapath, new_names)
        new_names
      }) |>  shiny::bindEvent(input$file)

      # Import handling:
      # - capture the console output from import_Dataset() for the summary panel
      # - return the imported data for the table and overview plot
      import_result <- shiny::eventReactive(input$import, {
        shiny::req(input$device)
        import_msg <- character()
        imported_data <- NULL

        import_msg <- capture.output({
          imported_data <- import_Dataset(
            input$device,
            new_names(),
            tz = input$tz,
            not.before = input$not_before,
            dst_adjustment = "dst_jumps" %in% input$options,
            remove_duplicates = "remove_duplicates" %in% input$options,
            auto.plot = FALSE,
            print_n = Inf
          )
        })

        list(
          data = imported_data,
          msg = import_msg
        )
      })

      shiny::observe({
        shiny::showNotification(
          paste("Import is in progress. Should the app freeze and grey out, if the import is not successful. A message will be shown upon successfull import."),
          type = "default",
          duration = 5
        )
      }) |>
        shiny::bindEvent(input$import)

      #outputs p1
      output$import_msg <- shiny::renderText({
        shiny::req(import_result())
        paste(import_result()$msg, collapse = "\n")
      })

      shiny::observe({
        # req(data())
        shiny::showModal(
          shiny::modalDialog(
            title = icon("check", style = "font-size: 60px;"),
            easy_close = TRUE,
            "Import seems to have been successful. Please check the import message and overview plot and continue to the analysis tab if satisfied."
          ),
        )
      }) |>
        shiny::bindEvent(import_result())

      output$import_table <-  gt::render_gt({
        shiny::req(import_result())
        gt::gt(import_result()$data)
      })

      output$plot_overview <- shiny::renderPlot({
        shiny::req(import_result())
        import_result()$data %>% gg_overview()
      })


      #Return_Value
      # shiny::reactive(
      #   list(in1 = (input$zu_Import1),
      #        in2 = (input$zu_Import2)
      #   ))

    })
  }

# App ---------------------------------------------------------------------
