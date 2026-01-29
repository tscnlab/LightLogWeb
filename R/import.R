build_import_call <- function(device,
                              file_names,
                              tz,
                              not_before,
                              options,
                              version,
                              conditional_args) {
  options <- rlang::`%||%`(options, character())
  conditional_args <- rlang::`%||%`(conditional_args, list())

  args <- list(
    device = device,
    filename = file_names,
    tz = tz,
    not.before = not_before,
    dst_adjustment = "dst_jumps" %in% options,
    remove_duplicates = "remove_duplicates" %in% options,
    auto.plot = FALSE,
    version = version
  )

  if (length(conditional_args) > 0) {
    args <- c(args, conditional_args)
  }

  import_call <- rlang::call2(
    .fn = "import_Dataset",
    .ns = "LightLogR",
    !!!args
  )

  shinymeta::metaExpr(import_call)
}

importUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    #Import accordion
    bslib::accordion(
      multiple = FALSE,
      id = ns("import_accordion"),
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
            #Set a name for the dataset
            shiny::textInput(
              ns("dataset_name"),
              shiny::span(
                bsicons::bs_icon("4-circle"),
                shiny::strong("Choose a unique name for the dataset (internal reference)")
              ),
            )
          ),
          #second column of specs
          shiny::tagList(
          shiny::h4("Optional"),
            #Choose version
            shiny::selectizeInput(
              ns("version"),
              shiny::span(
                bsicons::bs_icon("5-circle"),
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
                bsicons::bs_icon("6-circle"),
                weekstart = 1,
                shiny::strong("Define a cut-off date, prior to which no data will be imported."),
              ),
              value = "2001-01-01"
            ),
            #Choose options
            shiny::checkboxGroupInput(
              ns("options"),
              shiny::span(
                bsicons::bs_icon("7-circle"),
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
              bsicons::bs_icon("8-circle"),
              shiny::strong("Participant IDs")) |> tooltip("If the files contain a column `Id`, neither option will be used."),
                              inline = TRUE,
                              width = "100%",
                              choiceNames = list(
                                tooltip("Automated handling", "An automatically shortened file name is used as an identifier."),
                                tooltip("Set a global ID manually", "Allows you to set an individual ID."),
                                tooltip("Extract ID from file name", "Allows you to define a regular expression to extract portion of the filename")
                              ),
                              choiceValues = list("automated", "manual", "extract"))
          ),
          #third column of specs
          shiny::tagList(
            shiny::h4("Conditional"),
            shiny::p("This section will show conditional options when needed"),
            shiny::conditionalPanel(
              condition = "input.device == 'VEET'",
              ns = ns,
              shiny::selectizeInput(
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
            shiny::conditionalPanel(
              condition = "input.id == 'manual'",
              ns = ns,
              shiny::textInput(
                inputId = ns("Id_manual"),
                label   = "Specify the global ID",
                placeholder = "Give me a string",
                value = "Participant",
                updateOn = "blur",
                width = "100%"
              )
            ),
            shiny::conditionalPanel(
              condition = "input.id == 'extract'",
              ns = ns,
              shiny::textInput(
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
              theme = bslib::value_box_theme(bg =  "#d3d3d350"),
              shiny::textOutput(ns("filenames"))
            ),
            #show extracted ids
            bslib::value_box(
              shiny::span("Number of IDs") |>
                tooltip("Based on the prior settings and file names (irrelevant if `Id` column is present in the data"),
              value = shiny::textOutput(ns("n_ids")),
              showcase = bsicons::bs_icon("search"),
              theme = bslib::value_box_theme(bg =  "#d3d3d350"),
              shiny::textOutput(ns("pattern"))
            )
        ),
        #import button
        shiny::p(
          bslib::input_task_button(
            ns("import"),
            shiny::span(shiny::strong("Import")),
            icon = shiny::icon("file-import"),
            width = "50%",
            class = "btn-primary btn-lg",
            style = "width: 50%;"
          ),
          style = "text-align:center;"
        ),
      ),
      bslib::accordion_panel(
        shiny::h3("Import summary"),
        value = "import_summary",
        bslib::layout_column_wrap(
          bslib::card(
            bslib::card_header("Import message", container = htmltools::h4),
            shiny::verbatimTextOutput(ns("import_msg")),
            min_height = "400px"
          ),
          bslib::card(
            bslib::card_header("Overview Plot", container = htmltools::h4),
            shiny::plotOutput(ns("plot_overview")),
            min_height = "400px"
          )
        ),
        shiny::p(
          shiny::actionButton(
            ns("add_dataset"),
            shiny::span(shiny::strong("Add dataset to library")),
            icon = shiny::icon("database"),
            width = "50%",
            class = "btn-primary btn-lg"
          ),
          style = "text-align:center;"
        ),
        bslib::card(
          bslib::card_header("Imported table", container = htmltools::h4) |>
            tooltip("This table shows the first and last 50 rows of the imported data"),
          gt::gt_output(ns("import_table"))
        )
      )
    )
  )
}

# Server ------------------------------------------------------------------

importServer <-
  function(id, import_specs = NULL
  ) {

    shiny::moduleServer(id, function(input, output, session) {


  # General -------------
      #Set up a container for the import specs to go into, if it isn´t already defined
      if (is.null(import_specs)){
        import_specs <-
          shiny::reactiveValues(
            file.names = NULL, device = NULL, tz = NULL, options = NULL,
            not_before = NULL, version = NULL, Id = NULL, pattern = NULL
          )
      }

  # Import specification adjustments -------------

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

      #update to dataset name
      shiny::observe({
        shiny::updateTextInput(session,
                               "dataset_name",
                               value = paste0(input$device,
                                              ".",
                                              lubridate::now() |>
                                                lubridate::format_ISO8601(precision = "ymdhm")
                                              ) |> make.names()
                                              )
      }) |> shiny::bindEvent(input$device, ignoreInit = TRUE)

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

  # Import -------------

      #change the filenames to the original name
      new_names <- shiny::reactive({
        shiny::req(input$file)
        #renaming the temp-files to their old filename
        new_names <- paste0(dirname(input$file$datapath), "/", input$file$name)
        file.rename(input$file$datapath, new_names)
        new_names
      }) |>  shiny::bindEvent(input$file)

      conditional_args <- shiny::reactive({
        args <- list()

        if (input$device == "VEET") {
          args <- append(args, list(modality = input$veet_modality))
        }

        if (input$id == "manual") {
          args <- append(args, list(manual.id = input$Id_manual))
        } else if (input$id == "extract") {
          args <- append(args, list(auto.id = input$Id_extract))
        } else if (input$id == "automated" && length(Id_preview()) > 0) {
          args <- append(
            args,
            list(auto.id = paste0(".{", nchar(Id_preview()[1]), "}"))
          )
        }

        args
      })

      # Import handling:
      # - capture the console output from import_Dataset() for the summary panel
      # - return the imported data for the table and overview plot
      import_result <- shiny::eventReactive(input$import, {

        import_msg <- character()
        imported_data <- NULL

        shiny::req(input$device,
                   input$file,
                   input$device != "",
                   input$dataset_name != "")

        conditional_arg <- append(list(print_n = Inf), conditional_args())

        rlang::inject({
        import_msg <- capture.output({
          tryCatch({
          imported_data <- import_Dataset(
            device = input$device,
            filename = new_names(),
            tz = input$tz,
            not.before = input$not_before,
            dst_adjustment = "dst_jumps" %in% input$options,
            remove_duplicates = "remove_duplicates" %in% input$options,
            auto.plot = FALSE,
            version = input$version,
            !!!conditional_arg
          )
        }, error = \(x) {
          cat("\nImport failed.\nPlease check files and settings.\nDetailed error message:\n")
          x
        }
        )
        })
        })

        #adjust import string if necessary
        if (length(import_msg) > 0 && startsWith(import_msg[1], "\r")) {
          import_msg <- import_msg[-1]
        }

        list(
          data = imported_data,
          msg = import_msg
        )
      })

  # Import messaging -------------

      #Notification
      shiny::observe({
        if(!list(input$file, input$device, input$tz) |>
           purrr::map_lgl(is.null) |>
           any() & input$device != "" & input$dataset_name != ""
           ){
        shiny::showNotification(
          paste("Import is in progress. A message will be shown upon successfull import."),
          type = "message",
          duration = 5
        ) } else {
        shiny::showNotification(
            "Please specify files, device, time zone, and name",
          type = "error",
          duration = 5
        )
      }
        }) |>
        shiny::bindEvent(input$import)

      #Switch to summary after import
      shiny::observe({
        bslib::accordion_panel_set("import_accordion", "import_summary")
      }) |> shiny::bindEvent(import_result())

      #Modal
      shiny::observe({
        req(import_result()$data)
        dimensions <- dim(import_result()$data)
        shiny::showModal(
          shiny::modalDialog(
            title = icon("check", style = "font-size: 60px;"),
            easyClose = TRUE,
            shiny::strong("Import successful!"),
            shiny::p(glue::glue("The imported table measures {dimensions[2]|> prettyNum()} x {dimensions[1]|> prettyNum()} (columns x rows).")),
            shiny::p("Please check the import message and overview plot and continue to the analysis tab if satisfied.")
          ),
        )
      }) |>
        shiny::bindEvent(import_result()$data |> is.data.frame())

      # Import summary -------------

      output$import_table <-  gt::render_gt({
        gt::gt_preview(import_result()$data, top_n = 50, bottom_n = 50) |>
          gt::opt_interactive(
            use_search = TRUE,
            use_filters = TRUE, use_resizers = TRUE,
            use_highlight = TRUE
          )
      })

      output$plot_overview <- shiny::renderPlot({
        import_result()$data %>% gg_overview()
      })

      output$import_msg <- shiny::renderText({
        paste(import_result()$msg, collapse = "\n")
      })

      add_dataset <- shiny::eventReactive(input$add_dataset, {

        shiny::req(import_result()$data,
                   input$dataset_name,
                   input$device,
                   input$tz)

        id_value <- if (input$id == "manual") {
          input$Id_manual
        } else if (input$id == "extract") {
          input$Id_extract
        } else {
          NA_character_
        }

        import_specs <- list(
          file_names = input$file$name,
          options = input$options,
          version = input$version,
          not_before = input$not_before,
          id_strategy = input$id,
          id_value = id_value
        )

        import_call <- build_import_call(
          device = input$device,
          file_names = input$file$name,
          tz = input$tz,
          not_before = input$not_before,
          options = input$options,
          version = input$version,
          conditional_args = conditional_args()
        )
        list(
          name = input$dataset_name,
          data = import_result()$data,
          device = input$device,
          tz = input$tz,
          import_specs = import_specs,
          import_call = import_call
        )
      })

      shiny::observe({
        res <- tryCatch(
          import_result(),
          error = function(e) NULL
        )

        if (is.null(res) || is.null(res$data) || !is.data.frame(res$data)) {
          shiny::showNotification(
            "Please provide a valid import.",
            type = "warning",
            duration = 5
          )}
          }) |> shiny::bindEvent(input$add_dataset)

      # Return value -------------

      list(add_dataset = add_dataset)

    })
  }
