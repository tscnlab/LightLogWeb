reportUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3("Report"),
    shiny::p("Download a consolidated report of your imported datasets."),
    shiny::uiOutput(ns("report_summary")),
    shiny::downloadButton(
      ns("download_report_qmd"),
      "Download report (QMD)",
      class = "btn-primary"
    )
  )
}

reportServer <- function(id, datasets) {
  shiny::moduleServer(id, function(input, output, session) {
    report_data <- shiny::reactive({
      build_report_data(datasets)
    })

    output$report_summary <- shiny::renderUI({
      data <- report_data()
      if (data$dataset_count == 0) {
        return(shiny::p("No datasets imported yet."))
      }

      shiny::tagList(
        shiny::p(
          shiny::strong("Total datasets imported:"),
          data$dataset_count
        ),
        shiny::p(
          shiny::strong("Multiple datasets imported:"),
          ifelse(data$dataset_count > 1, "Yes", "No")
        ),
        shiny::p(
          shiny::strong("Dataset names:"),
          paste(data$dataset_names, collapse = ", ")
        )
      )
    })

    output$download_report_qmd <- shiny::downloadHandler(
      filename = function() {
        paste0("lightlogweb-report-", Sys.Date(), ".qmd")
      },
      content = function(file) {
        report_text <- build_report_qmd(report_data())
        writeLines(report_text, file)
      }
    )
  })
}

build_report_data <- function(datasets) {
  dataset_list <- shiny::reactiveValuesToList(datasets)
  dataset_names <- names(dataset_list)

  dataset_details <- lapply(dataset_names, function(dataset_name) {
    dataset <- dataset_list[[dataset_name]]
    metadata <- rlang::`%||%`(dataset$metadata, list())
    import_specs <- rlang::`%||%`(dataset$import_specs, list())

    list(
      name = dataset_name,
      rows = if (is.data.frame(dataset$data)) nrow(dataset$data) else NA_integer_,
      cols = if (is.data.frame(dataset$data)) ncol(dataset$data) else NA_integer_,
      device = rlang::`%||%`(metadata$device, NA_character_),
      tz = rlang::`%||%`(metadata$tz, NA_character_),
      variable = rlang::`%||%`(metadata$variable, NA_character_),
      variable_name = rlang::`%||%`(metadata$variable_name, NA_character_),
      variable_unit = rlang::`%||%`(metadata$variable_unit, NA_character_),
      import_specs = import_specs,
      import_call = dataset$import_call
    )
  })

  list(
    dataset_count = length(dataset_names),
    dataset_names = rlang::`%||%`(dataset_names, character()),
    dataset_details = dataset_details
  )
}

build_report_qmd <- function(report_data) {
  header <- c(
    "---",
    "title: \"LightLogWeb Report\"",
    "format: html",
    "---",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    ""
  )

  overview <- c(
    "## Dataset overview",
    "",
    paste0("- Total datasets imported: ", report_data$dataset_count),
    paste0(
      "- Multiple datasets imported: ",
      ifelse(report_data$dataset_count > 1, "Yes", "No")
    ),
    if (report_data$dataset_count > 0) {
      paste0("- Dataset names: ", paste(report_data$dataset_names, collapse = ", "))
    } else {
      "- Dataset names: None"
    },
    ""
  )

  details <- c("## Dataset details", "")

  if (report_data$dataset_count == 0) {
    details <- c(details, "No datasets were available when this report was generated.")
  } else {
    for (dataset in report_data$dataset_details) {
      details <- c(
        details,
        paste0("### ", dataset$name),
        "",
        paste0("- Rows: ", format_report_value(dataset$rows)),
        paste0("- Columns: ", format_report_value(dataset$cols)),
        paste0("- Device: ", format_report_value(dataset$device)),
        paste0("- Time zone: ", format_report_value(dataset$tz)),
        paste0("- Variable: ", format_report_value(dataset$variable)),
        paste0("- Variable name: ", format_report_value(dataset$variable_name)),
        paste0("- Variable unit: ", format_report_value(dataset$variable_unit)),
        paste0(
          "- Imported files: ",
          format_report_value(dataset$import_specs$file_names)
        ),
        paste0(
          "- Import options: ",
          format_report_value(dataset$import_specs$options)
        ),
        paste0(
          "- Import version: ",
          format_report_value(dataset$import_specs$version)
        ),
        paste0(
          "- Import date cut-off: ",
          format_report_value(dataset$import_specs$not_before)
        ),
        paste0(
          "- ID strategy: ",
          format_report_value(dataset$import_specs$id_strategy)
        ),
        paste0(
          "- ID value: ",
          format_report_value(dataset$import_specs$id_value)
        ),
        ""
      )

      import_call <- format_import_call(dataset$import_call)
      details <- c(
        details,
        "#### Reproducible import call",
        "",
        "```r",
        import_call,
        "```",
        ""
      )
    }
  }

  c(header, overview, details)
}

format_report_value <- function(value) {
  if (is.null(value)) {
    return("Not recorded")
  }
  if (length(value) == 0) {
    return("None")
  }
  if (length(value) > 1) {
    return(paste(value, collapse = ", "))
  }
  if (is.na(value)) {
    return("Not recorded")
  }
  as.character(value)
}

format_import_call <- function(import_call) {
  if (is.null(import_call)) {
    return("NULL")
  }

  expanded <- tryCatch(
    shinymeta::expandChain(import_call),
    error = function(e) NULL
  )

  if (is.null(expanded)) {
    return(paste(deparse(import_call), collapse = "\n"))
  }

  expr_text <- tryCatch(
    rlang::expr_deparse(expanded),
    error = function(e) NULL
  )

  if (is.null(expr_text)) {
    return(paste(deparse(import_call), collapse = "\n"))
  }

  paste(expr_text, collapse = "\n")
}
