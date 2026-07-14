# Production Shiny application ---------------------------------------------

llw_app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#F5F8FA",
    fg = "#0B3041",
    primary = "#0B789C",
    secondary = "#5E7480",
    success = "#26735B",
    warning = "#A86118",
    danger = "#A9413D",
    base_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif"),
    heading_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif")
  )
}

llw_brand <- function(compact = FALSE) {
  shiny::tags$div(
    class = paste("llw-brand", if (compact) "llw-brand-compact" else ""),
    shiny::tags$span(class = "llw-brand-icon", bsicons::bs_icon("sunrise")),
    shiny::tags$span(shiny::tags$strong("LightLog"), "Web")
  )
}

llw_screen_heading <- function(eyebrow, title, description = NULL, actions = NULL) {
  shiny::tags$header(
    class = "llw-screen-heading",
    shiny::tags$div(
      shiny::tags$p(class = "llw-eyebrow", eyebrow),
      shiny::tags$h1(title),
      if (!is.null(description)) shiny::tags$p(class = "llw-lede", description)
    ),
    if (!is.null(actions)) shiny::tags$div(class = "llw-heading-actions", actions)
  )
}

llw_landing_ui <- function() {
  shiny::tags$div(
    class = "llw-landing",
    shiny::tags$header(
      class = "llw-landing-nav",
      llw_brand(),
      shiny::tags$nav(
        `aria-label` = "LightLogR resources",
        shiny::tags$a("Documentation", href = "https://tscnlab.github.io/LightLogR/", target = "_blank", rel = "noopener"),
        shiny::actionButton("landing_import", "Start import", class = "btn btn-outline-primary")
      )
    ),
    shiny::tags$main(
      class = "llw-hero",
      shiny::tags$section(
        class = "llw-hero-copy",
        shiny::tags$p(class = "llw-eyebrow", "Wearable light data, clearly understood"),
        shiny::tags$h1("From logger files to a transparent analysis."),
        shiny::tags$p(class = "llw-hero-lede", "Import, inspect, prepare, calculate, and visualize personal light exposure - then reproduce every decision from R."),
        shiny::tags$div(
          class = "llw-hero-actions",
          shiny::actionButton("explore_demo", shiny::tagList(bsicons::bs_icon("play-circle"), "Explore the demo"), class = "btn btn-primary btn-lg"),
          shiny::actionButton("start_import", shiny::tagList(bsicons::bs_icon("upload"), "Start an import"), class = "btn btn-light btn-lg")
        ),
        shiny::tags$p(class = "llw-privacy", bsicons::bs_icon("shield-check"), "Data stay in this R session unless you explicitly download a project bundle.")
      ),
      shiny::tags$section(
        class = "llw-hero-visual",
        shiny::tags$div(class = "llw-sun"),
        shiny::tags$div(
          class = "llw-hero-card",
          shiny::tags$strong("Personal light exposure"),
          shiny::tags$span(class = "llw-demo-badge", "DEMO"),
          shiny::tags$svg(
            viewBox = "0 0 640 230", role = "img", `aria-label` = "Illustrative personal light exposure curve",
            shiny::tags$path(class = "llw-chart-grid", d = "M20 55H620 M20 115H620 M20 175H620"),
            shiny::tags$path(class = "llw-chart-area", d = "M20 196 C80 194 110 160 180 143 C220 136 225 66 270 82 C307 96 320 50 362 62 C407 76 420 132 460 139 C520 150 540 186 620 196 L620 210 L20 210 Z"),
            shiny::tags$path(class = "llw-chart-line", d = "M20 196 C80 194 110 160 180 143 C220 136 225 66 270 82 C307 96 320 50 362 62 C407 76 420 132 460 139 C520 150 540 186 620 196")
          ),
          shiny::tags$div(class = "llw-chart-axis", lapply(c("00", "06", "12", "18", "24"), shiny::tags$span))
        )
      )
    ),
    shiny::tags$section(
      class = "llw-journey",
      llw_screen_heading("One reproducible workflow", "Stay close to the data. Keep every decision."),
      shiny::tags$div(
        class = "llw-journey-grid",
        lapply(
          list(
            c("01", "Import", "Identify devices, participants, timestamps, and quantities."),
            c("02", "Inspect", "Review coverage, gaps, sampling, and temporal structure."),
            c("03", "Analyse", "Prepare, group, calculate metrics, and visualize."),
            c("04", "Continue in R", "Download code, results, figures, or a local project.")
          ),
          function(item) shiny::tags$article(shiny::tags$span(item[[1]]), shiny::tags$h3(item[[2]]), shiny::tags$p(item[[3]]))
        )
      )
    )
  )
}

llw_sidebar_choices <- function(modules) {
  built_in <- c(
    "Overview" = "overview",
    "Prepare" = "prepare",
    "Group" = "group",
    "Metrics" = "metrics",
    "Visualize" = "visualize"
  )
  custom <- if (length(modules)) stats::setNames(paste0("module__", names(modules)), vapply(modules, `[[`, character(1), "title")) else character()
  c(built_in, custom, "Metadata" = "metadata", "Raw data" = "raw", "Recipe" = "recipe", "Projects & datasets" = "projects", "Import data" = "import")
}

llw_workspace_ui <- function(dataset_names, selected, screen, modules) {
  shiny::tags$div(
    class = "llw-workspace",
    shiny::tags$header(
      class = "llw-app-header",
      shiny::actionButton("home", llw_brand(compact = TRUE), class = "llw-home", `aria-label` = "Return to LightLogWeb home"),
      shiny::tags$div(
        class = "llw-dataset-picker",
        shiny::tags$label(`for` = "active_dataset", "Active dataset"),
        shiny::selectInput("active_dataset", NULL, choices = dataset_names, selected = selected, width = "250px")
      ),
      shiny::tags$div(
        class = "llw-header-actions",
        shiny::actionButton("header_recipe", shiny::tagList(bsicons::bs_icon("list-check"), shiny::tags$span("Recipe")), class = "btn btn-light"),
        shiny::downloadButton("download_script", "R script", class = "btn btn-primary")
      )
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::tags$p(class = "llw-eyebrow", "Workflow"),
        shiny::radioButtons("workflow", NULL, choices = llw_sidebar_choices(modules), selected = screen),
        shiny::tags$div(
          class = "llw-sidebar-note",
          bsicons::bs_icon("info-circle"),
          shiny::tags$p("Calculations run through the same exported functions available from the R console.")
        ),
        open = "desktop",
        width = 280
      ),
      shiny::uiOutput("screen", class = "llw-screen-host")
    )
  )
}

llw_no_dataset_ui <- function() {
  shiny::tags$section(
    class = "llw-screen llw-empty",
    bsicons::bs_icon("database-add"),
    shiny::tags$h2("Add a dataset to begin"),
    shiny::tags$p("Import logger files or use the bundled demonstration dataset."),
    shiny::actionButton("empty_import", "Open import", class = "btn btn-primary")
  )
}

llw_import_screen <- function() {
  devices <- LightLogR::supported_devices()
  shiny::tags$section(
    class = "llw-screen",
    llw_screen_heading("Import", "Bring source data into a traceable workspace.", "LightLogR device support is discovered at runtime; normalized LightLogR tables can also be uploaded directly."),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("Source and interpretation"),
        shiny::fileInput("import_files", "Logger or normalized files", multiple = TRUE, accept = c(".csv", ".tsv", ".txt", ".rds", ".zip")),
        shiny::selectInput("import_device", "Device / format", choices = c("Normalized LightLogR table" = "normalized", stats::setNames(devices, devices)), selected = "normalized"),
        shiny::uiOutput("import_device_options"),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::selectizeInput("import_tz", "Time zone", choices = OlsonNames(), selected = "UTC"),
          shiny::selectInput("import_id_mode", "Participant IDs", choices = c("Automatic from filename" = "auto", "Extract with regular expression" = "extract", "One manual ID" = "manual"))
        ),
        shiny::conditionalPanel("input.import_id_mode == 'extract'", shiny::textInput("import_id_regex", "ID extraction expression", value = "^([^_]+)")),
        shiny::conditionalPanel("input.import_id_mode == 'manual'", shiny::textInput("import_manual_id", "Participant ID", value = "Participant")),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::textInput("import_name", "Dataset name", value = "light-data"),
        shiny::textInput("import_variable", "Primary variable", value = "MEDI")
        ),
        shiny::uiOutput("import_id_preview"),
        shiny::checkboxInput("import_dst", "Apply device-specific DST correction", FALSE),
        shiny::checkboxInput("import_remove_duplicates", "Remove duplicate timestamps during import", TRUE),
        shiny::actionButton("run_import", shiny::tagList(bsicons::bs_icon("file-earmark-arrow-up"), "Import and validate"), class = "btn btn-primary")
      ),
      bslib::card(
        bslib::card_header("Import report"),
        shiny::uiOutput("import_report"),
        DT::DTOutput("import_preview")
      )
    )
  )
}

llw_overview_screen <- function(dataset) {
  ids <- unique(as.character(dataset$prepared_data$Id))
  shiny::tags$section(
    class = "llw-screen",
    llw_screen_heading("Dataset overview", dataset$name, "Shape, temporal integrity, and availability before analysis."),
    shiny::uiOutput("overview_values"),
    shiny::tags$section(class = "llw-quality-strip", shiny::uiOutput("quality_strip")),
    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          "Exposure across the study",
          shiny::selectInput("overview_id", "Stream", choices = c("All" = "All", ids), selected = ids[[1]], width = "180px")
        ),
        shiny::tags$div(
          role = "img",
          `aria-label` = "Light exposure timeline for the selected stream",
          shiny::plotOutput("overview_plot", height = "430px")
        )
      ),
      bslib::card(
        bslib::card_header("Daily availability"),
        DT::DTOutput("availability_table")
      )
    )
  )
}

llw_prepare_screen <- function(dataset) {
  data <- dataset$prepared_data
  variable <- llw_primary_variable(dataset)
  current_range <- range(data[[variable]], na.rm = TRUE)
  dates <- range(as.Date(data$Datetime, tz = dataset$metadata$timezone))
  shiny::tags$section(
    class = "llw-screen",
    llw_screen_heading("Prepare", "Make transformations explicit.", "Previewed transformations create a new analysis stage; the original data are never overwritten."),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("Preparation recipe"),
        bslib::accordion(
          multiple = TRUE, open = c("time", "sampling"),
          bslib::accordion_panel(
            "1 - Time and study window", value = "time",
            shiny::selectizeInput("prep_timezone", "Analysis time zone", choices = OlsonNames(), selected = dataset$metadata$timezone),
            shiny::radioButtons("prep_timezone_mode", "Timezone operation", choices = c("Preserve instants" = "preserve_instant", "Reinterpret recorded clock readings" = "reinterpret"), selected = "preserve_instant"),
            shiny::dateRangeInput("prep_dates", "Study window", start = dates[[1]], end = dates[[2]], min = dates[[1]], max = dates[[2]])
          ),
          bslib::accordion_panel(
            "2 - Measurement range", value = "range",
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::numericInput("prep_min", "Plausible minimum", current_range[[1]]),
              shiny::numericInput("prep_max", "Plausible maximum", current_range[[2]])
            ),
            shiny::checkboxInput("prep_invalidate", "Mark out-of-range readings as explicit NA with a reason", TRUE)
          ),
          bslib::accordion_panel(
            "3 - Sampling and gaps", value = "sampling",
            shiny::radioButtons("prep_gaps", "Implicit gaps", choices = c("Convert to explicit NA observations" = "explicit_na", "Leave timestamps unchanged" = "leave"), selected = "explicit_na"),
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::selectInput("prep_interval", "Analysis interval", choices = c("No aggregation" = "", "30 sec", "1 min", "5 min", "15 min", "1 hour"), selected = "5 min"),
              shiny::selectInput("prep_function", "Numeric aggregation", choices = c("Mean" = "mean", "Median" = "median", "Maximum" = "max"))
            )
          ),
          bslib::accordion_panel(
            "4 - Daily inclusion", value = "inclusion",
            shiny::sliderInput("prep_missing", "Maximum missing time per participant-day", min = 0, max = 50, value = 20, step = 5, post = "%")
          ),
          bslib::accordion_panel(
            "5 - Auxiliary intervals", value = "annotations",
            shiny::fileInput("annotation_file", "Sleep, wear, protocol, or event CSV", accept = ".csv"),
            shiny::textInput("annotation_output", "Output state column", value = "State"),
            shiny::tags$p(class = "form-text", "Expected columns: Id, start, end, State. Start is inclusive; end is exclusive."),
            shiny::actionButton("add_annotation", "Add interval annotation", class = "btn btn-outline-primary")
          )
        ),
        shiny::actionButton("add_preparation", "Add preparation to recipe", class = "btn btn-primary")
      ),
      bslib::card(
        bslib::card_header("Impact preview"),
        shiny::uiOutput("prepare_preview_stats"),
        shiny::tags$div(
          role = "img",
          `aria-label` = "Prepared data preview",
          shiny::plotOutput("prepare_preview_plot", height = "320px")
        )
      )
    )
  )
}

llw_group_screen <- function(dataset) {
  annotation_columns <- names(dataset$annotations)
  coords <- dataset$metadata$coordinates %||% c(NA_real_, NA_real_)
  shiny::tags$section(
    class = "llw-screen",
    llw_screen_heading("Group", "Define the units every result describes.", "Grouping remains separate from metric selection and is recorded in the recipe."),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("Grouping dimensions"),
        shiny::checkboxGroupInput(
          "group_dimensions", "Group by",
          choices = c("Participant / stream" = "participant", "Local calendar date" = "date", "Weekday / weekend" = "day_type", "Clock window" = "clock_window", "Photoperiod" = "photoperiod", "Annotation state" = "annotation"),
          selected = c("participant", "date")
        ),
        shiny::conditionalPanel(
          "input.group_dimensions.indexOf('clock_window') >= 0",
          bslib::layout_columns(col_widths = c(6, 6), shiny::numericInput("group_clock_start", "Start hour", 6, min = 0, max = 24), shiny::numericInput("group_clock_end", "End hour", 12, min = 0, max = 24))
        ),
        shiny::conditionalPanel(
          "input.group_dimensions.indexOf('photoperiod') >= 0",
          bslib::layout_columns(col_widths = c(4, 4, 4), shiny::numericInput("group_lat", "Latitude", coords[[1]], min = -90, max = 90), shiny::numericInput("group_lon", "Longitude", coords[[2]], min = -180, max = 180), shiny::numericInput("group_solar", "Solar depression", 6))
        ),
        shiny::conditionalPanel(
          "input.group_dimensions.indexOf('annotation') >= 0",
          shiny::selectizeInput("group_annotation", "Annotation columns", choices = annotation_columns, multiple = TRUE)
        ),
        shiny::actionButton("add_grouping", "Add grouping to recipe", class = "btn btn-primary")
      ),
      bslib::card(
        bslib::card_header("Grouping preview"),
        shiny::uiOutput("group_preview")
      )
    )
  )
}

llw_metrics_screen <- function() {
  registry <- llw_metric_registry()
  labels <- paste0(registry$name, " - ", registry$family)
  values <- stats::setNames(registry$id, labels)
  shiny::tags$section(
    class = "llw-screen",
    llw_screen_heading("Metrics", "Choose measures by the question they answer.", "Every metric shows its requirements, parameters, output, and direct LightLogR documentation. Defaults are not exposure recommendations."),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("Metric catalog"),
        shiny::selectizeInput("metric_id", "Metric", choices = values, selected = "duration_above_threshold", options = list(placeholder = "Search metric or function")),
        shiny::uiOutput("metric_definition"),
        shiny::uiOutput("metric_parameters"),
        shiny::actionButton("add_metric", "Add metric to recipe", class = "btn btn-primary")
      ),
      bslib::card(
        bslib::card_header("Calculated metric outputs"),
        shiny::uiOutput("metric_results"),
        shiny::downloadButton("download_metrics_csv", "Download CSV", class = "btn btn-outline-primary"),
        shiny::downloadButton("download_metrics_rds", "Download RDS", class = "btn btn-outline-primary")
      )
    )
  )
}

llw_visualize_screen <- function(dataset) {
  types <- llw_plot_types()
  ids <- unique(as.character(dataset$prepared_data$Id))
  state_columns <- intersect(names(dataset$prepared_data), names(dataset$annotations))
  shiny::tags$section(
    class = "llw-screen",
    llw_screen_heading("Visualize", "Match a view to the exploratory task.", "Figures are built by the same `llw_plot()` function available from the console."),
    bslib::card(
      bslib::card_header(
        shiny::selectInput("viz_type", "View", choices = stats::setNames(types$id, types$name), selected = "timeline", width = "190px"),
        shiny::selectInput("viz_id", "Stream", choices = c("All", ids), selected = ids[[1]], width = "180px"),
        shiny::selectInput("viz_scale", "Scale", choices = c("Symlog" = "symlog", "Linear" = "identity"), width = "140px"),
        shiny::selectInput("viz_state", "State", choices = c("None" = "", state_columns), width = "160px")
      ),
      shiny::tags$div(
        role = "img",
        `aria-label` = "Selected light exposure visualization",
        shiny::plotOutput("visual_plot", height = "560px")
      ),
      shiny::tags$div(
        class = "llw-download-row",
        shiny::actionButton("add_visualization", "Add view to recipe", class = "btn btn-primary"),
        shiny::downloadButton("download_plot_png", "PNG", class = "btn btn-light"),
        shiny::downloadButton("download_plot_svg", "SVG", class = "btn btn-light"),
        shiny::downloadButton("download_plot_pdf", "PDF", class = "btn btn-light"),
        shiny::downloadButton("download_plot_data", "Plotted data", class = "btn btn-light")
      )
    )
  )
}

llw_metadata_screen <- function(dataset) {
  numeric <- names(dataset$raw_data)[vapply(dataset$raw_data, is.numeric, logical(1))]
  coords <- dataset$metadata$coordinates %||% c(NA_real_, NA_real_)
  shiny::tags$section(
    class = "llw-screen",
    llw_screen_heading("Metadata", "Keep measurement context with the analysis.", "Variable, units, timezone, device, and location feed validation, labels, photoperiod, and exports."),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Dataset and quantity"),
        shiny::textInput("metadata_name", "Dataset name", dataset$name),
        shiny::selectInput("metadata_variable", "Primary variable", choices = numeric, selected = dataset$metadata$variable),
        shiny::textInput("metadata_variable_name", "Variable label", dataset$metadata$variable_name %||% dataset$metadata$variable),
        shiny::textInput("metadata_unit", "Unit", dataset$metadata$variable_unit %||% ""),
        shiny::textInput("metadata_device", "Device", dataset$metadata$device %||% "")
      ),
      bslib::card(
        bslib::card_header("Time and location"),
        shiny::selectizeInput("metadata_timezone", "Time zone", choices = OlsonNames(), selected = dataset$metadata$timezone),
        bslib::layout_columns(col_widths = c(6, 6), shiny::numericInput("metadata_lat", "Latitude", coords[[1]], min = -90, max = 90), shiny::numericInput("metadata_lon", "Longitude", coords[[2]], min = -180, max = 180)),
        shiny::textInput("metadata_site", "Site / city", dataset$metadata$site %||% ""),
        shiny::textInput("metadata_country", "Country", dataset$metadata$country %||% "")
      )
    ),
    shiny::actionButton("save_metadata", "Save metadata", class = "btn btn-primary")
  )
}

llw_recipe_screen <- function(recipe) {
  ids <- vapply(recipe$steps, `[[`, character(1), "id")
  labels <- if (length(ids)) stats::setNames(ids, vapply(recipe$steps, function(step) paste(step$label, if (step$enabled) "" else "(disabled)"), character(1))) else character()
  shiny::tags$section(
    class = "llw-screen",
    llw_screen_heading("Analysis recipe", "Every decision, in execution order.", "Steps can be disabled, moved, removed, undone, redone, or exported as readable R code."),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header("Recipe steps"),
        shiny::selectInput("recipe_step", "Selected step", choices = labels),
        shiny::uiOutput("recipe_step_detail"),
        shiny::tags$div(
          class = "llw-button-grid",
          shiny::actionButton("recipe_toggle", "Enable / disable"),
          shiny::actionButton("recipe_up", "Move up"),
          shiny::actionButton("recipe_down", "Move down"),
          shiny::actionButton("recipe_remove", "Remove", class = "btn btn-outline-danger"),
          shiny::actionButton("recipe_undo", "Undo"),
          shiny::actionButton("recipe_redo", "Redo")
        )
      ),
      bslib::card(
        bslib::card_header("Reproducible R handoff"),
        shiny::tags$pre(class = "llw-code", shiny::textOutput("script_preview")),
        shiny::tags$div(
          class = "llw-download-row",
          shiny::downloadButton("download_recipe_json", "Recipe JSON", class = "btn btn-light"),
          shiny::downloadButton("download_manifest", "Analysis manifest", class = "btn btn-light")
        )
      )
    )
  )
}

llw_projects_screen <- function(datasets, selected) {
  other <- setdiff(names(datasets), selected)
  shiny::tags$section(
    class = "llw-screen",
    llw_screen_heading("Projects & datasets", "Manage data without hidden server storage.", "Projects are local ZIP bundles. Raw participant data are excluded unless explicitly requested."),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Dataset library"),
        shiny::uiOutput("dataset_library"),
        shiny::textInput("rename_dataset_value", "Rename active dataset", selected %||% ""),
        shiny::actionButton("rename_dataset_action", "Rename"),
        shiny::selectInput("merge_dataset_value", "Merge with", choices = other),
        shiny::actionButton("merge_dataset_action", "Merge compatible datasets"),
        shiny::actionButton("delete_dataset_action", "Delete active dataset", class = "btn btn-outline-danger")
      ),
      bslib::card(
        bslib::card_header("Local project bundle"),
        shiny::checkboxInput("project_include_data", "Include raw and prepared participant data", FALSE),
        shiny::downloadButton("download_project", "Save .llw project", class = "btn btn-primary"),
        shiny::tags$hr(),
        shiny::fileInput("project_file", "Load .llw project", accept = ".llw"),
        shiny::actionButton("load_project", "Load project", class = "btn btn-outline-primary"),
        shiny::uiOutput("project_status")
      )
    )
  )
}

llw_table_html <- function(data, max_rows = 20L) {
  if (!is.data.frame(data) || !nrow(data)) return(shiny::tags$p(class = "text-muted", "No rows to display."))
  shown <- utils::head(data, max_rows)
  shiny::tags$div(
    class = "table-responsive",
    shiny::tags$table(
      class = "table table-sm llw-result-table",
      shiny::tags$thead(shiny::tags$tr(lapply(names(shown), shiny::tags$th))),
      shiny::tags$tbody(lapply(seq_len(nrow(shown)), function(i) shiny::tags$tr(lapply(shown[i, , drop = TRUE], function(value) shiny::tags$td(format(value))))))
    ),
    if (nrow(data) > max_rows) shiny::tags$p(class = "form-text", paste("Showing", max_rows, "of", nrow(data), "rows."))
  )
}

llw_metric_input_id <- function(name) paste0("metric_parameter__", gsub("[^A-Za-z0-9]", "_", name))

llw_metric_parameter_control <- function(name, value) {
  id <- llw_metric_input_id(name)
  label <- gsub("\\.", " ", tools::toTitleCase(name))
  if (name == "comparison") return(shiny::selectInput(id, label, choices = c("Above" = "above", "Below" = "below", "Within range" = "within"), selected = value))
  if (name == "period") return(shiny::selectInput(id, label, choices = c("Brightest" = "brightest", "Darkest" = "darkest"), selected = value))
  if (name == "threshold") return(shiny::textInput(id, label, paste(value, collapse = ", "), placeholder = "250, or lower, upper for within"))
  if (is.logical(value)) return(shiny::checkboxInput(id, label, isTRUE(value)))
  if (is.numeric(value)) return(shiny::numericInput(id, label, value))
  shiny::textInput(id, label, value = value %||% "", placeholder = if (is.null(value)) "Optional" else NULL)
}

llw_metric_params_from_input <- function(input, id) {
  defaults <- llw_metric_definition(id)$defaults[[1]]
  parameters <- lapply(names(defaults), function(name) {
    default <- defaults[[name]]
    value <- input[[llw_metric_input_id(name)]]
    if (is.null(value)) return(default)
    if (name == "threshold") {
      parsed <- suppressWarnings(as.numeric(trimws(strsplit(value, ",", fixed = TRUE)[[1]])))
      if (!length(parsed) || anyNA(parsed)) llw_abort("Threshold must contain one number, or two comma-separated numbers for a range.")
      return(parsed)
    }
    if (is.null(default)) return(if (is.character(value) && !nzchar(value)) NULL else value)
    if (is.logical(default)) return(isTRUE(value))
    if (is.numeric(default)) return(as.numeric(value))
    as.character(value)
  })
  names(parameters) <- names(defaults)
  parameters
}

llw_shiny_app <- function(modules = list(), project = NULL, ...) {
  modules <- llw_validate_modules(modules)
  resource <- system.file("app/www", package = "LightLogWeb")
  if (nzchar(resource)) try(shiny::addResourcePath("lightlogweb", resource), silent = TRUE)
  ui <- bslib::page_fillable(
    theme = llw_app_theme(),
    padding = 0,
    gap = 0,
    fillable_mobile = TRUE,
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "lightlogweb/styles.css"),
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    ),
    shiny::uiOutput("llw_root", fill = TRUE)
  )

  server <- function(input, output, session) {
    datasets <- shiny::reactiveVal(list())
    selected <- shiny::reactiveVal(NULL)
    recipe <- shiny::reactiveVal(llw_recipe())
    mode <- shiny::reactiveVal("landing")
    screen <- shiny::reactiveVal("overview")
    import_result <- shiny::reactiveVal(NULL)
    project_status <- shiny::reactiveVal(NULL)
    published_modules <- shiny::reactiveVal(list())

    set_dataset <- function(value, select = TRUE) {
      all <- datasets()
      all[[value$name]] <- value
      datasets(all)
      if (select) selected(value$name)
    }

    current_dataset <- shiny::reactive({
      shiny::req(selected())
      datasets()[[selected()]]
    })
    current_analysis <- shiny::reactive({
      shiny::req(current_dataset())
      llw_run(current_dataset(), recipe(), modules)
    })

    initial_project <- tryCatch({
      if (inherits(project, "llw_project")) project else if (is.character(project) && length(project) == 1L) llw_load_project(project, modules) else NULL
    }, error = function(error) error)
    if (inherits(initial_project, "llw_project")) {
      recipe(initial_project$recipe)
      if (!is.null(initial_project$dataset)) set_dataset(initial_project$dataset)
      project_status(initial_project)
      mode("workspace")
      screen(if (is.null(initial_project$dataset)) "projects" else "overview")
    }

    output$llw_root <- shiny::renderUI({
      if (mode() == "landing") return(llw_landing_ui())
      llw_workspace_ui(names(datasets()), selected(), screen(), modules)
    })

    observe_to <- function(event, value) shiny::observeEvent(event, { screen(value) }, ignoreInit = TRUE)
    shiny::observeEvent(input$workflow, screen(input$workflow), ignoreInit = TRUE)
    shiny::observeEvent(input$active_dataset, { if (nzchar(input$active_dataset %||% "")) selected(input$active_dataset) }, ignoreInit = TRUE)
    shiny::observeEvent(input$home, mode("landing"), ignoreInit = TRUE)
    shiny::observeEvent(input$header_recipe, screen("recipe"), ignoreInit = TRUE)
    shiny::observeEvent(input$empty_import, screen("import"), ignoreInit = TRUE)
    shiny::observeEvent(input$landing_import, { mode("workspace"); screen("import") }, ignoreInit = TRUE)
    shiny::observeEvent(input$start_import, { mode("workspace"); screen("import") }, ignoreInit = TRUE)
    shiny::observeEvent(input$explore_demo, {
      demo <- llw_dataset(
        LightLogR::sample.data.environment,
        metadata = list(variable = "MEDI", variable_name = "Melanopic EDI", variable_unit = "lx", timezone = "Europe/Berlin", coordinates = c(48.52, 9.06), device = "ActLumus", site = "Tuebingen", country = "Germany"),
        name = "sample.data.environment",
        provenance = list(source_type = "LightLogR bundled sample", citation = "https://tscnlab.github.io/LightLogR/")
      )
      set_dataset(demo)
      recipe(llw_recipe())
      mode("workspace")
      screen("overview")
    }, ignoreInit = TRUE)

    output$screen <- shiny::renderUI({
      value <- screen()
      if (value == "import") return(llw_import_screen())
      if (value == "projects") return(llw_projects_screen(datasets(), selected()))
      if (is.null(selected()) || is.null(datasets()[[selected()]])) return(llw_no_dataset_ui())
      dataset <- current_analysis()$dataset
      if (startsWith(value, "module__")) {
        id <- sub("^module__", "", value)
        module <- modules[[id]]
        return(shiny::tags$section(class = "llw-screen", llw_screen_heading("Custom analysis", module$title, if (!is.null(module$docs_url)) shiny::tags$a("Module documentation", href = module$docs_url, target = "_blank", rel = "noopener")), module$ui(module$id)))
      }
      switch(
        value,
        overview = llw_overview_screen(dataset),
        prepare = llw_prepare_screen(dataset),
        group = llw_group_screen(dataset),
        metrics = llw_metrics_screen(),
        visualize = llw_visualize_screen(dataset),
        metadata = llw_metadata_screen(current_dataset()),
        raw = shiny::tags$section(
          class = "llw-screen",
          llw_screen_heading("Raw data", "Verify the source values.", "The table is read-only; transformations occur in separate stages."),
          DT::DTOutput("raw_table"),
          shiny::tags$div(
            class = "llw-download-row",
            shiny::downloadButton("download_raw", "Raw RDS", class = "btn btn-light"),
            shiny::downloadButton("download_prepared_csv", "Prepared CSV", class = "btn btn-light"),
            shiny::downloadButton("download_prepared_rds", "Prepared RDS", class = "btn btn-light")
          )
        ),
        recipe = llw_recipe_screen(recipe()),
        llw_overview_screen(dataset)
      )
    })

    output$import_device_options <- shiny::renderUI({
      device <- input$import_device %||% "normalized"
      if (device == "normalized") return(shiny::tags$p(class = "form-text", "Normalized input must contain Id, POSIXct-compatible Datetime, and a numeric measurement column."))
      versions <- LightLogR::supported_versions(device)
      ui <- list(shiny::selectInput("import_version", "File-format version", choices = c("Default" = "default", stats::setNames(versions$Version, versions$Version))))
      if (device == "VEET") ui <- c(ui, list(shiny::selectInput("import_veet_modality", "VEET modality", choices = c("ALS", "IMU", "INF", "PHO", "TOF"), selected = "ALS")))
      shiny::tagList(ui)
    })
    output$import_id_preview <- shiny::renderUI({
      shiny::req(input$import_files)
      preview <- llw_preview_ids(
        input$import_files$name,
        mode = input$import_id_mode %||% "auto",
        pattern = input$import_id_regex %||% "^([^_]+)",
        manual_id = input$import_manual_id %||% "Participant"
      )
      shiny::tags$div(
        class = "llw-id-preview",
        shiny::tags$strong("Participant-ID preview"),
        llw_table_html(preview, max_rows = 5)
      )
    })

    shiny::observeEvent(input$run_import, {
      shiny::req(input$import_files)
      tryCatch({
        upload_dir <- tempfile("llw-upload-")
        dir.create(upload_dir)
        files <- file.path(upload_dir, input$import_files$name)
        ok <- file.copy(input$import_files$datapath, files, overwrite = TRUE)
        if (!all(ok)) llw_abort("One or more uploaded files could not be prepared for import.")
        args <- list(
          files = files,
          device = input$import_device,
          variable = input$import_variable,
          name = input$import_name,
          timezone = input$import_tz
        )
        if (input$import_device != "normalized") {
          args$version <- if (identical(input$import_version, "default")) NULL else input$import_version
          args$dst_adjustment <- isTRUE(input$import_dst)
          args$remove_duplicates <- isTRUE(input$import_remove_duplicates)
          if (input$import_id_mode == "manual") args$manual.id <- input$import_manual_id
          if (input$import_id_mode == "extract") args$auto.id <- input$import_id_regex
          if (input$import_id_mode == "auto") args$auto.id <- ".*"
          if (input$import_device == "VEET") args$modality <- input$import_veet_modality
        }
        imported <- rlang::exec(llw_import, !!!args)
        imported$provenance$files$files <- input$import_files$name
        loaded_project <- project_status()
        if (inherits(loaded_project, "llw_project") && is.null(loaded_project$dataset)) {
          loaded_project <- llw_relink_project(loaded_project, imported)
          imported <- loaded_project$dataset
          recipe(loaded_project$recipe)
          project_status(loaded_project)
        } else {
          recipe(llw_recipe())
        }
        import_result(imported)
        set_dataset(imported)
        shiny::showNotification(paste("Imported", imported$name), type = "message")
        screen("overview")
      }, error = function(error) shiny::showNotification(conditionMessage(error), type = "error", duration = NULL))
    })
    output$import_report <- shiny::renderUI({
      result <- import_result()
      if (is.null(result)) return(shiny::tags$div(class = "llw-empty-small", bsicons::bs_icon("file-earmark-arrow-up"), shiny::tags$p("Choose files and import settings to see validation results.")))
      quality <- llw_quality(result)$value$overview
      shiny::tags$div(
        class = "llw-stat-grid",
        shiny::tags$div(shiny::tags$span("Rows"), shiny::tags$strong(format(quality$rows, big.mark = ","))),
        shiny::tags$div(shiny::tags$span("IDs"), shiny::tags$strong(quality$ids)),
        shiny::tags$div(shiny::tags$span("Missing"), shiny::tags$strong(scales::percent(quality$explicit_missing))),
        shiny::tags$div(shiny::tags$span("Duplicates"), shiny::tags$strong(quality$duplicate_rows))
      )
    })
    output$import_preview <- DT::renderDT({ shiny::req(import_result()); DT::datatable(utils::head(import_result()$raw_data, 200), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE) })

    output$overview_values <- shiny::renderUI({
      q <- current_analysis()$quality$value
      o <- q$overview
      bslib::layout_column_wrap(
        width = 1 / 4,
        bslib::value_box("IDs", o$ids, showcase = bsicons::bs_icon("people"), paste(o$participant_days, "participant-days"), class = "llw-value-box"),
        bslib::value_box("Observations", format(o$rows, big.mark = ","), showcase = bsicons::bs_icon("grid-3x3"), paste(format(o$start, "%d %b"), "-", format(o$end, "%d %b")), class = "llw-value-box"),
        bslib::value_box("Daily coverage", scales::percent(o$median_daily_coverage, accuracy = 1), showcase = bsicons::bs_icon("circle-half"), "median participant-day", class = "llw-value-box"),
        bslib::value_box("Missing readings", scales::percent(o$explicit_missing, accuracy = 0.1), showcase = bsicons::bs_icon("exclamation-circle"), paste(o$invalidated_rows, "invalidated"), class = "llw-value-box")
      )
    })
    output$quality_strip <- shiny::renderUI({
      o <- current_analysis()$quality$value$overview
      item <- function(label, ok, good, bad) shiny::tags$div(class = paste("llw-quality-item", if (isTRUE(ok)) "is-good" else "is-warn"), if (isTRUE(ok)) bsicons::bs_icon("check-circle") else bsicons::bs_icon("exclamation-triangle"), shiny::tags$span(label), shiny::tags$strong(if (isTRUE(ok)) good else bad))
      shiny::tagList(
        item("Implicit gaps", !isTRUE(o$has_gaps), "None detected", "Review required"),
        item("Irregular timestamps", !isTRUE(o$has_irregulars), "None detected", "Review required"),
        item("Duplicate timestamps", o$duplicate_rows == 0, "None detected", paste(o$duplicate_rows, "rows")),
        item("Explicit missingness", o$explicit_missing == 0, "None", scales::percent(o$explicit_missing))
      )
    })
    output$overview_plot <- shiny::renderPlot({
      id <- input$overview_id %||% unique(as.character(current_analysis()$dataset$prepared_data$Id))[[1]]
      llw_plot(current_analysis()$dataset, "timeline", id = id)$value
    }, res = 110)
    output$availability_table <- DT::renderDT({
      DT::datatable(current_analysis()$quality$value$by_day, options = list(pageLength = 12, dom = "tip", scrollX = TRUE), rownames = FALSE)
    })

    preparation_parameters <- shiny::reactive({
      list(
        timezone = input$prep_timezone,
        timezone_mode = input$prep_timezone_mode %||% "preserve_instant",
        date_range = input$prep_dates,
        value_range = c(input$prep_min, input$prep_max),
        invalidate_outside = isTRUE(input$prep_invalidate),
        gap_policy = input$prep_gaps %||% "explicit_na",
        interval = if (nzchar(input$prep_interval %||% "")) input$prep_interval else NULL,
        aggregate_function = input$prep_function %||% "mean",
        daily_missing_max = (input$prep_missing %||% 20) / 100
      )
    })
    preparation_preview <- shiny::reactive({ rlang::exec(llw_prepare, x = current_analysis()$dataset, !!!preparation_parameters()) })
    output$prepare_preview_stats <- shiny::renderUI({
      preview <- preparation_preview()
      q <- llw_quality(preview)$value$overview
      shiny::tags$div(class = "llw-stat-grid", shiny::tags$div(shiny::tags$span("Rows"), shiny::tags$strong(format(q$rows, big.mark = ","))), shiny::tags$div(shiny::tags$span("Coverage"), shiny::tags$strong(scales::percent(q$median_daily_coverage))), shiny::tags$div(shiny::tags$span("Invalidated"), shiny::tags$strong(q$invalidated_rows)))
    })
    output$prepare_preview_plot <- shiny::renderPlot({ llw_plot(preparation_preview(), "timeline", id = unique(as.character(preparation_preview()$prepared_data$Id))[[1]])$value }, res = 100)
    shiny::observeEvent(input$add_preparation, {
      tryCatch({ recipe(llw_recipe_add(recipe(), "preparation", preparation_parameters(), label = "Prepare data")); shiny::showNotification("Preparation added to the recipe.", type = "message") }, error = function(error) shiny::showNotification(conditionMessage(error), type = "error"))
    })
    shiny::observeEvent(input$add_annotation, {
      shiny::req(input$annotation_file)
      tryCatch({
        intervals <- utils::read.csv(input$annotation_file$datapath, stringsAsFactors = FALSE)
        parameters <- list(intervals = intervals, output_col = input$annotation_output %||% "State", overwrite = TRUE)
        recipe(llw_recipe_add(recipe(), "annotation", parameters, label = paste("Annotate", input$annotation_output)))
        shiny::showNotification("Interval annotation added to the recipe.", type = "message")
      }, error = function(error) shiny::showNotification(conditionMessage(error), type = "error"))
    })

    grouping_parameters <- shiny::reactive({
      dimensions <- input$group_dimensions %||% c("participant", "date")
      p <- list(dimensions = dimensions)
      if ("clock_window" %in% dimensions) p$clock_window <- c(input$group_clock_start, input$group_clock_end)
      if ("photoperiod" %in% dimensions) { p$coordinates <- c(input$group_lat, input$group_lon); p$solar_depression <- input$group_solar }
      if ("annotation" %in% dimensions) p$annotation <- input$group_annotation
      p
    })
    grouping_preview <- shiny::reactive({ rlang::exec(llw_group, x = current_analysis()$dataset, !!!grouping_parameters()) })
    output$group_preview <- shiny::renderUI({
      preview <- grouping_preview()
      counts <- preview$prepared_data |> dplyr::count(dplyr::across(dplyr::all_of(preview$groups)), name = "observations")
      shiny::tagList(shiny::tags$p(class = "llw-group-pill", paste("Grouped by", paste(preview$groups, collapse = " - "))), llw_table_html(counts))
    })
    shiny::observeEvent(input$add_grouping, {
      tryCatch({ recipe(llw_recipe_add(recipe(), "grouping", grouping_parameters(), label = "Define analysis groups")); shiny::showNotification("Grouping added to the recipe.", type = "message") }, error = function(error) shiny::showNotification(conditionMessage(error), type = "error"))
    })

    output$metric_definition <- shiny::renderUI({
      row <- llw_metric_definition(input$metric_id %||% "duration_above_threshold")
      shiny::tags$article(
        class = "llw-metric-definition",
        shiny::tags$p(class = "llw-eyebrow", row$family),
        shiny::tags$h3(row$name),
        shiny::tags$p(row$summary),
        shiny::tags$div(shiny::tags$span(row$requirement), shiny::tags$span(row$units)),
        shiny::tags$p(class = "form-text", row$grouping),
        shiny::tags$p(class = "form-text", row$caution),
        shiny::tags$a("Open LightLogR function documentation", href = row$documentation, target = "_blank", rel = "noopener")
      )
    })
    output$metric_parameters <- shiny::renderUI({
      id <- input$metric_id %||% "duration_above_threshold"
      defaults <- llw_metric_definition(id)$defaults[[1]]
      shiny::tagList(lapply(names(defaults), function(name) llw_metric_parameter_control(name, defaults[[name]])))
    })
    shiny::observeEvent(input$add_metric, {
      id <- input$metric_id %||% "duration_above_threshold"
      params <- llw_metric_params_from_input(input, id)
      tryCatch({
        recipe(llw_recipe_add(recipe(), "metric", list(metrics = list(list(id = id, parameters = params))), label = llw_metric_definition(id)$name[[1]]))
        shiny::showNotification(paste(llw_metric_definition(id)$name[[1]], "added to the recipe."), type = "message")
      }, error = function(error) shiny::showNotification(conditionMessage(error), type = "error"))
    })
    all_metric_tables <- shiny::reactive({
      results <- current_analysis()$metrics
      unlist(lapply(names(results), function(step_id) {
        result <- results[[step_id]]
        stats::setNames(result$value, paste(step_id, names(result$value), sep = "::"))
      }), recursive = FALSE)
    })
    output$metric_results <- shiny::renderUI({
      tables <- all_metric_tables()
      if (!length(tables)) return(shiny::tags$div(class = "llw-empty-small", bsicons::bs_icon("calculator"), shiny::tags$p("Add a metric to the recipe to calculate results.")))
      shiny::tagList(lapply(names(tables), function(name) shiny::tags$section(class = "llw-result-section", shiny::tags$h3(sub("^.*::", "", name)), llw_table_html(tables[[name]], 12))))
    })

    current_plot <- shiny::reactive({
      llw_plot(
        current_analysis()$dataset,
        type = input$viz_type %||% "timeline",
        id = input$viz_id %||% unique(as.character(current_analysis()$dataset$prepared_data$Id))[[1]],
        scale = input$viz_scale %||% "symlog",
        state = if (nzchar(input$viz_state %||% "")) input$viz_state else NULL
      )
    })
    output$visual_plot <- shiny::renderPlot({ current_plot()$value }, res = 110)
    shiny::observeEvent(input$add_visualization, {
      p <- current_plot()$parameters
      recipe(llw_recipe_add(recipe(), "visualization", p, label = current_plot()$label))
      shiny::showNotification("Visualization added to the recipe.", type = "message")
    })

    output$raw_table <- DT::renderDT({ DT::datatable(current_dataset()$raw_data, filter = "top", options = list(pageLength = 20, scrollX = TRUE, deferRender = TRUE), rownames = FALSE) })
    shiny::observeEvent(input$save_metadata, {
      tryCatch({
        old <- current_dataset()
        old$name <- input$metadata_name
        old$metadata <- utils::modifyList(old$metadata, list(variable = input$metadata_variable, variable_name = input$metadata_variable_name, variable_unit = input$metadata_unit, timezone = input$metadata_timezone, coordinates = c(input$metadata_lat, input$metadata_lon), device = input$metadata_device, site = input$metadata_site, country = input$metadata_country))
        all <- datasets(); all[[selected()]] <- NULL; all[[old$name]] <- old; datasets(all); selected(old$name)
        shiny::showNotification("Metadata saved.", type = "message")
      }, error = function(error) shiny::showNotification(conditionMessage(error), type = "error"))
    })

    output$recipe_step_detail <- shiny::renderUI({
      id <- input$recipe_step
      if (is.null(id) || !nzchar(id)) return(shiny::tags$p(class = "text-muted", "The recipe has no steps."))
      step <- recipe()$steps[[match(id, vapply(recipe()$steps, `[[`, character(1), "id"))]]
      shiny::tagList(shiny::tags$p(shiny::tags$strong(step$type), " - ", if (step$enabled) "enabled" else "disabled"), shiny::tags$pre(class = "llw-code llw-code-small", llw_dput(step$parameters)))
    })
    output$script_preview <- shiny::renderText(paste(llw_build_script(recipe()), collapse = "\n"))
    shiny::observeEvent(input$recipe_toggle, { id <- input$recipe_step; if (nzchar(id %||% "")) { step <- recipe()$steps[[match(id, vapply(recipe()$steps, `[[`, character(1), "id"))]]; recipe(llw_recipe_update(recipe(), id, enabled = !step$enabled)) } })
    shiny::observeEvent(input$recipe_remove, { if (nzchar(input$recipe_step %||% "")) recipe(llw_recipe_remove(recipe(), input$recipe_step)) })
    shiny::observeEvent(input$recipe_up, { if (nzchar(input$recipe_step %||% "")) recipe(llw_recipe_move(recipe(), input$recipe_step, "up")) })
    shiny::observeEvent(input$recipe_down, { if (nzchar(input$recipe_step %||% "")) recipe(llw_recipe_move(recipe(), input$recipe_step, "down")) })
    shiny::observeEvent(input$recipe_undo, recipe(llw_recipe_undo(recipe())))
    shiny::observeEvent(input$recipe_redo, recipe(llw_recipe_redo(recipe())))

    output$dataset_library <- shiny::renderUI({
      if (!length(datasets())) return(shiny::tags$p("No datasets in this session."))
      shiny::tags$ul(class = "llw-dataset-list", lapply(names(datasets()), function(name) shiny::tags$li(class = if (identical(name, selected())) "active" else NULL, shiny::tags$strong(name), shiny::tags$span(paste(format(nrow(datasets()[[name]]$raw_data), big.mark = ","), "rows")))))
    })
    shiny::observeEvent(input$rename_dataset_action, {
      shiny::req(selected(), nzchar(input$rename_dataset_value))
      all <- datasets(); value <- all[[selected()]]; all[[selected()]] <- NULL; value$name <- input$rename_dataset_value; all[[value$name]] <- value; datasets(all); selected(value$name)
    })
    shiny::observeEvent(input$delete_dataset_action, {
      shiny::req(selected()); all <- datasets(); all[[selected()]] <- NULL; datasets(all); selected(names(all)[[1]] %||% NULL); if (!length(all)) screen("import")
    })
    shiny::observeEvent(input$merge_dataset_action, {
      shiny::req(selected(), input$merge_dataset_value)
      tryCatch({ set_dataset(llw_merge(datasets()[[selected()]], datasets()[[input$merge_dataset_value]])); shiny::showNotification("Datasets merged.", type = "message") }, error = function(error) shiny::showNotification(conditionMessage(error), type = "error"))
    })
    shiny::observeEvent(input$load_project, {
      shiny::req(input$project_file)
      tryCatch({
        loaded <- llw_load_project(input$project_file$datapath, modules)
        project_status(loaded); recipe(loaded$recipe)
        if (!is.null(loaded$dataset)) { set_dataset(loaded$dataset); screen("overview") }
        shiny::showNotification(if (is.null(loaded$dataset)) "Project recipe loaded; relink the source data to continue." else "Project loaded.", type = if (is.null(loaded$dataset)) "warning" else "message")
      }, error = function(error) shiny::showNotification(conditionMessage(error), type = "error", duration = NULL))
    })
    output$project_status <- shiny::renderUI({
      status <- project_status()
      if (is.null(status)) return(shiny::tags$p(class = "text-muted", "No project loaded."))
      shiny::tagList(shiny::tags$p(shiny::tags$strong(status$manifest$dataset_name), " - schema ", status$manifest$schema_version), if (length(status$warnings)) shiny::tags$ul(class = "text-warning", lapply(status$warnings, shiny::tags$li)))
    })

    for (module in modules) local({
      current_module <- module
      context <- list(
        raw = shiny::reactive(current_dataset()$raw_data),
        prepared = shiny::reactive(current_analysis()$dataset$prepared_data),
        grouped = shiny::reactive(current_analysis()$dataset$prepared_data),
        metadata = shiny::reactive(current_analysis()$dataset$metadata),
        recipe = shiny::reactive(recipe()),
        selection = shiny::reactive(list(dataset = selected(), screen = screen())),
        commit = function(params, label = current_module$title) {
          recipe(llw_recipe_add(recipe(), "module", params, label = label, module_id = current_module$id, module_version = current_module$version))
        },
        publish = function(result) {
          if (!inherits(result, "llw_result")) llw_abort("Published module outputs must be `llw_result` objects.")
          values <- published_modules(); values[[current_module$id]] <- result; published_modules(values); invisible(result)
        }
      )
      current_module$server(current_module$id, context)
    })

    metric_csv <- function() {
      tables <- all_metric_tables()
      if (!length(tables)) return(tibble::tibble())
      dplyr::bind_rows(lapply(names(tables), function(name) dplyr::mutate(tables[[name]], .metric = sub("^.*::", "", name), .step = sub("::.*$", "", name), .before = 1)))
    }
    output$download_script <- shiny::downloadHandler(filename = function() "lightlogweb-analysis.R", contentType = "text/plain", content = function(file) writeLines(llw_build_script(recipe()), file, useBytes = TRUE))
    output$download_recipe_json <- shiny::downloadHandler(filename = function() "lightlogweb-recipe.json", contentType = "application/json", content = function(file) llw_write_json(recipe()$steps, file))
    output$download_manifest <- shiny::downloadHandler(filename = function() "lightlogweb-manifest.json", contentType = "application/json", content = function(file) llw_export(current_analysis(), file, "json"))
    output$download_metrics_csv <- shiny::downloadHandler(filename = function() "lightlogweb-metrics.csv", content = function(file) llw_export(metric_csv(), file, "csv"))
    output$download_metrics_rds <- shiny::downloadHandler(filename = function() "lightlogweb-metrics.rds", content = function(file) llw_export(current_analysis()$metrics, file, "rds"))
    output$download_raw <- shiny::downloadHandler(filename = function() paste0(selected(), "-raw.rds"), content = function(file) llw_export(current_dataset()$raw_data, file, "rds"))
    output$download_prepared_csv <- shiny::downloadHandler(filename = function() paste0(selected(), "-prepared.csv"), content = function(file) llw_export(current_analysis()$dataset$prepared_data, file, "csv"))
    output$download_prepared_rds <- shiny::downloadHandler(filename = function() paste0(selected(), "-prepared.rds"), content = function(file) llw_export(current_analysis()$dataset$prepared_data, file, "rds"))
    output$download_plot_png <- shiny::downloadHandler(filename = function() paste0(input$viz_type %||% "plot", ".png"), content = function(file) llw_export(current_plot(), file, "png"))
    output$download_plot_svg <- shiny::downloadHandler(filename = function() paste0(input$viz_type %||% "plot", ".svg"), content = function(file) llw_export(current_plot(), file, "svg"))
    output$download_plot_pdf <- shiny::downloadHandler(filename = function() paste0(input$viz_type %||% "plot", ".pdf"), content = function(file) llw_export(current_plot(), file, "pdf"))
    output$download_plot_data <- shiny::downloadHandler(filename = function() paste0(input$viz_type %||% "plot", "-data.csv"), content = function(file) llw_export(current_plot(), file, "csv"))
    output$download_project <- shiny::downloadHandler(filename = function() paste0(selected() %||% "lightlogweb", ".llw"), contentType = "application/zip", content = function(file) llw_save_project(current_analysis(), path = file, include_data = isTRUE(input$project_include_data)))
  }
  shiny::shinyApp(ui, server, ...)
}
