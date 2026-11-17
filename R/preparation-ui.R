# Link the method a user can inspect, not general-purpose plumbing. Some
# operations use local adapters; their descriptions explicitly say so.
preparation_operation_documentation <- function(form) {
  reference <- function(label, url, note = NULL)
    list(
      label = label,
      url = url,
      note = note
    )
  lightlogr <- "https://tscnlab.github.io/LightLogR/reference/"
  base_r <- "https://stat.ethz.ch/R-manual/R-devel/library/base/html/"
  method <- switch(
    form$step_type,
    dates = reference(
      "LightLogR::filter_Date()",
      paste0(lightlogr, "filter_Datetime.html")
    ),
    datetimes = reference(
      "LightLogR::filter_Datetime()",
      paste0(lightlogr, "filter_Datetime.html")
    ),
    duration = reference(
      "LightLogR::filter_Datetime()",
      paste0(lightlogr, "filter_Datetime.html"),
      "The app calculates the selected duration bounds, then filters the timestamps."
    ),
    photoperiod = if (identical(form$photo_method, "coordinates"))
      reference(
        "LightLogR::add_photoperiod()",
        paste0(lightlogr, "photoperiod.html")
      ),
    coverage = reference(
      "LightLogR::remove_partial_data()",
      paste0(lightlogr, "remove_partial_data.html")
    ),
    transform = switch(
      form$measurement_operation,
      log = reference("base::log()", paste0(base_r, "Log.html")),
      root = reference(
        "R powers (^)",
        paste0(base_r, "Arithmetic.html"),
        "Roots use a reciprocal power; the app also supports negative values for odd root degrees."
      ),
      power = reference("R powers (^)", paste0(base_r, "Arithmetic.html")),
      exp = reference("R powers (^)", paste0(base_r, "Arithmetic.html")),
      abs = reference("base::abs()", paste0(base_r, "MathFun.html")),
      zero_inflated = reference(
        paste0(
          "LightLogR::",
          form$zero_operation %||% "log_zero_inflated",
          "()"
        ),
        paste0(lightlogr, "log_zero_inflated.html")
      )
    ),
    variability = reference(
      "stats::sd()",
      "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/sd.html",
      "The app implements the moving windows and coverage checks using sample SD. CV is 100 \u00d7 SD / |window mean|."
    ),
    timezone = if (identical(form$time_method, "force"))
      reference(
        "lubridate::force_tz()",
        "https://lubridate.tidyverse.org/reference/force_tz.html"
      ) else
      reference(
        "lubridate::with_tz()",
        "https://lubridate.tidyverse.org/reference/with_tz.html"
      ),
    dst = reference(
      "LightLogR::dst_change_handler()",
      paste0(lightlogr, "dst_change_handler.html"),
      "Method reference: the app uses its own offset-based adapter, which also supports half-hour changes."
    ),
    gaps = reference(
      "LightLogR::gap_handler()",
      paste0(lightlogr, "gap_handler.html"),
      if (identical(form$gap_bounds, "days"))
        "Method reference: complete local days use the app's day-boundary adapter. First-to-last expansion calls gap_handler() directly."
    ),
    aggregate = reference(
      "LightLogR::aggregate_Datetime()",
      paste0(lightlogr, "aggregate_Datetime.html"),
      "Uses start-labelled bins (type = floor) and the chosen numeric handler. The app reports observed-duration coverage separately; it never fills missing measurements."
    )
  )
  if (is.null(method)) return(NULL)
  preparation_help(
    "Function documentation",
    shiny::tags$p(shiny::tags$a(
      href = method$url,
      target = "_blank",
      rel = "noopener noreferrer",
      method$label,
      shiny::tags$span(class = "visually-hidden", " (opens in a new tab)")
    )),
    if (!is.null(method$note)) shiny::tags$p(method$note)
  )
}

preparation_choice_buttons <- function(
  id,
  label,
  labels,
  values,
  icons,
  selected
) {
  shiny::tags$div(
    class = "llw-prepare-operation",
    shiny::radioButtons(
      id,
      label,
      choiceNames = unname(Map(
        function(text, icon) htmltools::tagList(shiny::icon(icon), text),
        labels,
        icons
      )),
      choiceValues = values,
      selected = selected,
      inline = TRUE,
      width = "100%"
    )
  )
}

preparation_new_filters_ui <- function(ns) {
  missing_choices <- c(
    "Keep missing values" = "keep",
    "Remove missing values" = "drop",
    "Keep only missing values" = "only"
  )
  htmltools::tagList(
    shiny::conditionalPanel(
      "input.step_type === 'variable'",
      ns = ns,
      shiny::selectInput(
        ns("variable_name"),
        "Dataset variable",
        choices = character(),
        width = "100%"
      ),
      shiny::selectInput(
        ns("variable_mode"),
        "Filter using",
        c("Numeric interval" = "range", "Exact categories" = "categories"),
        width = "100%"
      ),
      shiny::conditionalPanel(
        "input.variable_mode === 'categories'",
        ns = ns,
        shiny::selectizeInput(
          ns("variable_values"),
          "Categories",
          choices = NULL,
          multiple = TRUE,
          width = "100%"
        )
      ),
      shiny::conditionalPanel(
        "input.variable_mode === 'range'",
        ns = ns,
        shiny::tags$div(
          class = "llw-prepare-field-pair",
          shiny::numericInput(
            ns("variable_lower"),
            "Lower bound (included)",
            0,
            width = "100%"
          ),
          shiny::numericInput(
            ns("variable_upper"),
            "Upper bound (included)",
            1,
            width = "100%"
          )
        ),
        shiny::tags$p(
          class = "llw-prepare-hint",
          "The interval creates two categories for this filter: inside and outside. Source values stay unchanged."
        )
      ),
      shiny::selectInput(
        ns("variable_condition"),
        "Keep",
        c("Matching values" = "inside", "Other values" = "outside"),
        width = "100%"
      ),
      shiny::selectInput(
        ns("variable_missing"),
        "Missing values (NA)",
        missing_choices,
        width = "100%"
      ),
      shiny::textOutput(ns("variable_help"))
    ),
    shiny::conditionalPanel(
      "input.step_type === 'photoperiod'",
      ns = ns,
      shiny::selectInput(
        ns("photo_method"),
        "Day/night source",
        c("Derive from coordinates" = "coordinates"),
        width = "100%"
      ),
      shiny::selectInput(
        ns("photo_state"),
        "Keep observations during",
        c("Day" = "day", "Night" = "night"),
        width = "100%"
      ),
      shiny::conditionalPanel(
        "input.photo_method === 'coordinates'",
        ns = ns,
        shiny::tags$div(
          class = "llw-prepare-field-pair",
          shiny::numericInput(
            ns("photo_latitude"),
            "Latitude",
            NA_real_,
            min = -90,
            max = 90,
            width = "100%"
          ),
          shiny::numericInput(
            ns("photo_longitude"),
            "Longitude",
            NA_real_,
            min = -180,
            max = 180,
            width = "100%"
          )
        ),
        shiny::selectInput(
          ns("photo_solar"),
          "Solar depression",
          c(
            "Sunrise / sunset \u00b7 0\u00b0" = "0",
            "Civil twilight \u00b7 6\u00b0" = "6",
            "Nautical twilight \u00b7 12\u00b0" = "12",
            "Astronomical twilight \u00b7 18\u00b0" = "18"
          ),
          selected = "6",
          width = "100%"
        ),
        shiny::tags$p(
          class = "llw-prepare-hint",
          "This location applies to all observations reaching this step. Day/night uses the dataset's native time zone. No classification is guessed when dawn or dusk is unavailable."
        )
      ),
      shiny::selectInput(
        ns("photo_missing"),
        "Unknown day/night",
        missing_choices,
        width = "100%"
      )
    ),
    shiny::conditionalPanel(
      "input.step_type === 'coverage'",
      ns = ns,
      shiny::selectInput(
        ns("coverage_variable"),
        "Assess available values in",
        choices = character(),
        width = "100%"
      ),
      shiny::selectInput(
        ns("coverage_mode"),
        "Threshold",
        c(
          "Minimum available percentage" = "percentage",
          "Minimum available duration" = "available_duration",
          "Maximum missing duration" = "missing_duration"
        ),
        width = "100%"
      ),
      shiny::conditionalPanel(
        "input.coverage_mode === 'percentage'",
        ns = ns,
        shiny::numericInput(
          ns("coverage_minimum"),
          "Minimum available data (%)",
          80,
          min = 0,
          max = 100,
          width = "100%"
        )
      ),
      shiny::conditionalPanel(
        "input.coverage_mode !== 'percentage'",
        ns = ns,
        shiny::div(
          class = "llw-prepare-field-pair",
          shiny::numericInput(
            ns("coverage_duration"),
            "Duration",
            16,
            min = 0,
            width = "100%"
          ),
          shiny::selectInput(
            ns("coverage_unit"),
            "Unit",
            c(
              "Seconds" = "seconds",
              "Minutes" = "minutes",
              "Hours" = "hours",
              "Days of 24 hours" = "days"
            ),
            selected = "hours",
            width = "100%"
          )
        )
      ),
      shiny::selectInput(
        ns("coverage_basis"),
        "Coverage basis",
        c(
          "Complete local day, including gaps" = "full_day",
          "Observed span within each day" = "observed_span",
          "Whole group, including gaps" = "full_group",
          "Whole observed group" = "observed_group"
        ),
        width = "100%"
      ),
      shiny::tags$p(
        class = "llw-prepare-hint",
        shiny::textOutput(ns("coverage_basis_help"))
      ),
      preparation_help(
        "How coverage is assessed",
        shiny::tags$p(
          "Uses LightLogR's remove_partial_data with the dataset's current groups. Daily options check each local day within each group. Whole-group options keep or remove the entire group. With no grouping, all rows form one group."
        ),
        shiny::tags$p(
          "Minimum available duration keeps groups or days with at least that much available data. Maximum missing duration keeps those with no more than that much missing data. Equality passes. Minimum available duration must be positive. Duration is calculated from the recording intervals, not a count of rows."
        ),
        shiny::tags$p(
          "Including gaps uses a temporary grid at the group's dominant interval and extends the first and last day to their local midnight boundaries. Missing timestamps and NA values count as missing. Only original observations are returned; no measurements are filled in."
        ),
        shiny::tags$p(
          "Observed options assess the rows reaching this step, without adding unrecorded timestamps or extending to midnight. NA values count as missing. Put coverage before other filters to assess the original recording. Groups with only one observation are removed."
        )
      )
    )
  )
}

preparation_measurements_ui <- function(ns) {
  htmltools::tagList(
    preparation_choice_buttons(
      ns("value_type"),
      "Change measurement values",
      c("Valid range", "Transform", "Variability"),
      c("invalidate", "calibrate", "variability"),
      c("ruler", "sliders", "wave-square"),
      "invalidate"
    ),
    shiny::tags$div(
      class = "llw-prepare-target",
      shiny::tags$div(
        class = "llw-prepare-field-pair",
        shiny::selectInput(
          ns("measurement_variable"),
          "Target variable",
          character(),
          width = "100%"
        ),
        shiny::textInput(
          ns("measurement_output"),
          "Write to",
          value = "",
          width = "100%"
        )
      ),
      shiny::uiOutput(ns("measurement_output_help"))
    ),
    shiny::conditionalPanel(
      "input.value_type === 'variability'",
      ns = ns,
      shiny::selectInput(
        ns("variability_source"),
        "Source for variability check",
        character(),
        width = "100%"
      ),
      shiny::tags$div(
        class = "llw-prepare-field-pair",
        shiny::numericInput(
          ns("variability_length"),
          "Window length",
          10,
          min = 0,
          width = "100%"
        ),
        shiny::selectInput(
          ns("variability_unit"),
          "Unit",
          c("Seconds" = "seconds", "Minutes" = "minutes", "Hours" = "hours"),
          selected = "minutes",
          width = "100%"
        )
      ),
      shiny::tags$div(
        class = "llw-prepare-field-pair",
        shiny::selectInput(
          ns("variability_alignment"),
          "Window position",
          c(
            "Centred on each reading" = "centered",
            "Ending at each reading" = "trailing"
          ),
          width = "100%"
        ),
        shiny::selectInput(
          ns("variability_statistic"),
          "Variability measure",
          c(
            "Standard deviation (SD)" = "sd",
            "Coefficient of variation (%)" = "cv"
          ),
          width = "100%"
        )
      ),
      shiny::tags$div(
        class = "llw-prepare-field-pair",
        shiny::selectInput(
          ns("variability_direction"),
          "Set target to NA when",
          c(
            "Variability is below threshold" = "below",
            "Variability is above threshold" = "above"
          ),
          width = "100%"
        ),
        shiny::numericInput(
          ns("variability_threshold"),
          "Threshold",
          1,
          min = 0,
          width = "100%"
        )
      ),
      shiny::tags$div(
        class = "llw-prepare-field-pair",
        shiny::numericInput(
          ns("variability_coverage"),
          "Minimum window coverage (%)",
          80,
          min = 0,
          max = 100,
          width = "100%"
        ),
        shiny::selectInput(
          ns("variability_unevaluable"),
          "Unevaluable windows",
          c("Keep target unchanged" = "keep", "Set target to NA" = "missing"),
          selected = "keep",
          width = "100%"
        )
      ),
      shiny::tags$p(
        class = "llw-prepare-hint",
        "Unevaluable windows are reported in the preview."
      )
    ),
    shiny::conditionalPanel(
      "input.value_type === 'invalidate'",
      ns = ns,
      shiny::tags$div(
        class = "llw-prepare-field-pair",
        shiny::selectInput(
          ns("invalidation_source"),
          "Source for condition",
          character(),
          width = "100%"
        ),
        shiny::selectInput(
          ns("invalidation_mode"),
          "Check using",
          c("Numeric interval" = "range", "Categories" = "categories"),
          width = "100%"
        )
      ),
      shiny::conditionalPanel(
        "input.invalidation_mode === 'categories'",
        ns = ns,
        shiny::selectizeInput(
          ns("invalidation_values"),
          "Categories",
          character(),
          multiple = TRUE,
          width = "100%",
          options = list(plugins = list("remove_button"))
        )
      ),
      shiny::conditionalPanel(
        "input.invalidation_mode === 'range'",
        ns = ns,
        shiny::tags$div(
          class = "llw-prepare-field-pair",
          shiny::numericInput(
            ns("measurement_lower"),
            "Lower bound (included)",
            0,
            width = "100%"
          ),
          shiny::numericInput(
            ns("measurement_upper"),
            "Upper bound (included)",
            100000,
            width = "100%"
          )
        )
      ),
      shiny::tags$div(
        class = "llw-prepare-field-pair",
        shiny::selectInput(
          ns("invalidation_condition"),
          "Set target to NA when",
          c("Source matches" = "inside", "Source does not match" = "outside"),
          selected = "outside",
          width = "100%"
        ),
        shiny::selectInput(
          ns("invalidation_missing"),
          "Missing source values",
          c("Keep target unchanged" = "keep", "Set target to NA" = "missing"),
          width = "100%"
        )
      )
    ),
    shiny::conditionalPanel(
      "input.value_type === 'calibrate'",
      ns = ns,
      shiny::selectInput(
        ns("measurement_operation"),
        "Operation",
        c(
          "Scale and offset" = "calibrate",
          "Logarithm" = "log",
          "Root" = "root",
          "Power" = "power",
          "Exponential" = "exp",
          "Absolute value" = "abs",
          "Zero inflated" = "zero_inflated"
        ),
        width = "100%"
      ),
      shiny::conditionalPanel(
        "input.measurement_operation === 'zero_inflated'",
        ns = ns,
        shiny::selectInput(
          ns("zero_operation"),
          "Transformation",
          c(
            "Log (log_zero_inflated)" = "log_zero_inflated",
            "Inverse (exp_zero_inflated)" = "exp_zero_inflated"
          ),
          width = "100%"
        ),
        shiny::tags$div(
          class = "llw-prepare-field-pair",
          shiny::numericInput(
            ns("zero_base"),
            "Base",
            10,
            min = 0,
            width = "100%"
          ),
          shiny::numericInput(
            ns("zero_offset"),
            "Offset",
            0.1,
            min = 0,
            step = 0.1,
            width = "100%"
          )
        )
      ),
      shiny::conditionalPanel(
        "input.measurement_operation === 'calibrate'",
        ns = ns,
        shiny::tags$div(
          class = "llw-prepare-field-pair",
          shiny::numericInput(
            ns("calibration_multiplier"),
            "Multiplier",
            1,
            width = "100%"
          ),
          shiny::numericInput(
            ns("calibration_offset"),
            "Offset",
            0,
            width = "100%"
          )
        )
      ),
      shiny::conditionalPanel(
        "input.measurement_operation === 'log' || input.measurement_operation === 'exp'",
        ns = ns,
        shiny::selectInput(
          ns("transform_base"),
          "Base",
          c(
            "Natural (e)" = "e",
            "2" = "2",
            "10" = "10",
            "Custom base" = "custom"
          ),
          width = "100%"
        ),
        shiny::conditionalPanel(
          "input.transform_base === 'custom'",
          ns = ns,
          shiny::numericInput(
            ns("transform_base_custom"),
            "Custom base",
            10,
            min = 0,
            width = "100%"
          )
        )
      ),
      shiny::conditionalPanel(
        "input.measurement_operation === 'root'",
        ns = ns,
        shiny::numericInput(
          ns("transform_degree"),
          "Root degree",
          2,
          min = 1,
          step = 1,
          width = "100%"
        )
      ),
      shiny::conditionalPanel(
        "input.measurement_operation === 'power'",
        ns = ns,
        shiny::numericInput(
          ns("transform_power"),
          "Exponent",
          2,
          width = "100%"
        )
      ),
      shiny::textOutput(ns("calibration_formula")),
      shiny::conditionalPanel(
        "input.measurement_operation !== 'calibrate'",
        ns = ns,
        shiny::selectInput(
          ns("transform_invalid"),
          "If a result is undefined or nonfinite",
          c("Stop and explain" = "error", "Mark it missing (NA)" = "missing"),
          width = "100%"
        )
      ),
      preparation_help(
        "Reference and operation details",
        shiny::textInput(
          ns("calibration_reference"),
          "Reference or reason",
          width = "100%"
        ),
        shiny::textOutput(ns("transformation_help"))
      )
    ),
    preparation_help(
      "How this step works",
      shiny::conditionalPanel(
        "input.value_type === 'invalidate'",
        ns = ns,
        shiny::tags$p(
          "Matching means inside the inclusive interval or among the selected categories. Rows, timestamps and existing target missing values are retained."
        ),
        shiny::textOutput(ns("invalidation_help"))
      ),
      shiny::conditionalPanel(
        "input.value_type === 'variability'",
        ns = ns,
        shiny::tags$p(
          "Sample SD uses the finite source readings within the window. CV is 100 \u00d7 SD / |mean|; a zero mean is unevaluable. Equality with the threshold keeps the target value."
        ),
        shiny::tags$p(
          "Windows use elapsed time within each current group and participant/file recording. Coverage is the observed time in the full window, using the original recording's sampling interval. Missing values and gaps are not filled; windows at recording edges may have lower coverage."
        ),
        shiny::tags$p(
          "At least two finite source readings and the selected coverage are required. Duplicate timestamps or an unknown sampling interval make a window unevaluable. The check source stays fixed when the target changes; if unavailable, its recorded source is used and the change is shown."
        )
      ),
      shiny::conditionalPanel(
        "input.value_type === 'calibrate'",
        ns = ns,
        shiny::tags$p(
          "Uses values after earlier steps. Rows, timestamps and existing missing values are retained."
        )
      ),
      shiny::textOutput(ns("measurement_help"))
    )
  )
}

preparation_editor_actions_ui <- function(ns) {
  shiny::tagList(
    shiny::uiOutput(ns("operation_docs")),
    shiny::conditionalPanel(
      "input.preparation_category !== 'sampling' || input.sampling_type !== 'timezone'",
      ns = ns,
      shiny::tags$p(
        class = "llw-prepare-native-time",
        shiny::icon("clock"),
        shiny::textOutput(ns("native_timezone"), inline = TRUE)
      )
    ),
    shiny::tags$div(
      class = "llw-prepare-editor-footer",
      shiny::tags$div(
        class = "llw-prepare-editor-feedback",
        shiny::conditionalPanel(
          "output.form_loading === 'yes'",
          ns = ns,
          shiny::tags$p("Loading the step controls...")
        ),
        shiny::textOutput(ns("form_status")),
        shiny::tags$div(
          class = "text-danger",
          role = "alert",
          shiny::textOutput(ns("editor_error"))
        )
      ),
      shiny::tags$div(
        class = "llw-prepare-save-actions",
        shiny::actionButton(
          ns("save_step"),
          "Adjust current step",
          icon = shiny::icon("pen"),
          class = "btn-primary",
          disabled = TRUE
        ),
        shiny::actionButton(
          ns("save_new"),
          "Save as new step",
          icon = shiny::icon("plus"),
          class = "btn-primary"
        )
      ),
      shiny::actionButton(
        ns("new_step"),
        "Clear editor",
        class = "btn-link btn-sm"
      )
    )
  )
}

prepare_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$section(
    class = "llw-prepare",
    `aria-label` = "Prepare dataset",
    shiny::tags$header(
      class = "llw-prepare-intro",
      shiny::tags$h2("Prepare your data"),
      shiny::tags$p(
        "Edit one recipe. Every preview and Apply starts from the original source."
      )
    ),
    shiny::uiOutput(ns("heading")),
    shiny::tags$div(
      class = "llw-prepare-layout",
      shiny::tags$div(
        class = "llw-prepare-workflow",
        bslib::card(
          class = "llw-prepare-card llw-prepare-editor-card",
          bslib::card_header(preparation_step_header(
            1,
            shiny::textOutput(ns("editor_title"), inline = TRUE),
            NULL
          )),
          bslib::card_body(
            bslib::navset_pill(
              id = ns("preparation_category"),
              bslib::nav_panel(
                "Filtering",
                value = "filter",
                preparation_choice_buttons(
                  ns("step_type"),
                  "Filter observations by",
                  c(
                    "Participants",
                    "Date & time",
                    "Duration",
                    "Day / night",
                    "Variable",
                    "Coverage"
                  ),
                  c(
                    "participants",
                    "time",
                    "duration",
                    "photoperiod",
                    "variable",
                    "coverage"
                  ),
                  c(
                    "users",
                    "calendar",
                    "hourglass-half",
                    "sun",
                    "filter",
                    "chart-bar"
                  ),
                  "participants"
                ),
                shiny::conditionalPanel(
                  "input.step_type === 'time'",
                  ns = ns,
                  shiny::selectInput(
                    ns("time_mode"),
                    "Date and time selection",
                    c(
                      "Calendar dates" = "dates",
                      "Date and time endpoints" = "datetimes",
                      "Daily clock-time window" = "clock"
                    ),
                    width = "100%"
                  )
                ),
                preparation_existing_filters_ui(ns),
                preparation_new_filters_ui(ns)
              ),
              bslib::nav_panel(
                "Measurement values",
                value = "values",
                preparation_measurements_ui(ns)
              ),
              bslib::nav_panel(
                "Time & sampling",
                value = "sampling",
                preparation_time_ui(ns)
              )
            ),
            preparation_editor_actions_ui(ns)
          )
        )
      ),
      shiny::tags$aside(
        class = "llw-prepare-preview",
        `aria-label` = "Impact preview",
        bslib::card(
          class = "llw-prepare-card llw-prepare-impact-card",
          bslib::card_header(preparation_step_header(
            2,
            "Impact preview",
            shiny::textOutput(ns("plot_axis_label"), inline = TRUE)
          )),
          bslib::card_body(
            shiny::uiOutput(ns("action_status")),
            shiny::conditionalPanel(
              "output.large_work === 'yes'",
              ns = ns,
              shiny::uiOutput(ns("resource_notice")),
              shiny::checkboxInput(
                ns("ack_cost"),
                "Allow this large preview within the session memory budget.",
                FALSE
              )
            ),
            shiny::conditionalPanel(
              "output.work_active === 'yes'",
              ns = ns,
              shiny::actionButton(ns("cancel"), "Cancel preview")
            ),
            shiny::uiOutput(ns("preview_notice")),
            shiny::conditionalPanel(
              "output.preview_ready === 'yes'",
              ns = ns,
              shiny::uiOutput(ns("impact_summary")),
              bslib::navset_pill(
                bslib::nav_panel(
                  "Plot",
                  shiny::tags$div(
                    class = "llw-prepare-plot-controls",
                    shiny::selectInput(
                      ns("plot_step"),
                      "Comparison",
                      c("Whole recipe" = "all"),
                      width = "100%"
                    ),
                    shiny::selectizeInput(
                      ns("plot_id"),
                      "Participant",
                      choices = NULL,
                      width = "100%"
                    ),
                    shiny::selectizeInput(
                      ns("plot_day"),
                      "Date",
                      choices = NULL,
                      width = "100%"
                    )
                  ),
                  shiny::uiOutput(ns("transformation_note")),
                  shiny::tags$div(
                    class = "llw-prepare-operation llw-prepare-plot-layout",
                    shiny::radioButtons(
                      ns("plot_layout"),
                      "Plot view",
                      c("Overlay" = "overlay", "Split view" = "split"),
                      selected = "overlay",
                      inline = TRUE
                    )
                  ),
                  shiny::conditionalPanel(
                    "input.plot_layout !== 'split'",
                    ns = ns,
                    shiny::plotOutput(ns("plot"), height = "390px")
                  ),
                  shiny::conditionalPanel(
                    "input.plot_layout === 'split'",
                    ns = ns,
                    shiny::plotOutput(ns("plot_source"), height = "310px"),
                    shiny::plotOutput(ns("plot_preview"), height = "310px")
                  ),
                  shiny::tags$div(
                    class = "llw-prepare-day-actions",
                    shiny::checkboxInput(
                      ns("automatic_day"),
                      "Automatically choose a day",
                      value = TRUE
                    ),
                    shiny::actionButton(
                      ns("auto_day"),
                      "Choose an informative day",
                      icon = shiny::icon("wand-magic-sparkles"),
                      class = "btn-outline-secondary btn-sm"
                    )
                  )
                ),
                bslib::nav_panel(
                  "Variable history",
                  shiny::uiOutput(ns("variable_history"))
                ),
                bslib::nav_panel(
                  "Measures",
                  shiny::tags$div(
                    class = "table-responsive",
                    shiny::tableOutput(ns("comparison"))
                  ),
                  shiny::uiOutput(ns("aggregation_coverage"))
                ),
                bslib::nav_panel(
                  "Source sample",
                  shiny::tags$div(
                    class = "table-responsive",
                    shiny::tableOutput(ns("source_sample"))
                  )
                ),
                bslib::nav_panel(
                  "Preview sample",
                  shiny::tags$div(
                    class = "table-responsive",
                    shiny::tableOutput(ns("preview_sample"))
                  )
                )
              ),
              preparation_help(
                "Preview details",
                shiny::tags$div(
                  class = "llw-prepare-hint",
                  shiny::textOutput(ns("plot_caption"))
                ),
                shiny::uiOutput(ns("participant_impact")),
                shiny::tags$p(
                  "Filtering removes rows. Measurement-value steps keep rows and may set readings to NA. A broken plot line is a display cue, not an inserted NA row. Only a saved Gaps step adds explicit missing rows."
                ),
                shiny::tags$p(
                  "Amber marks compare removed source timestamps within the same recording and group. Existing gaps in the source and shifted timestamps are not marked as removals. Blue marks show NA readings in the preview, whether they were already missing or became missing in the recipe."
                ),
                shiny::tags$p(
                  "Recipe counts use every observation. Step loss is shown relative to the rows entering that step and to the original source. The plot shows a single source day; its sampling, if needed, is labelled. Tables show the first 100 rows and at most 8 columns."
                ),
                shiny::uiOutput(ns("preview_details"))
              )
            ),
            shiny::conditionalPanel(
              "output.preview_ready !== 'yes'",
              ns = ns,
              shiny::actionButton(
                ns("preview"),
                "Refresh preview",
                icon = shiny::icon("rotate-right"),
                class = "btn-outline-secondary"
              )
            )
          )
        )
      ),
      shiny::tags$div(
        class = "llw-prepare-recipe-area",
        preparation_recipe_ui(ns)
      )
    )
  )
}

preparation_existing_filters_ui <- function(ns) {
  shiny::tags$div(
    class = "llw-prepare-fields",
    shiny::conditionalPanel(
      "input.step_type === 'participants'",
      ns = ns,
      shiny::selectizeInput(
        ns("participants"),
        "Participants and sources to keep",
        choices = NULL,
        multiple = TRUE,
        width = "100%",
        options = list(placeholder = "Choose one or more identities")
      ),
      shiny::actionButton(
        ns("all_streams"),
        "Select all identities",
        class = "btn-sm"
      ),
      preparation_help(
        "How participants and sources are identified",
        shiny::uiOutput(ns("selection_scope"))
      )
    ),
    shiny::conditionalPanel(
      "input.step_type === 'dates' || (input.step_type === 'time' && input.time_mode === 'dates')",
      ns = ns,
      shiny::tags$div(
        class = "llw-prepare-field-pair",
        shiny::uiOutput(ns("date_start_control")),
        shiny::uiOutput(ns("date_end_control"))
      ),
      shiny::tags$p(
        class = "llw-prepare-hint",
        "Both dates are included. Available dates follow the recording span."
      )
    ),
    shiny::conditionalPanel(
      "input.step_type === 'datetimes' || (input.step_type === 'time' && input.time_mode === 'datetimes')",
      ns = ns,
      preparation_endpoint_ui(ns, "start", "Start (included)"),
      preparation_endpoint_ui(ns, "end", "End (excluded)"),
      preparation_help(
        "Time formats and daylight-saving changes",
        shiny::tags$p(
          "Enter HH:MM or HH:MM:SS. During a repeated daylight-saving hour, add the UTC offset, for example 02:30:00 +0100. These controls select observations and keep their timestamps unchanged."
        )
      )
    ),
    shiny::conditionalPanel(
      "input.step_type === 'clock' || (input.step_type === 'time' && input.time_mode === 'clock')",
      ns = ns,
      shiny::tags$div(
        class = "llw-prepare-field-pair",
        shiny::textInput(
          ns("clock_start"),
          "Start time (included)",
          "22:00:00",
          updateOn = "blur",
          width = "100%"
        ),
        shiny::textInput(
          ns("clock_end"),
          "End time (excluded)",
          "06:00:00",
          updateOn = "blur",
          width = "100%"
        )
      ),
      shiny::tags$p(
        class = "llw-prepare-hint",
        "Keep this clock-time window each day. An earlier end crosses midnight."
      ),
      preparation_help(
        "Clock-time rules",
        shiny::tags$p(
          "Use HH:MM:SS in the dataset's native time zone, for example 22:00:00 to 06:00:00. Equal bounds are not allowed. This filters observations; analysis grouping is a separate step. The preview labels overnight intervals by their starting date."
        )
      )
    ),
    shiny::conditionalPanel(
      "input.step_type === 'duration'",
      ns = ns,
      shiny::tags$div(
        class = "llw-prepare-field-pair",
        shiny::numericInput(
          ns("duration_length"),
          "Length",
          value = 1,
          min = 0.001,
          step = 1,
          updateOn = "blur",
          width = "100%"
        ),
        shiny::selectInput(
          ns("duration_unit"),
          "Unit",
          width = "100%",
          selected = "elapsed_days",
          choices = c(
            "Hours (elapsed time)" = "hours",
            "Minutes (elapsed time)" = "minutes",
            "Seconds (elapsed time)" = "seconds",
            "Days of 24 hours (elapsed time)" = "elapsed_days",
            "Calendar days (local clock time)" = "calendar_days"
          )
        )
      ),
      shiny::radioButtons(
        ns("duration_from"),
        "Count length from",
        choices = c("Start" = "start", "End" = "end"),
        inline = TRUE
      ),
      shiny::selectInput(
        ns("duration_scope"),
        "Apply the length",
        choices = c("Overall" = "overall"),
        width = "100%"
      ),
      shiny::tags$div(
        class = "llw-prepare-hint",
        shiny::textOutput(ns("duration_scope_help"))
      ),
      preparation_help(
        "How duration is counted",
        shiny::tags$p(
          "For example, 2 days from Start with Per participant and file keeps the first 2 days of each participant in each source file, even when those files begin on different dates. Per current group uses the dataset's existing groups instead."
        ),
        shiny::tags$p(
          "The anchor is the first or last observation reaching this step. Gaps count toward the length. Calendar days keep the local clock time; elapsed units measure time passed."
        ),
        shiny::tags$p(
          "From start includes the first observation and excludes the upper boundary. From end includes the last observation and the lower boundary."
        )
      )
    )
  )
}

preparation_recipe_choices_ui <- function(
  ns,
  labels,
  ids,
  selected,
  disabled = FALSE
) {
  if (!length(ids)) return(NULL)
  choices <- shiny::radioButtons(
    ns("selected_step"),
    "Select a step to edit",
    choiceNames = labels,
    choiceValues = ids,
    selected = selected,
    width = "100%"
  )
  query <- htmltools::tagQuery(choices)
  query$addAttrs(`aria-describedby` = ns("recipe_order_help"))
  query$find(".shiny-options-group")$addAttrs(id = ns("recipe_order"))
  query$find(".radio")$each(function(tag, i) {
    tag$attribs[["data-rank-id"]] <- ids[[i]]
  })
  query$find(".radio > label")$prepend(shiny::tags$span(
    class = "llw-prepare-drag-handle",
    title = "Drag to reorder",
    `aria-hidden` = "true",
    shiny::icon("grip-vertical")
  ))
  shiny::tagList(
    shiny::tags$p(
      id = ns("recipe_order_help"),
      class = "llw-prepare-hint",
      "Drag the grip to reorder, or use Earlier / Later."
    ),
    query$allTags(),
    sortable::sortable_js(
      css_id = ns("recipe_order"),
      elementId = ns("recipe_sortable"),
      options = sortable::sortable_options(
        handle = ".llw-prepare-drag-handle",
        draggable = ".radio",
        disabled = disabled,
        onLoad = sortable::sortable_js_capture_input(ns("step_order")),
        onSort = sortable::sortable_js_capture_input(ns("step_order"))
      )
    )
  )
}

preparation_recipe_ui <- function(ns) {
  bslib::card(
    class = "llw-prepare-card llw-prepare-recipe-card",
    bslib::card_header(preparation_step_header(
      3,
      "Your recipe",
      "One ordered recipe, always evaluated from the source."
    )),
    bslib::card_body(
      shiny::tags$div(
        class = "llw-prepare-recipe-layout",
        shiny::tags$div(
          class = "llw-prepare-recipe-content",
          shiny::uiOutput(ns("chain_changes")),
          shiny::uiOutput(ns("recipe_list")),
          shiny::conditionalPanel(
            "output.has_steps === 'yes'",
            ns = ns,
            shiny::tags$div(
              class = "llw-prepare-recipe-choices",
              shiny::uiOutput(ns("recipe_choices"))
            )
          ),
          shiny::uiOutput(ns("pending_removals"))
        ),
        shiny::tags$div(
          class = "llw-prepare-recipe-navigation",
          shiny::tags$h4("Recipe controls"),
          shiny::conditionalPanel(
            "output.has_steps === 'yes'",
            ns = ns,
            shiny::tags$div(
              class = "llw-prepare-recipe-actions",
              shiny::actionButton(
                ns("edit"),
                "Edit",
                icon = shiny::icon("pen")
              ),
              shiny::actionButton(
                ns("up"),
                "Earlier",
                icon = shiny::icon("arrow-up")
              ),
              shiny::actionButton(
                ns("down"),
                "Later",
                icon = shiny::icon("arrow-down")
              ),
              shiny::actionButton(ns("toggle"), "Enable / disable"),
              shiny::actionButton(
                ns("remove"),
                "Remove",
                class = "llw-prepare-remove"
              )
            )
          ),
          shiny::actionButton(
            ns("undo_edit"),
            "Undo recipe edit",
            icon = shiny::icon("rotate-left"),
            class = "btn-sm",
            disabled = TRUE
          ),
          shiny::tags$div(
            class = "llw-prepare-commit-actions",
            shiny::actionButton(
              ns("apply"),
              "Apply",
              class = "btn-primary",
              disabled = TRUE
            ),
            shiny::actionButton(
              ns("reset"),
              "Reset to source",
              icon = shiny::icon("arrow-rotate-left")
            )
          ),
          shiny::tags$p(
            class = "llw-prepare-hint",
            "Apply replaces prepared data using this recipe and the original source. Reset restores the source and clears the recipe."
          )
        )
      )
    )
  )
}
