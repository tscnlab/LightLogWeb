dashboard_value_box <- function(
  title,
  value,
  icon_name,
  explanation,
  tone = c("neutral", "success", "warning"),
  outcome = NULL,
  control = NULL
) {
  tone <- match.arg(tone)
  if (!is.null(outcome)) {
    if (
      !is.character(outcome) ||
        length(outcome) != 1L ||
        is.na(outcome) ||
        !nzchar(outcome)
    ) {
      abort_llw(
        "`outcome` must be `NULL` or one non-empty string.",
        type = "validation"
      )
    }
  }
  box <- tags$div(
    class = paste(
      "llw-dashboard-value-box",
      paste0("llw-dashboard-value-box--", tone)
    ),
    tooltip2(
      tags$div(
        class = "llw-dashboard-value-box__icon",
        role = "button",
        tabindex = "0",
        `aria-label` = paste("About", title),
        icon(icon_name)
      ),
      explanation,
      placement = "bottom"
    ),
    tags$div(
      class = "llw-dashboard-value-box__body",
      tags$div(
        class = "llw-dashboard-value-box__title-row",
        tags$div(class = "llw-dashboard-value-box__title", title),
        control
      ),
      tags$div(
        class = "llw-dashboard-value-box__value-row",
        tags$div(class = "llw-dashboard-value-box__value", value),
        if (!is.null(outcome)) {
          tags$span(
            class = "llw-dashboard-value-box__outcome",
            outcome
          )
        }
      )
    )
  )
  box
}

dashboard_quality_tone <- function(value) {
  if (
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      value == 0
  ) {
    "success"
  } else {
    "warning"
  }
}

dashboard_page_select_renderers <- function() {
  page_icon <- paste0(
    '<span class="llw-dashboard-page-option__icon" aria-hidden="true">',
    '<svg viewBox="0 0 16 16" fill="none" focusable="false">',
    '<path d="M3.5 1.5h5.25l3.75 3.75V14.5h-9z"/>',
    '<path d="M8.75 1.5v3.75h3.75"/>',
    '<path d="M5.75 8h4.5M5.75 10.5h4.5"/>',
    "</svg>",
    "</span>"
  )
  renderer <- function(css_class) {
    paste0(
      "function(item, escape) {",
      "var label = item.label == null ? ",
      "(item.text == null ? item.value : item.text) : item.label;",
      "return '<div class=\"llw-dashboard-page-option ",
      css_class,
      "\">",
      page_icon,
      "<span class=\"llw-dashboard-page-option__label\">' + ",
      "escape(label) + '</span></div>';",
      "}"
    )
  }
  I(paste0(
    "{option:",
    renderer("llw-dashboard-page-option--dropdown"),
    ",item:",
    renderer("llw-dashboard-page-option--selected"),
    "}"
  ))
}

dashboard_page_select_input <- function(
  input_id,
  label,
  choices,
  selected,
  width = "100%"
) {
  selectizeInput(
    input_id,
    label = label,
    choices = choices,
    selected = selected,
    width = width,
    options = list(
      dropdownParent = "body",
      render = dashboard_page_select_renderers()
    )
  )
}

datasetDashboardUI <- function(id) {
  ns <- NS(id)
  defaults <- dashboard_view_defaults()
  limits <- dashboard_plot_limits()

  tagList(
    tags$header(
      class = "llw-view-header",
      tags$p(class = "llw-eyebrow", "Selected session dataset"),
      uiOutput(ns("dataset_name")),
      tags$p(
        class = "llw-view-lede",
        paste(
          "Inspect source or pre-processed measurements without changing the",
          "stored dataset. Every preview reduction and scale is labelled."
        )
      )
    ),
    uiOutput(ns("dashboard_error")),
    layout_column_wrap(
      fill = FALSE,
      width = "15rem",
      gap = "0.75rem",
      class = "llw-dashboard-value-grid",
      htmltools::tagAppendAttributes(
        dashboard_value_box(
          "Focus metric",
          selectizeInput(
            ns("focus_variable"),
            label = NULL,
            choices = NULL,
            multiple = FALSE,
            options = list(
              placeholder = "Choose a numeric measurement"
            )
          ),
          "bullseye",
          paste(
            "The selected metric drives missingness, coverage, the plot, and",
            "compact table columns. Its recorded unit remains visible in",
            "the plot and data views."
          )
        ),
        class = paste(
          "llw-dashboard-value-box--focus",
          "llw-dashboard-value-slot",
          "llw-dashboard-value-slot--focus"
        )
      ),
      uiOutput(
        ns("summary_participants"),
        class = paste(
          "llw-dashboard-value-slot",
          "llw-dashboard-value-slot--participants"
        )
      ),
      uiOutput(
        ns("summary_date_span"),
        class = paste(
          "llw-dashboard-value-slot",
          "llw-dashboard-value-slot--date"
        )
      ),
      uiOutput(
        ns("summary_sampling"),
        class = paste(
          "llw-dashboard-value-slot",
          "llw-dashboard-value-slot--sampling"
        )
      ),
      uiOutput(
        ns("summary_missingness"),
        class = paste(
          "llw-dashboard-value-slot",
          "llw-dashboard-value-slot--missingness"
        )
      )
    ),
    htmltools::tagAppendAttributes(
      navset_card_pill(
        id = ns("dashboard_tabs"),
        selected = "explore",
        nav_panel(
          title = "Explore",
          value = "explore",
          tags$div(
            class = "llw-explorer-workbench",
            htmltools::tagAppendAttributes(
              layout_sidebar(
              sidebar = sidebar(
                title = NULL,
                width = 300,
                padding = 0,
                gap = 0,
                class = "llw-explorer-sidebar",
                tags$div(
                  class = "llw-explorer-sidebar__header",
                  tags$div(
                    class = "llw-explorer-sidebar__title",
                    tags$span(
                      class = "llw-explorer-sidebar__eyebrow",
                      icon("sliders"),
                      "Explorer"
                    ),
                    tags$h2(class = "h5 mb-0", "View controls")
                  ),
                  uiOutput(ns("view_recommendation"))
                ),
                tags$section(
                  class = paste(
                    "llw-explorer-control-group",
                    "llw-explorer-control-group--display"
                  ),
                  `aria-label` = "Display controls",
                  htmltools::tagAppendAttributes(
                    radioButtons(
                      ns("view_mode"),
                      "View",
                      choiceNames = list(
                        tags$span(
                          class = "llw-dashboard-view-mode__option",
                          `data-view-mode` = "auto",
                          title = "Choose the view from the focus-metric span",
                          icon("wand-magic-sparkles"),
                          tags$span("Auto")
                        ),
                        tags$span(
                          class = "llw-dashboard-view-mode__option",
                          `data-view-mode` = "detailed",
                          title = "Always show the detailed timeline",
                          icon("chart-line"),
                          tags$span("Timeline")
                        ),
                        tags$span(
                          class = "llw-dashboard-view-mode__option",
                          `data-view-mode` = "availability",
                          `aria-label` = "Coverage overview",
                          title = "Always show the focus-coverage overview",
                          icon("circle-half-stroke"),
                          tags$span("Coverage")
                        )
                      ),
                      choiceValues = c("auto", "detailed", "availability"),
                      selected = defaults$view_mode,
                      inline = TRUE
                    ),
                    class = "llw-dashboard-view-mode"
                  ),
                  tags$div(
                    class = "llw-explorer-display-options",
                    uiOutput(ns("scale_control")),
                    uiOutput(ns("y_range_control"))
                  )
                ),
                tags$section(
                  class = paste(
                    "llw-explorer-control-group",
                    "llw-explorer-control-group--data"
                  ),
                  `aria-label` = "Data controls",
                  selectInput(
                    ns("data_stage"),
                    "Data stage",
                    choices = c(
                      "Pre-processed" = "preprocessed",
                      "Source" = "source"
                    ),
                    selected = "preprocessed",
                    selectize = FALSE
                  )
                ),
                tags$section(
                  class = paste(
                    "llw-explorer-control-group",
                    "llw-explorer-control-group--time"
                  ),
                  `aria-label` = "Time controls",
                  tags$div(
                    class = "llw-explorer-time-reference",
                    bslib::popover(
                      tags$button(
                        type = "button",
                        class = paste(
                          "btn btn-sm btn-link",
                          "llw-explorer-info-trigger"
                        ),
                        title = "Explain time scope and page navigation",
                        `aria-label` = "Explain time scope and page navigation",
                        icon("circle-info")
                      ),
                      title = "Time scope and navigation",
                      placement = "right",
                      tags$div(
                        class = "llw-explorer-time-help",
                        uiOutput(ns("time_basis_context")),
                        tags$p(
                          class = "llw-secondary small mb-0",
                          paste(
                            "Participant pages move up/down.",
                            "Time pages move left/right.",
                            "Automatic view uses each participant's available",
                            "focus-metric duration."
                          )
                        )
                      )
                    ),
                    selectInput(
                      ns("time_basis"),
                      "Time reference",
                      choices = c(
                        "Own recording dates" = "participant",
                        "Shared calendar dates" = "calendar",
                        "Days from first measurement" = "elapsed",
                        "Day of week (calendar weeks)" = "week"
                      ),
                      selected = defaults$time_basis,
                      selectize = FALSE
                    )
                  ),
                  uiOutput(ns("date_window_ui")),
                  tags$div(
                    class = "llw-explorer-time-actions",
                    tags$label(
                      class = paste(
                        "control-label",
                        "llw-explorer-time-actions__label"
                      ),
                      `for` = ns("days_per_page"),
                      "Max. days per time page"
                    ),
                    selectInput(
                      ns("days_per_page"),
                      NULL,
                      choices = c(
                        "1 day" = "1",
                        "3 days" = "3",
                        "7 days" = "7",
                        "14 days" = "14",
                        "30 days" = "30",
                        "90 days" = "90",
                        "1 year" = "365"
                      ),
                      selected = as.character(defaults$days_per_page),
                      selectize = FALSE
                    ),
                    actionButton(
                      ns("fit_time_scope"),
                      tags$span(
                        class = "llw-explorer-fit-time__label",
                        "Fit window"
                      ),
                      icon = icon("expand"),
                      title = "Fit the time window to the selected participants",
                      `aria-label` = paste(
                        "Fit the time window to the selected participants"
                      ),
                      class = paste(
                        "btn-outline-secondary",
                        "llw-explorer-fit-time"
                      )
                    )
                  )
                ),
                tags$section(
                  class = paste(
                    "llw-explorer-control-group",
                    "llw-explorer-control-group--participants"
                  ),
                  `aria-label` = "Participant controls",
                  tags$div(
                    class = "llw-dashboard-participant-control",
                    conditionalPanel(
                      condition = "!input.show_all_participants",
                      ns = ns,
                      selectizeInput(
                        ns("participants"),
                        "Participant IDs",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = "Choose participant IDs",
                          closeAfterSelect = TRUE
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.show_all_participants",
                      ns = ns,
                      uiOutput(ns("show_all_warning"))
                    )
                  ),
                  tags$div(
                    class = "llw-explorer-participant-options",
                    htmltools::tagAppendAttributes(
                      checkboxInput(
                        ns("show_all_participants"),
                        tagList(
                          tags$span(
                            class = "visually-hidden",
                            "Plot all participants"
                          ),
                          tags$span(
                            class = "llw-explorer-show-all__label",
                            "Plot all"
                          )
                        ),
                        value = FALSE
                      ),
                      class = paste(
                        "llw-dashboard-checkbox",
                        "llw-explorer-show-all"
                      )
                    ),
                    tags$div(
                      class = "llw-dashboard-participants-per-page",
                      tags$label(
                        class = "control-label",
                        `for` = ns("participants_per_page"),
                        "Max. participants per page"
                      ),
                      tags$div(
                        class = paste(
                          "llw-dashboard-participants-per-page-control"
                        ),
                        selectInput(
                          ns("participants_per_page"),
                          label = NULL,
                          choices = c(
                            "1 participant" = "1",
                            "2 participants" = "2",
                            "4 participants" = "4",
                            "6 participants" = "6",
                            "8 participants" = "8",
                            "12 participants" = "12"
                          ),
                          selected = as.character(limits$facets_per_page),
                          selectize = FALSE
                        )
                      )
                    )
                  )
                ),
                uiOutput(
                  ns("participant_pagination"),
                  class = "llw-explorer-participant-pagination"
                )
              ),
              tags$section(
                class = "llw-explorer-canvas",
                `aria-labelledby` = ns("explorer_heading"),
                tags$header(
                  class = "llw-explorer-canvas__header",
                  tags$div(
                    class = "llw-dashboard-heading-group",
                    tags$h2(
                      id = ns("explorer_heading"),
                      class = "h5 mb-1",
                      "Focus metric over time"
                    ),
                    uiOutput(ns("explore_context"))
                  ),
                  tags$div(
                    class = "llw-explorer-canvas__status",
                    uiOutput(ns("log_omission_badge"))
                  )
                ),
                tags$div(
                  class = "llw-explorer-canvas__body",
                  tags$div(
                    class = "llw-plot-stage",
                    tags$nav(
                      class = paste(
                        "llw-participant-rail",
                        "llw-participant-rail--vertical"
                      ),
                      `aria-label` = "Participant pages",
                      uiOutput(
                        ns("participant_previous_control"),
                        class = paste(
                          "llw-participant-rail__slot",
                          "llw-participant-rail--previous"
                        )
                      ),
                      uiOutput(
                        ns("participant_next_control"),
                        class = paste(
                          "llw-participant-rail__slot",
                          "llw-participant-rail--next"
                        )
                      )
                    ),
                    tags$div(
                      class = "llw-dashboard-plot",
                      role = "region",
                      `aria-label` = paste(
                        "Dataset coverage and measurement preview"
                      ),
                      tabindex = "0",
                      plotOutput(ns("dashboard_plot"), height = "auto")
                    )
                  )
                ),
                tags$footer(
                  class = "llw-explorer-pager-strip",
                  tags$nav(
                    class = "llw-explorer-pager",
                    `aria-label` = "Time and page navigation",
                    uiOutput(ns("facet_pagination"))
                  )
                )
              ),
                border = FALSE,
                border_radius = FALSE
              ),
              class = paste(
                "llw-dashboard-layout",
                "llw-explorer-workbench__layout"
              )
            )
          )
        ),
        nav_panel(
        title = "Quality & coverage",
        value = "quality",
        layout_column_wrap(
          fill = FALSE,
          width = "15rem",
          gap = "0.75rem",
          class = "llw-dashboard-value-grid",
          uiOutput(ns("quality_explicit_missing")),
          uiOutput(ns("quality_implicit_gaps")),
          uiOutput(ns("quality_irregular")),
          uiOutput(ns("quality_dst"))
        ),
        uiOutput(ns("quality_note")),
        card(
          class = paste(
            "llw-dashboard-inventory-card",
            "llw-dashboard-auto-height-card"
          ),
          fill = FALSE,
          full_screen = TRUE,
          card_header(
            tags$div(
              class = "llw-dashboard-heading-group",
              tags$h2(
                class = "h5 mb-1",
                "Pre-processed variable inventory"
              ),
              tags$p(
                class = "llw-secondary small mb-0",
                paste(
                  "Missing values and exact measured zeros stay separate.",
                  "Units remain explicit when they were not supplied."
                )
              )
            )
          ),
          tags$div(
            class = paste(
              "llw-dashboard-flat-table-region",
              "llw-dashboard-variable-inventory",
              "llw-dashboard-auto-height-table"
            ),
            role = "region",
            `aria-label` = "Pre-processed variable inventory and missingness",
            tabindex = "0",
            DT::dataTableOutput(ns("variable_inventory"), fill = FALSE)
          )
        ),
        card(
          class = paste(
            "llw-dashboard-coverage-card",
            "llw-dashboard-auto-height-card"
          ),
          fill = FALSE,
          full_screen = TRUE,
          card_header(
            tags$div(
              class = "llw-dashboard-heading-group",
              tags$h2(class = "h5 mb-1", "Participant-day coverage"),
              tags$p(
                class = "llw-secondary small mb-0",
                paste(
                  "Coverage uses non-missing focus values, consecutive",
                  "dominant-epoch intervals, and full participant-local day",
                  "boundaries. Exact zero counts as observed; timing jitter",
                  "remains a separate diagnostic."
                )
              )
            )
          ),
          tags$div(
            class = paste(
              "llw-dashboard-flat-table-region",
              "llw-dashboard-coverage-table",
              "llw-dashboard-auto-height-table"
            ),
            role = "region",
            `aria-label` = "Participant-day coverage table",
            tabindex = "0",
            DT::dataTableOutput(ns("coverage_table"), fill = FALSE)
          )
        )
      ),
      nav_panel(
        title = "Pre-processed data",
        value = "prepared",
        tags$section(
          class = paste(
            "llw-dashboard-data-view",
            "llw-dashboard-data-view--preprocessed"
          ),
          `aria-labelledby` = ns("prepared_heading"),
          tags$div(
            class = "llw-dashboard-data-view__header",
            tags$div(
              tags$p(class = "llw-eyebrow", "Active analysis table"),
              tags$h2(
                id = ns("prepared_heading"),
                class = "h4 mb-1",
                "Pre-processed data"
              ),
              tags$p(
                class = "llw-secondary mb-0",
                paste(
                  "This is the active result used by later analyses. Search,",
                  "sort, paginate, and choose a compact or full column view",
                  "without changing the data."
                )
              )
            ),
            tags$div(
              class = "llw-dashboard-data-view__counts",
              tags$span("Rows"),
              textOutput(ns("prepared_rows"), inline = TRUE),
              tags$span("Revision"),
              textOutput(ns("revision"), inline = TRUE),
              tags$span("Time zone"),
              textOutput(ns("prepared_timezone"), inline = TRUE)
            )
          ),
          tags$div(
            class = "llw-dashboard-state-note",
            uiOutput(ns("prepared_state_note"))
          ),
          tags$div(
            class = paste(
              "llw-dashboard-table-mode",
              "llw-dashboard-table-toolbar"
            ),
            input_switch(
              ns("preprocessed_main_only"),
              "Main columns only",
              value = TRUE
            ),
            tags$span(
              class = "llw-secondary small",
              "Grouping / Id \u00b7 Datetime \u00b7 focus metric"
            ),
            uiOutput(ns("preprocessed_column_selector"))
          ),
          tags$div(
            class = paste(
              "llw-data-region llw-dashboard-wide-table",
              "llw-dashboard-auto-height-table"
            ),
            role = "region",
            `aria-label` = paste(
              "Pre-processed dataset table with server-side search, sorting,",
              "column visibility, and pagination"
            ),
            tabindex = "0",
            DT::dataTableOutput(ns("prepared_table"), fill = FALSE)
          ),
          layout_columns(
            col_widths = c(5, 7),
            class = "llw-dashboard-preprocessed-support",
            card(
              class = "llw-dashboard-preprocessed-state",
              full_screen = TRUE,
              card_header(
                tags$h2(
                  id = ns("preprocessed_state_heading"),
                  class = "h5 mb-0",
                  "Recipe & grouping state"
                )
              ),
              tags$div(
                class = paste(
                  "llw-dashboard-support-region",
                  "llw-dashboard-summary-table"
                ),
                role = "region",
                `aria-labelledby` = ns("preprocessed_state_heading"),
                tabindex = "0",
                tableOutput(ns("state_table"))
              )
            ),
            card(
              class = "llw-dashboard-preprocessed-integrity",
              full_screen = TRUE,
              card_header(
                tags$div(
                  class = "llw-dashboard-heading-group",
                  tags$h2(
                    id = ns("preprocessed_integrity_heading"),
                    class = "h5 mb-1",
                    "Pre-processed integrity"
                  ),
                  tags$p(
                    class = "llw-secondary small mb-0",
                    paste(
                      "Checks the active result after recipe application.",
                      "These are not source import diagnostics."
                    )
                  )
                )
              ),
              tags$div(
                class = paste(
                  "llw-dashboard-support-region",
                  "llw-dashboard-integrity-region"
                ),
                role = "region",
                `aria-labelledby` = ns("preprocessed_integrity_heading"),
                tabindex = "0",
                uiOutput(ns("preprocessed_integrity"))
              )
            )
          )
        )
      ),
      nav_panel(
        title = "Source data",
        value = "raw",
        tags$section(
          class = "llw-dashboard-data-view llw-dashboard-data-view--source",
          `aria-labelledby` = ns("raw_heading"),
          tags$div(
            class = "llw-dashboard-data-view__header",
            tags$div(
              tags$p(class = "llw-eyebrow", "Immutable imported source"),
              tags$h2(
                id = ns("raw_heading"),
                class = "h4 mb-1",
                "Source data"
              ),
              tags$p(
                class = "llw-secondary mb-0",
                paste(
                  "This view preserves imported values and types. It is",
                  "visually separated from the active pre-processed result."
                )
              )
            ),
            tags$div(
              class = "llw-dashboard-data-view__counts",
              tags$span("Rows"),
              textOutput(ns("raw_rows"), inline = TRUE),
              tags$span("Stable ID"),
              textOutput(ns("dataset_id"), inline = TRUE),
              tags$span("Time zone"),
              textOutput(ns("source_timezone"), inline = TRUE)
            )
          ),
          tags$div(
            class = "llw-dashboard-state-note",
            uiOutput(ns("raw_state_note"))
          ),
          tags$div(
            class = paste(
              "llw-dashboard-table-mode",
              "llw-dashboard-table-toolbar"
            ),
            input_switch(
              ns("source_main_only"),
              "Main columns only",
              value = TRUE
            ),
            tags$span(
              class = "llw-secondary small",
              "Grouping / Id \u00b7 Datetime \u00b7 focus metric"
            ),
            uiOutput(ns("source_column_selector"))
          ),
          tags$div(
            class = paste(
              "llw-data-region llw-dashboard-wide-table",
              "llw-dashboard-auto-height-table"
            ),
            role = "region",
            `aria-label` = paste(
              "Source dataset table with server-side search, sorting,",
              "column visibility, and pagination"
            ),
            tabindex = "0",
            DT::dataTableOutput(ns("raw_table"), fill = FALSE)
          ),
          tags$div(
            class = "llw-dashboard-source-support",
            card(
              class = "llw-dashboard-source-provenance",
              full_screen = TRUE,
              card_header(
                tags$h2(
                  id = ns("source_provenance_heading"),
                  class = "h5 mb-0",
                  "Source provenance"
                )
              ),
              tags$div(
                class = paste(
                  "llw-dashboard-support-region",
                  "llw-dashboard-summary-table"
                ),
                role = "region",
                `aria-labelledby` = ns("source_provenance_heading"),
                tabindex = "0",
                tableOutput(ns("provenance_table"))
              )
            ),
            accordion(
              id = ns("source_import_checks"),
              open = FALSE,
              class = paste(
                "llw-dashboard-import-checks",
                "llw-dashboard-source-import-checks"
              ),
              accordion_panel(
                title = "Source import checks",
                tags$p(
                  class = "llw-secondary small",
                  paste(
                    "Gap counts use complete dominant-epoch intervals between",
                    "consecutive timestamps; phase shifts remain separate",
                    "timing diagnostics. Nothing is imputed or turned into",
                    "darkness."
                  )
                ),
                uiOutput(ns("merged_import_note")),
                tags$div(
                  class = paste(
                    "llw-dashboard-support-region",
                    "llw-dashboard-diagnostics"
                  ),
                  role = "region",
                  `aria-label` = "Source import-quality diagnostics",
                  tabindex = "0",
                  uiOutput(ns("quality_diagnostics"))
                )
              )
            )
          )
        )
      )
      ),
      class = "llw-dashboard-tabs"
    )
  )
}

datasetDashboardServer <- function(
  id,
  dataset,
  active_panel,
  color_mode = reactive("light")
) {
  reactive_inputs <- list(
    dataset = dataset,
    active_panel = active_panel,
    color_mode = color_mode
  )
  invalid <- names(reactive_inputs)[
    !vapply(
      reactive_inputs,
      shiny::is.reactive,
      logical(1)
    )
  ]
  if (length(invalid) > 0L) {
    abort_llw(
      paste0(
        "Dashboard input(s) must be reactive: ",
        paste(invalid, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }

  moduleServer(id, function(input, output, session) {
    limits <- dashboard_plot_limits()
    table_contract <- dashboard_table_contract()
    event_value <- reactiveVal(NULL)
    empty_modal_visible <- reactiveVal(FALSE)
    focus_cache <- new.env(parent = emptyenv())
    scope_revision <- reactiveVal(0L)
    plot_scale_preference <- reactiveVal(NULL)
    symlog_threshold_preference <- reactiveVal(NULL)
    page_state <- reactiveVal(c(participant = 1L, time = 1L))
    previous_measurement_window <- reactiveVal(NULL)
    previous_date_window <- reactiveVal(NULL)

    observe({
      is_dashboard <- identical(active_panel(), "dashboard")
      missing_dataset <- is.null(dataset())
      if (is_dashboard && missing_dataset && !empty_modal_visible()) {
        empty_modal_visible(TRUE)
        showModal(modalDialog(
          title = "No dataset selected",
          easyClose = TRUE,
          footer = NULL,
          p("Import a dataset or load the test data to continue."),
          actionButton(
            session$ns("to_import"),
            "Go to import",
            class = "btn-primary",
            icon = icon("file-import"),
            width = "100%"
          )
        ))
      }
      if ((!is_dashboard || !missing_dataset) && empty_modal_visible()) {
        removeModal()
        empty_modal_visible(FALSE)
      }
    })

    observe({
      removeModal()
      empty_modal_visible(FALSE)
      event_value(structure(
        list(id = new_stable_id("dashboard_event"), type = "open_import"),
        class = c("llw_dashboard_event", "list")
      ))
    }) |>
      bindEvent(input$to_import, ignoreInit = TRUE)

    snapshot_result <- reactive({
      record <- dataset()
      if (is.null(record)) return(NULL)
      tryCatch(
        dashboard_dataset_snapshot(record),
        llw_error = identity,
        error = function(cnd) normalize_task_error(cnd, "preparation")
      )
    })

    snapshot <- reactive({
      value <- snapshot_result()
      req(inherits(value, "llw_dashboard_snapshot"))
      value
    })

    observe({
      value <- snapshot()
      current_focus <- isolate(input$focus_variable)
      selected_focus <- if (
        is.character(current_focus) &&
          length(current_focus) == 1L &&
          current_focus %in% value$focus_variables
      ) {
        current_focus
      } else {
        value$primary_variable
      }
      updateSelectizeInput(
        session,
        "focus_variable",
        choices = value$focus_variables,
        selected = selected_focus,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "participants",
        choices = value$participants,
        selected = utils::head(
          value$participants,
          dashboard_view_defaults(limits)$participants_per_page
        ),
        server = TRUE
      )
    })

    snapshot_signature <- reactive({
      value <- snapshot()
      paste(value$record$id, value$record$revision, value$record$raw_checksum)
    })

    observeEvent(
      snapshot_signature(),
      {
        keys <- ls(focus_cache, all.names = TRUE)
        if (length(keys) > 0L) rm(list = keys, envir = focus_cache)
      },
      ignoreInit = FALSE
    )

    selected_focus_variable <- reactive({
      value <- snapshot()
      requested <- input$focus_variable
      if (
        is.character(requested) &&
          length(requested) == 1L &&
          !is.na(requested) &&
          nzchar(requested) &&
          requested %in% value$focus_variables
      ) {
        requested
      } else {
        value$primary_variable
      }
    })

    focus_view_for <- function(data_stage) {
      value <- snapshot()
      focus <- selected_focus_variable()
      key <- paste(snapshot_signature(), data_stage, focus, sep = "::")
      if (!exists(key, envir = focus_cache, inherits = FALSE)) {
        assign(
          key,
          dashboard_focus_view(
            value,
            focus_variable = focus,
            data_stage = data_stage
          ),
          envir = focus_cache
        )
      }
      get(key, envir = focus_cache, inherits = FALSE)
    }

    preprocessed_focus_view <- reactive(focus_view_for("preprocessed"))

    explore_focus_view <- reactive({
      stage <- input$data_stage %||% "preprocessed"
      if (!stage %in% c("preprocessed", "source")) stage <- "preprocessed"
      focus_view_for(stage)
    })

    selected_participants <- reactive({
      dashboard_requested_participants(
        snapshot(),
        participants = input$participants %||% character(),
        show_all = input$show_all_participants %||% FALSE,
        fallback_count = participants_per_page(),
        limits = limits
      )
    })

    view_recommendation <- reactive({
      dashboard_view_recommendation(
        snapshot(),
        focus_view = explore_focus_view(),
        participants = selected_participants(),
        show_all = FALSE,
        limits = limits
      )
    })

    observeEvent(
      snapshot_signature(),
      {
        value <- snapshot()
        focus <- isolate(selected_focus_variable())
        recommended_participants_per_page <- dashboard_view_defaults(
          limits
        )$participants_per_page
        initial_participants <- utils::head(
          value$participants,
          recommended_participants_per_page
        )
        initial_view <- dashboard_focus_view(
          value,
          focus_variable = focus,
          data_stage = "preprocessed"
        )
        recommendation <- dashboard_view_recommendation(
          value,
          focus_view = initial_view,
          participants = initial_participants,
          limits = limits
        )
        updateSelectInput(
          session,
          "data_stage",
          selected = recommendation$data_stage
        )
        updateCheckboxInput(
          session,
          "show_all_participants",
          value = FALSE
        )
        updateSelectInput(
          session,
          "participants_per_page",
          selected = as.character(recommendation$participants_per_page)
        )
        updateSelectInput(
          session,
          "time_basis",
          selected = recommendation$time_basis
        )
        updateRadioButtons(
          session,
          "view_mode",
          selected = recommendation$view_mode
        )
        updateSelectInput(
          session,
          "days_per_page",
          selected = as.character(recommendation$days_per_page)
        )
        plot_scale_preference(recommendation$plot_scale)
        symlog_threshold_preference(recommendation$symlog_threshold)
        updateSelectInput(
          session,
          "plot_scale",
          selected = recommendation$plot_scale
        )
        updateSelectInput(
          session,
          "symlog_threshold",
          selected = as.character(recommendation$symlog_threshold)
        )
        updateRadioButtons(
          session,
          "y_axis_scope",
          selected = "shared"
        )
        updateTextInput(session, "y_axis_min", value = "")
        updateTextInput(session, "y_axis_max", value = "")
        page_state(c(participant = 1L, time = 1L))
        scope_revision(scope_revision() + 1L)
      },
      ignoreInit = FALSE
    )

    time_basis <- reactive({
      value <- input$time_basis %||% "participant"
      allowed <- c(
        "calendar",
        "participant",
        "elapsed",
        "week",
        "month",
        "year"
      )
      if (value %in% allowed) value else "participant"
    })

    view_mode <- reactive({
      value <- input$view_mode %||% "auto"
      if (value %in% c("auto", "detailed", "availability")) value else
        "auto"
    })

    days_per_page <- reactive({
      dashboard_days_per_page(input$days_per_page %||% 7L)
    })

    participants_per_page <- reactive({
      dashboard_participants_per_page(
        input$participants_per_page,
        fallback = limits$facets_per_page
      )
    })

    missingness_scope <- reactive({
      value <- input$missingness_scope %||% "full_days"
      if (value %in% c("recorded", "regular", "full_days")) {
        value
      } else {
        "full_days"
      }
    })

    output$date_window_ui <- renderUI({
      value <- snapshot()
      participant_ids <- selected_participants()
      scope_revision()
      if (time_basis() %in% c("participant", "elapsed")) {
        ranges <- value$participant_ranges
        ranges <- ranges[ranges$Id %in% participant_ids, , drop = FALSE]
        maximum <- if (nrow(ranges) == 0L) {
          1L
        } else {
          max(ranges[["Measurement days"]])
        }
        return(sliderInput(
          session$ns("measurement_window"),
          "Measurement duration (days from start)",
          min = 1L,
          max = maximum,
          value = c(1L, maximum),
          step = 1L,
          ticks = FALSE,
          sep = ""
        ))
      }
      default <- dashboard_participant_date_window(value, participant_ids)
      dateRangeInput(
        session$ns("date_window"),
        "Participant-local date window",
        start = default[[1L]],
        end = default[[2L]],
        min = value$date_start,
        max = value$date_end,
        separator = " to ",
        format = "yyyy-mm-dd",
        weekstart = 1
      )
    })

    observeEvent(
      input$fit_time_scope,
      {
        scope_revision(scope_revision() + 1L)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$apply_recommendation,
      {
        recommendation <- view_recommendation()
        updateSelectInput(
          session,
          "participants_per_page",
          selected = as.character(recommendation$participants_per_page)
        )
        updateSelectInput(
          session,
          "time_basis",
          selected = recommendation$time_basis
        )
        updateRadioButtons(
          session,
          "view_mode",
          selected = recommendation$view_mode
        )
        updateSelectInput(
          session,
          "days_per_page",
          selected = as.character(recommendation$days_per_page)
        )
        plot_scale_preference(recommendation$plot_scale)
        symlog_threshold_preference(recommendation$symlog_threshold)
        updateSelectInput(
          session,
          "plot_scale",
          selected = recommendation$plot_scale
        )
        updateSelectInput(
          session,
          "symlog_threshold",
          selected = as.character(recommendation$symlog_threshold)
        )
        updateRadioButtons(
          session,
          "y_axis_scope",
          selected = "shared"
        )
        updateTextInput(session, "y_axis_min", value = "")
        updateTextInput(session, "y_axis_max", value = "")
        page_state(c(participant = 1L, time = 1L))
        scope_revision(scope_revision() + 1L)
        showNotification(
          tags$div(
            tags$div(class = "fw-semibold", recommendation$summary),
            tags$p(
              class = "small mb-0 mt-1",
              paste(recommendation$reasons, collapse = " ")
            )
          ),
          type = "message",
          duration = 9,
          closeButton = TRUE
        )
      },
      ignoreInit = TRUE
    )

    output$time_basis_context <- renderUI({
      value <- snapshot()
      participant_ids <- selected_participants()
      ranges <- value$participant_ranges[
        value$participant_ranges$Id %in% participant_ids,
        ,
        drop = FALSE
      ]
      envelope <- if (nrow(ranges) == 0L) {
        "No participant range is available"
      } else {
        paste0(
          format(min(ranges[["Date start"]]), "%d %b %Y"),
          "\u2013",
          format(max(ranges[["Date end"]]), "%d %b %Y")
        )
      }
      duration <- if (nrow(ranges) == 0L) {
        "unknown"
      } else {
        duration_range <- range(ranges[["Measurement days"]])
        if (duration_range[[1L]] == duration_range[[2L]]) {
          paste(duration_range[[1L]], "day(s) each")
        } else {
          paste0(
            duration_range[[1L]],
            "\u2013",
            duration_range[[2L]],
            " days per participant"
          )
        }
      }
      text <- switch(
        time_basis(),
        participant = paste(
          "Each facet retains its own recorded dates. Selected calendar",
          "envelope:",
          paste0(envelope, ";"),
          "recording duration:",
          paste0(duration, ".")
        ),
        elapsed = paste(
          "Aligns every participant to measurement day 1. Selected recordings",
          "span",
          paste0(envelope, ";"),
          "individual duration:",
          paste0(duration, ".")
        ),
        calendar = paste(
          "Uses one shared absolute date axis. Changing participants resets the",
          "window to their combined calendar envelope:",
          paste0(envelope, ".")
        ),
        week = paste(
          "Uses calendar-aligned pages with weekday labels. Selected calendar",
          "envelope:",
          paste0(envelope, ".")
        ),
        month = paste(
          "Uses month-aligned pages with day-of-month labels. Selected calendar",
          "envelope:",
          paste0(envelope, ".")
        ),
        year = paste(
          "Uses year-aligned pages while retaining recorded dates. Selected",
          "calendar envelope:",
          paste0(envelope, ".")
        )
      )
      tags$p(class = "llw-secondary small mb-0", text)
    })

    output$view_recommendation <- renderUI({
      recommendation <- view_recommendation()
      state <- recommendation_state()
      trigger <- if (state$active) {
        tags$span(
          class = paste(
            "btn btn-sm w-100 llw-recommended-view-action",
            "llw-recommended-view-action--current is-current"
          ),
          role = "status",
          tabindex = "0",
          `aria-disabled` = "true",
          icon("magic"),
          tags$span("Recommended view")
        )
      } else {
        actionButton(
          session$ns("apply_recommendation"),
          "Recommended view",
          icon = icon("magic"),
          class = paste(
            "btn-sm w-100 llw-recommended-view-action",
            "btn-outline-primary llw-recommended-view-action--available"
          )
        )
      }
      detail <- if (state$active) {
        paste(
          "The recommended view is already active.",
          recommendation$summary
        )
      } else {
        paste0(
          "Apply the recommended view. Current differences: ",
          paste(state$differences, collapse = ", "),
          ". Recommendation: ",
          recommendation$summary
        )
      }
      tooltip2(
        trigger,
        detail,
        placement = "right"
      )
    })

    output$participant_previous_control <- renderUI({
      value <- selection()
      if (value$participant_pages <= 1L) return(NULL)
      current <- current_page_state()[["participant"]]
      actionButton(
        session$ns("participant_previous"),
        label = tags$span(class = "visually-hidden", "Previous participants"),
        icon = tagList(icon("users"), icon("arrow-up")),
        title = "Show the previous participant page",
        disabled = current <= 1L,
        class = "llw-participant-rail__button"
      )
    })

    output$participant_next_control <- renderUI({
      value <- selection()
      if (value$participant_pages <= 1L) return(NULL)
      current <- current_page_state()[["participant"]]
      actionButton(
        session$ns("participant_next"),
        label = tags$span(class = "visually-hidden", "Next participants"),
        icon = tagList(icon("users"), icon("arrow-down")),
        title = "Show the next participant page",
        disabled = current >= value$participant_pages,
        class = "llw-participant-rail__button"
      )
    })
    outputOptions(
      output,
      "participant_previous_control",
      suspendWhenHidden = FALSE
    )
    outputOptions(
      output,
      "participant_next_control",
      suspendWhenHidden = FALSE
    )

    base_selection <- reactive({
      value <- snapshot()
      dashboard_plot_selection(
        value,
        participants = input$participants %||% character(),
        show_all = input$show_all_participants %||% FALSE,
        date_window = input$date_window %||%
          dashboard_default_date_window(value),
        facet_page = 1L,
        participants_per_page = participants_per_page(),
        limits = limits,
        time_basis = time_basis(),
        measurement_window = input$measurement_window,
        view_mode = view_mode(),
        time_page = 1L,
        days_per_page = days_per_page(),
        focus_view = explore_focus_view()
      )
    })

    page_navigator_data <- reactive({
      dashboard_page_navigator(
        explore_focus_view(),
        base_selection()
      )
    })

    current_page_state <- reactive({
      available <- base_selection()
      requested <- page_state()
      c(
        participant = dashboard_page_number(
          requested[["participant"]],
          available$participant_pages
        ),
        time = dashboard_page_number(
          requested[["time"]],
          available$time_pages
        )
      )
    })

    set_page_state <- function(participant = NULL, time = NULL) {
      available <- base_selection()
      current <- current_page_state()
      next_state <- c(
        participant = if (is.null(participant)) {
          current[["participant"]]
        } else {
          dashboard_page_number(
            participant,
            available$participant_pages
          )
        },
        time = if (is.null(time)) {
          current[["time"]]
        } else {
          dashboard_page_number(time, available$time_pages)
        }
      )
      if (!identical(as.integer(next_state), as.integer(current))) {
        freezeReactiveValue(input, "participant_page")
        freezeReactiveValue(input, "time_page")
        freezeReactiveValue(input, "page_navigator")
        page_state(next_state)
      }
      invisible(next_state)
    }

    observe({
      current <- current_page_state()
      requested <- page_state()
      if (!identical(as.integer(current), as.integer(requested))) {
        page_state(current)
      }
    })

    output$participant_pagination <- renderUI({
      value <- base_selection()
      if (value$participant_pages <= 1L) {
        return(NULL)
      }
      participant_choices <- as.list(as.character(seq_len(
        value$participant_pages
      )))
      participant_starts <- (
        seq_len(value$participant_pages) - 1L
      ) * value$participants_per_page + 1L
      participant_ends <- pmin(
        length(value$participants),
        participant_starts + value$participants_per_page - 1L
      )
      names(participant_choices) <- paste0(
        "Participants ",
        participant_starts,
        "\u2013",
        participant_ends,
        " of ",
        length(value$participants)
      )
      current_participant <- current_page_state()[["participant"]]
      tags$div(
        class = "llw-explorer-participant-page-control",
        tags$div(
          class = paste(
            "llw-dashboard-page-axis",
            "llw-dashboard-page-axis--participants",
            "llw-explorer-pager__participants"
          ),
          tags$span(
            class = "visually-hidden",
            "Participant page"
          ),
          dashboard_page_select_input(
            session$ns("participant_page"),
            label = tags$span(
              class = "visually-hidden",
              "Participant page"
            ),
            choices = participant_choices,
            selected = as.character(current_participant),
            width = "100%"
          )
        )
      )
    })
    outputOptions(
      output,
      "participant_pagination",
      suspendWhenHidden = FALSE
    )

    output$facet_pagination <- renderUI({
      value <- base_selection()
      if (value$participant_pages <= 1L && value$time_pages <= 1L) {
        return(NULL)
      }
      participant_starts <- (
        seq_len(value$participant_pages) - 1L
      ) * value$participants_per_page + 1L
      participant_ends <- pmin(
        length(value$participants),
        participant_starts + value$participants_per_page - 1L
      )
      time_choices <- as.list(as.character(seq_len(value$time_pages)))
      names(time_choices) <- value$time_chunks$Label
      pages <- current_page_state()
      current_participant <- pages[["participant"]]
      current_time <- pages[["time"]]
      navigator <- page_navigator_data()
      navigator_names <- lapply(seq_len(nrow(navigator)), function(index) {
        participant_page <- navigator$participant_page[[index]]
        time_page <- navigator$time_page[[index]]
        available <- isTRUE(navigator$has_data[[index]])
        current <- identical(participant_page, current_participant) &&
          identical(time_page, current_time)
        label <- paste0(
          "Participants ",
          participant_starts[[participant_page]],
          "\u2013",
          participant_ends[[participant_page]],
          ", ",
          value$time_chunks$Label[[time_page]],
          if (available) {
            paste0(
              ", ",
              format(
                navigator$observed_focus_values[[index]],
                big.mark = ",",
                scientific = FALSE
              ),
              " observed focus value(s)"
            )
          } else {
            ", empty focus-metric page"
          }
        )
        tags$span(
          class = paste(
            "llw-dashboard-page-navigator__cell",
            if (available) {
              "llw-dashboard-page-navigator__cell--available"
            } else {
              "llw-dashboard-page-navigator__cell--empty"
            },
            if (current) {
              "llw-dashboard-page-navigator__cell--current"
            }
          ),
          `data-participant-page` = participant_page,
          `data-time-page` = time_page,
          `aria-current` = if (current) "page" else NULL,
          title = label,
          tags$span(class = "visually-hidden", label)
        )
      })
      navigator_values <- paste(
        navigator$participant_page,
        navigator$time_page,
        sep = ":"
      )
      navigator_control <- htmltools::tagAppendAttributes(
        radioButtons(
          session$ns("page_navigator"),
          label = tags$span(
            class = "visually-hidden",
            "Participant and time page map"
          ),
          choiceNames = navigator_names,
          choiceValues = navigator_values,
          selected = paste(current_participant, current_time, sep = ":"),
          inline = TRUE
        ),
        class = "llw-dashboard-page-navigator",
        `aria-label` = "Participant and time page map",
        style = paste0(
          "--llw-time-page-count:",
          value$time_pages,
          ";--llw-participant-page-count:",
          value$participant_pages,
          ";--llw-navigator-width:",
          max(3, min(18, 1.1 * value$time_pages)),
          "rem;--llw-navigator-row-size:",
          max(0.32, min(0.9, 3.5 / value$participant_pages)),
          "rem;"
        )
      )
      pagination_class <- paste(
        "llw-dashboard-pagination",
        if (
          value$participant_pages > 1L &&
            value$time_pages > 1L
        ) {
          "llw-dashboard-pagination--2d"
        } else {
          "llw-dashboard-pagination--single"
        },
        if (value$participant_pages <= 1L) {
          "llw-dashboard-pagination--time-only"
        } else if (value$time_pages <= 1L) {
          "llw-dashboard-pagination--participants-only"
        }
      )
      tags$div(
        class = paste(pagination_class, "llw-explorer-pager__dock"),
        tags$div(
          class = "llw-explorer-pager__main",
          navigator_control,
          if (value$time_pages > 1L) {
            tags$div(
              class = paste(
                "llw-dashboard-page-axis llw-dashboard-page-axis--time",
                "llw-explorer-pager__time"
              ),
              tags$span(
                class = "visually-hidden",
                "Time page"
              ),
              actionButton(
                session$ns("time_previous"),
                label = tagList(
                  tags$span(
                    class = "llw-dashboard-time-page-button__icons",
                    `aria-hidden` = "true",
                    icon("arrow-left"),
                    icon("calendar")
                  ),
                  tags$span(
                    class = "visually-hidden",
                    "Previous time page"
                  )
                ),
                title = "Show the previous time page",
                disabled = current_time <= 1L,
                class = paste(
                  "llw-dashboard-time-page-button",
                  "llw-dashboard-time-page-button--previous",
                  "btn-secondary"
                )
              ),
              tags$div(
                class = "llw-dashboard-time-page-control",
                dashboard_page_select_input(
                  session$ns("time_page"),
                  label = tags$span(
                    class = "visually-hidden",
                    "Time page"
                  ),
                  choices = time_choices,
                  selected = as.character(current_time),
                  width = "100%"
                )
              ),
              actionButton(
                session$ns("time_next"),
                label = tagList(
                  tags$span(
                    class = "llw-dashboard-time-page-button__icons",
                    `aria-hidden` = "true",
                    icon("calendar"),
                    icon("arrow-right")
                  ),
                  tags$span(
                    class = "visually-hidden",
                    "Next time page"
                  )
                ),
                title = "Show the next time page",
                disabled = current_time >= value$time_pages,
                class = paste(
                  "llw-dashboard-time-page-button",
                  "llw-dashboard-time-page-button--next",
                  "btn-secondary"
                )
              )
            )
          } else {
            NULL
          }
        )
      )
    })
    outputOptions(output, "facet_pagination", suspendWhenHidden = FALSE)

    observeEvent(
      list(
        input$participants,
        input$show_all_participants,
        input$time_basis,
        input$days_per_page
      ),
      {
        set_page_state(time = 1L)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$measurement_window,
      {
        current <- suppressWarnings(as.integer(input$measurement_window))
        if (length(current) < 2L || anyNA(current[1:2])) return()
        current <- current[1:2]
        previous <- previous_measurement_window()
        previous_measurement_window(current)
        if (is.null(previous) || identical(current, previous)) return()
        lower_changed <- !identical(current[[1L]], previous[[1L]])
        upper_changed <- !identical(current[[2L]], previous[[2L]])
        if (lower_changed || !upper_changed) {
          set_page_state(time = 1L)
        } else {
          set_page_state(time = base_selection()$time_pages)
        }
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$date_window,
      {
        if (length(input$date_window) < 2L) return()
        current <- c(
          dashboard_date_value(input$date_window[[1L]], as.Date(NA)),
          dashboard_date_value(input$date_window[[2L]], as.Date(NA))
        )
        if (length(current) < 2L || anyNA(current)) return()
        previous <- previous_date_window()
        previous_date_window(current)
        if (is.null(previous) || identical(current, previous)) return()
        lower_changed <- !identical(current[[1L]], previous[[1L]])
        upper_changed <- !identical(current[[2L]], previous[[2L]])
        if (lower_changed || !upper_changed) {
          set_page_state(time = 1L)
        } else {
          set_page_state(time = base_selection()$time_pages)
        }
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    observeEvent(
      list(
        input$participants,
        input$show_all_participants,
        input$participants_per_page
      ),
      {
        set_page_state(participant = 1L)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$participant_page,
      {
        set_page_state(participant = input$participant_page)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$time_page,
      {
        set_page_state(time = input$time_page)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$page_navigator,
      {
        value <- as.character(input$page_navigator %||% "")
        pages <- strsplit(value, ":", fixed = TRUE)[[1L]]
        if (length(pages) != 2L) return()
        participant_page <- suppressWarnings(as.integer(pages[[1L]]))
        time_page <- suppressWarnings(as.integer(pages[[2L]]))
        if (is.na(participant_page) || is.na(time_page)) return()
        set_page_state(
          participant = participant_page,
          time = time_page
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$participant_previous,
      {
        current <- current_page_state()[["participant"]]
        set_page_state(participant = current - 1L)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$participant_next,
      {
        current <- current_page_state()[["participant"]]
        set_page_state(participant = current + 1L)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$time_previous,
      {
        current <- current_page_state()[["time"]]
        set_page_state(time = current - 1L)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$time_next,
      {
        current <- current_page_state()[["time"]]
        set_page_state(time = current + 1L)
      },
      ignoreInit = TRUE
    )

    selection <- reactive({
      value <- snapshot()
      pages <- current_page_state()
      dashboard_plot_selection(
        value,
        participants = input$participants %||% character(),
        show_all = input$show_all_participants %||% FALSE,
        date_window = input$date_window %||%
          dashboard_default_date_window(value),
        facet_page = pages[["participant"]],
        participants_per_page = participants_per_page(),
        limits = limits,
        time_basis = time_basis(),
        measurement_window = input$measurement_window,
        view_mode = view_mode(),
        time_page = pages[["time"]],
        days_per_page = days_per_page(),
        focus_view = explore_focus_view()
      )
    })

    preview <- reactive({
      dashboard_plot_preview(
        snapshot(),
        selection(),
        limits = limits,
        focus_view = explore_focus_view()
      )
    })

    resolved_color_mode <- reactive({
      value <- color_mode()
      if (
        is.character(value) &&
          length(value) == 1L &&
          value %in% c("light", "dark")
      ) {
        value
      } else {
        "light"
      }
    })

    output$dashboard_error <- renderUI({
      value <- snapshot_result()
      if (!inherits(value, "error")) return(NULL)
      llw_status_callout(
        "error",
        llw_public_message(value),
        heading = "Dashboard unavailable",
        live = TRUE
      )
    })

    output$dataset_name <- renderUI({
      value <- snapshot()
      tags$h1(value$record$display_name)
    })
    output$dataset_id <- renderText(snapshot()$record$id)
    output$revision <- renderText(paste0("r", snapshot()$record$revision))
    output$prepared_timezone <- renderText(snapshot()$display_timezone)
    output$source_timezone <- renderText(snapshot()$display_timezone)
    output$raw_rows <- renderText(format(
      nrow(snapshot()$raw_data),
      big.mark = ",",
      scientific = FALSE
    ))
    output$prepared_rows <- renderText(format(
      nrow(snapshot()$prepared_data),
      big.mark = ",",
      scientific = FALSE
    ))
    output$summary_participants <- renderUI({
      count <- length(snapshot()$participants)
      dashboard_value_box(
        "Participants",
        format(count, big.mark = ",", scientific = FALSE),
        "users",
        "Number of distinct participant IDs in the pre-processed data."
      )
    })
    output$summary_date_span <- renderUI({
      dashboard_value_box(
        "Date span",
        dashboard_compact_span_label(snapshot()),
        "calendar",
        paste(
          dashboard_span_label(snapshot()),
          "Earliest through latest participant-local date, displayed in the",
          "dataset analysis timezone.",
          sep = ". "
        )
      )
    })
    output$summary_sampling <- renderUI({
      dashboard_value_box(
        "Sampling",
        dashboard_sampling_label(snapshot()$quality),
        "history",
        paste(
          "Distinct dominant timestamp epochs estimated per participant.",
          "Irregular timestamps are reported separately."
        )
      )
    })
    output$summary_missingness <- renderUI({
      value <- preprocessed_focus_view()
      scope <- missingness_scope()
      missingness <- value$missingness[
        value$missingness$scope == scope,
        ,
        drop = FALSE
      ]
      req(nrow(missingness) == 1L)
      missing_count <- missingness$missing_count[[1L]]
      missing_percent <- missingness$missing_percent[[1L]]
      display <- if (
        !is.finite(missing_count) ||
          !is.finite(missing_percent)
      ) {
        "Not estimable"
      } else {
        paste0(
          format(missing_count, big.mark = ",", scientific = FALSE),
          " (",
          format(round(missing_percent, 2), trim = TRUE, nsmall = 2),
          "%)"
        )
      }
      diagnostic <- if (
        is.finite(missingness$off_grid_count[[1L]]) &&
          missingness$off_grid_count[[1L]] > 0
      ) {
        paste0(
          " ",
          format(
            missingness$off_grid_count[[1L]],
            big.mark = ",",
            scientific = FALSE
          ),
          " irregular interval(s) are reported separately and are not ",
          "automatically treated as missing."
        )
      } else {
        ""
      }
      dashboard_value_box(
        paste0("Missing ", value$focus_variable),
        display,
        "question-circle",
        paste0(
          missingness$label[[1L]],
          ". ",
          missingness$denominator[[1L]],
          " Missingness concerns the selected pre-processed focus metric only.",
          diagnostic
        ),
        control = htmltools::tagAppendAttributes(
          bslib::popover(
            tags$button(
              type = "button",
              class = paste(
                "btn btn-sm btn-outline-secondary",
                "llw-dashboard-missingness-trigger"
              ),
              title = paste0(
                "Change missingness denominator (current: ",
                missingness$label[[1L]],
                ")"
              ),
              `aria-label` = paste0(
                "Change missingness denominator; current selection: ",
                missingness$label[[1L]]
              ),
              icon("chevron-down")
            ),
            title = "Missingness denominator",
            placement = "bottom",
            tags$div(
              class = "llw-dashboard-missingness-popover",
              radioButtons(
                session$ns("missingness_scope"),
                label = NULL,
                choices = c(
                  "Recorded times" = "recorded",
                  "Regular span" = "regular",
                  "Full days" = "full_days"
                ),
                selected = scope
              )
            )
          ),
          class = "llw-dashboard-missingness-scope"
        )
      )
    })

    output$explore_context <- renderUI({
      value <- explore_focus_view()
      chosen <- selection()
      resolved <- if (identical(chosen$mode, "detailed")) {
        "Detailed timeline"
      } else {
        "Focus coverage overview"
      }
      resolved_label <- if (identical(chosen$view_mode, "auto")) {
        paste0(resolved, " (automatic)")
      } else {
        paste0(resolved, " (selected)")
      }
      resolved_detail <- if (!identical(chosen$view_mode, "auto")) {
        paste(resolved, "was selected explicitly.")
      } else if (identical(chosen$mode, "detailed")) {
        paste0(
          "Automatic chose the detailed timeline because the longest ",
          "available focus-metric span in scope is ",
          chosen$available_span_days,
          " day(s)."
        )
      } else {
        paste0(
          "Automatic chose the focus coverage overview because the longest ",
          "available focus-metric span in scope is ",
          chosen$available_span_days,
          " day(s)."
        )
      }
      tags$p(
        class = "llw-secondary small mb-0",
        tags$span(paste0(
          "Showing ",
          tolower(value$stage_label),
          " \u00b7 focus metric ",
          value$focus_variable,
          " \u00b7 ",
          value$focus_unit,
          " \u00b7 "
        )),
        tooltip2(
          tags$span(
            class = "llw-dashboard-resolved-view",
            icon(
              if (identical(chosen$mode, "detailed")) {
                "chart-line"
              } else {
                "table-list"
              }
            ),
            resolved_label
          ),
          resolved_detail,
          placement = "bottom"
        ),
        "."
      )
    })

    output$scale_control <- renderUI({
      if (!identical(selection()$mode, "detailed")) return(NULL)
      selectInput(
        session$ns("plot_scale"),
        "Scale",
        choices = c(
          "Symlog" = "symlog",
          "Linear" = "linear",
          "Logarithmic" = "log"
        ),
        selected = plot_scale_preference() %||%
          view_recommendation()$plot_scale,
        selectize = FALSE
      )
    })

    output$symlog_threshold_control <- renderUI({
      if (
        !identical(selection()$mode, "detailed") ||
          !identical(plot_scale(), "symlog")
      ) {
        return(NULL)
      }
      selectInput(
        session$ns("symlog_threshold"),
        "Symlog linear range",
        choices = c(
          "\u00b110" = "10",
          "\u00b11 (default)" = "1",
          "\u00b10.1" = "0.1",
          "\u00b10.01" = "0.01",
          "\u00b10.001" = "0.001"
        ),
        selected = as.character(
          symlog_threshold_preference() %||%
            view_recommendation()$symlog_threshold
        ),
        selectize = FALSE
      )
    })

    output$log_omission_badge <- renderUI({
      if (
        !identical(selection()$mode, "detailed") ||
          !identical(plot_scale(), "log")
      ) {
        return(NULL)
      }
      values <- preview()$data[["Value"]]
      zero_count <- sum(values == 0, na.rm = TRUE)
      negative_count <- sum(values < 0, na.rm = TRUE)
      omitted_count <- zero_count + negative_count
      detail <- tags$div(
        tags$ul(
          class = "mb-1 ps-3",
          tags$li(
            format(zero_count, big.mark = ",", scientific = FALSE),
            " exact zero value(s)"
          ),
          tags$li(
            format(negative_count, big.mark = ",", scientific = FALSE),
            " negative value(s)"
          )
        ),
        tags$p(
          class = "mb-0",
          paste(
            "Both counts are omitted from this logarithmic display only.",
            "The recorded values remain unchanged."
          )
        )
      )
      tooltip2(
        tags$span(
          class = paste(
            "badge llw-log-omission-badge",
            if (omitted_count > 0L) {
              "text-bg-warning"
            } else {
              "text-bg-success"
            }
          ),
          paste(
            format(omitted_count, big.mark = ",", scientific = FALSE),
            if (omitted_count == 1L) "value omitted" else "values omitted"
          )
        ),
        detail,
        placement = "bottom"
      )
    })

    output$y_range_control <- renderUI({
      if (!identical(selection()$mode, "detailed")) return(NULL)
      bslib::popover(
        trigger = tags$button(
          type = "button",
          class = paste(
            "btn btn-sm btn-outline-secondary",
            "llw-dashboard-y-range-trigger"
          ),
          icon("sliders"),
          "Y range"
        ),
        title = "Manual y-axis range",
        placement = "bottom",
        tags$div(
          class = "llw-dashboard-y-range",
          radioButtons(
            session$ns("y_axis_scope"),
            "Automatic range",
            choices = c(
              "Shared across pages" = "shared",
              "Fit the current page" = "page"
            ),
            selected = isolate(input$y_axis_scope %||% "shared")
          ),
          uiOutput(session$ns("symlog_threshold_control")),
          textInput(
            session$ns("y_axis_min"),
            "Minimum (optional)",
            value = isolate(input$y_axis_min %||% "")
          ),
          textInput(
            session$ns("y_axis_max"),
            "Maximum (optional)",
            value = isolate(input$y_axis_max %||% "")
          ),
          tags$p(
            class = "llw-secondary small mb-0",
            "Leave either field blank to retain the automatic limit."
          )
        )
      )
    })

    output$plot_mode <- renderUI({
      value <- selection()
      resolved <- if (identical(value$mode, "detailed")) {
        "Detailed timeline"
      } else {
        "Focus coverage overview"
      }
      label <- if (identical(value$view_mode, "auto")) {
        paste("Automatic", "\u2192", resolved)
      } else {
        resolved
      }
      explanation <- if (!identical(value$view_mode, "auto")) {
        paste(resolved, "was selected explicitly.")
      } else if (identical(value$mode, "detailed")) {
        paste0(
          "Automatic chose the detailed timeline because no selected ",
          "participant has more than ",
          value$available_span_days,
          " day(s) of available focus-metric data in the selected scope."
        )
      } else {
        paste0(
          "Automatic chose the coverage overview because at least one selected ",
          "participant spans ",
          value$available_span_days,
          " day(s) of available focus-metric data in the selected scope."
        )
      }
      tooltip2(
        tags$span(class = "llw-dashboard-resolved-view", label),
        explanation,
        placement = "bottom"
      )
    })

    observeEvent(
      input$plot_scale,
      {
        plot_scale_preference(dashboard_plot_scale_value(input$plot_scale))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$symlog_threshold,
      {
        symlog_threshold_preference(
          dashboard_symlog_threshold_value(input$symlog_threshold)
        )
      },
      ignoreInit = TRUE
    )

    plot_scale <- reactive({
      dashboard_plot_scale_value(
        plot_scale_preference(),
        fallback = view_recommendation()$plot_scale
      )
    })

    symlog_threshold <- reactive({
      dashboard_symlog_threshold_value(
        symlog_threshold_preference(),
        fallback = view_recommendation()$symlog_threshold
      )
    })

    y_axis_limits <- reactive({
      dashboard_y_axis_limits(
        minimum = input$y_axis_min,
        maximum = input$y_axis_max,
        scale = plot_scale()
      )
    })

    y_axis_scope <- reactive({
      value <- input$y_axis_scope %||% "shared"
      if (value %in% c("shared", "page")) value else "shared"
    })

    recommendation_state <- reactive({
      state <- dashboard_recommendation_state(
        selection(),
        view_recommendation(),
        plot_scale = plot_scale(),
        symlog_threshold = symlog_threshold()
      )
      range <- y_axis_limits()
      if (isTRUE(range$active)) {
        state$active <- FALSE
        state$differences <- unique(c(
          state$differences,
          "manual y-axis range"
        ))
      }
      if (!identical(y_axis_scope(), "shared")) {
        state$active <- FALSE
        state$differences <- unique(c(
          state$differences,
          "page-specific y-axis range"
        ))
      }
      state
    })

    output$show_all_warning <- renderUI({
      value <- selection()
      warning <- value$show_all_warning
      if (!isTRUE(value$show_all) || is.null(warning)) return(NULL)
      tooltip2(
        tags$div(
          class = paste(
            "llw-dashboard-participant-scope--all",
            "llw-participant-scope__all"
          ),
          role = "status",
          tabindex = "0",
          icon("users"),
          tags$span(
            tags$strong(
              paste(
                "All",
                format(
                  length(value$participants),
                  big.mark = ",",
                  scientific = FALSE
                ),
                "participants"
              )
            ),
            tags$small(
              paste0(
                value$participant_pages,
                " page(s) \u00b7 max ",
                value$participants_per_page,
                "/page"
              )
            )
          )
        ),
        warning,
        placement = "right"
      )
    })

    output$dashboard_plot <- renderPlot(
      {
        value <- preview()
        if (identical(value$mode, "detailed")) {
          focus_values <- value$data[["Value"]]
          has_focus <- any(
            !is.na(focus_values) &
              if (is.numeric(focus_values)) {
                is.finite(focus_values)
              } else {
                rep(TRUE, length(focus_values))
              }
          )
          validate(need(
            has_focus,
            paste(
              "No focus measurements are available for this participant and",
              "time page. Move to another participant or time page."
            )
          ))
          range <- y_axis_limits()
          validate(need(is.null(range$message), range$message))
          plot_dashboard_timeline(
            snapshot(),
            value,
            mode = resolved_color_mode(),
            scale = plot_scale(),
            symlog_threshold = symlog_threshold(),
            y_limits = range,
            y_scope = y_axis_scope()
          )
        } else {
          plot_dashboard_availability(
            snapshot(),
            value,
            mode = resolved_color_mode()
          )
        }
      },
      height = function() {
        dashboard_plot_height(preview())
      },
      res = 96
    )

    output$provenance_table <- renderTable(
      dashboard_provenance_table(snapshot()),
      striped = TRUE,
      bordered = FALSE,
      spacing = "s"
    )
    output$state_table <- renderTable(
      dashboard_state_table(snapshot()),
      striped = TRUE,
      bordered = FALSE,
      spacing = "s"
    )

    preprocessed_integrity_item <- function(
      check,
      title,
      status,
      summary,
      detail
    ) {
      tags$div(
        class = paste(
          "llw-preprocessed-integrity__item",
          paste0("llw-preprocessed-integrity__item--", status),
          "list-group-item"
        ),
        role = "listitem",
        `data-check` = check,
        `data-status` = status,
        tags$div(
          class = "llw-preprocessed-integrity__item-heading",
          tags$h3(class = "h6 mb-0", title),
          raw_import_diagnostic_status_ui(status)
        ),
        tags$p(
          class = "llw-preprocessed-integrity__summary mb-1",
          summary
        ),
        tags$p(
          class = "llw-preprocessed-integrity__detail llw-secondary small mb-0",
          detail
        )
      )
    }

    output$preprocessed_integrity <- renderUI({
      value <- snapshot()
      data <- value$prepared_data
      source <- value$raw_data
      focus <- selected_focus_variable()
      count_label <- function(number) {
        format(number, big.mark = ",", scientific = FALSE, trim = TRUE)
      }

      row_count <- nrow(data)
      source_row_count <- nrow(source)
      row_delta <- row_count - source_row_count
      row_status <- if (row_count == 0L) {
        "warning"
      } else if (
        row_delta != 0L &&
          (
            isTRUE(value$recipe$unchanged) ||
              value$recipe$step_count == 0L
          )
      ) {
        "warning"
      } else if (row_delta == 0L) {
        "pass"
      } else {
        "information"
      }
      row_difference <- if (row_delta == 0L) {
        "the row count matches the source"
      } else {
        direction <- if (row_delta > 0L) "more" else "fewer"
        paste(
          count_label(abs(row_delta)),
          direction,
          "row(s) than the source"
        )
      }
      row_detail <- if (row_delta == 0L) {
        paste(
          "Equal row counts do not imply equal values; recipe provenance",
          "below remains authoritative."
        )
      } else if (value$recipe$step_count > 0L) {
        paste(
          "A changed row count can be valid when it is explained by committed",
          "filtering, aggregation, or expansion steps."
        )
      } else {
        paste(
          "No committed recipe step explains this row-count difference.",
          "Review the active result before analysis."
        )
      }

      id_present <- "Id" %in% names(data)
      id_values <- if (id_present) as.character(data$Id) else character()
      missing_identity <- if (id_present) {
        sum(is.na(id_values) | !nzchar(trimws(id_values)))
      } else {
        row_count
      }
      participant_count <- if (id_present) {
        length(unique(id_values[
          !is.na(id_values) &
            nzchar(trimws(id_values))
        ]))
      } else {
        0L
      }
      identity_status <- if (
        !id_present ||
          missing_identity > 0L ||
          participant_count == 0L
      ) {
        "warning"
      } else {
        "pass"
      }

      datetime_present <- "Datetime" %in% names(data)
      datetime_is_posix <- datetime_present &&
        inherits(data$Datetime, "POSIXct")
      datetime_numeric <- if (datetime_is_posix) {
        as.numeric(data$Datetime)
      } else {
        rep(NA_real_, row_count)
      }
      missing_datetime <- if (datetime_present) {
        sum(is.na(data$Datetime))
      } else {
        row_count
      }
      nonfinite_datetime <- if (datetime_is_posix) {
        sum(!is.na(datetime_numeric) & !is.finite(datetime_numeric))
      } else {
        0L
      }
      valid_key <- id_present &&
        datetime_is_posix
      duplicate_keys <- if (isTRUE(valid_key)) {
        usable <- !is.na(id_values) &
          nzchar(trimws(id_values)) &
          is.finite(datetime_numeric)
        if (any(usable)) {
          sum(duplicated(data.frame(
            Id = id_values[usable],
            Datetime = datetime_numeric[usable],
            stringsAsFactors = FALSE
          )))
        } else {
          0L
        }
      } else {
        NA_integer_
      }
      timestamp_status <- if (
        !datetime_is_posix ||
          missing_datetime > 0L ||
          nonfinite_datetime > 0L ||
          is.na(duplicate_keys) ||
          duplicate_keys > 0L
      ) {
        "warning"
      } else {
        "pass"
      }

      focus_present <- focus %in% names(data)
      focus_numeric <- focus_present && is.numeric(data[[focus]])
      focus_values <- if (focus_numeric) {
        data[[focus]]
      } else {
        rep(NA_real_, row_count)
      }
      focus_missing <- sum(is.na(focus_values))
      focus_nonfinite <- sum(
        !is.na(focus_values) &
          !is.finite(focus_values)
      )
      focus_finite <- sum(is.finite(focus_values))
      focus_status <- if (
        !focus_numeric ||
          focus_finite == 0L ||
          focus_nonfinite > 0L
      ) {
        "warning"
      } else if (focus_missing > 0L) {
        "information"
      } else {
        "pass"
      }

      recipe_unexplained <- !isTRUE(value$recipe$unchanged) &&
        value$recipe$step_count == 0L
      recipe_status <- if (recipe_unexplained) {
        "warning"
      } else if (isTRUE(value$recipe$unchanged)) {
        "pass"
      } else {
        "information"
      }

      tags$div(
        class = paste(
          "llw-preprocessed-integrity__list",
          "list-group list-group-flush"
        ),
        role = "list",
        preprocessed_integrity_item(
          "rows",
          "Active rows",
          row_status,
          paste0(
            count_label(row_count),
            " active row(s); ",
            row_difference,
            "."
          ),
          row_detail
        ),
        preprocessed_integrity_item(
          "identity",
          "Participant identity",
          identity_status,
          paste0(
            count_label(participant_count),
            " participant identity value(s); ",
            count_label(missing_identity),
            " row(s) have a missing or blank Id."
          ),
          paste(value$grouping$label, value$grouping$detail)
        ),
        preprocessed_integrity_item(
          "timestamps",
          "Participant timestamps",
          timestamp_status,
          paste0(
            if (datetime_is_posix) "POSIXct timestamps; " else
              "Datetime is not POSIXct; ",
            count_label(missing_datetime + nonfinite_datetime),
            " missing or non-finite timestamp(s); ",
            if (is.na(duplicate_keys)) {
              "duplicate keys not estimable."
            } else {
              paste0(
                count_label(duplicate_keys),
                " repeated participant\u2013timestamp key(s)."
              )
            }
          ),
          paste(
            "Checks the active Id–Datetime key after recipe application.",
            "Repeated keys are reported for review and are not removed here."
          )
        ),
        preprocessed_integrity_item(
          "focus",
          paste("Focus metric:", focus),
          focus_status,
          paste0(
            count_label(focus_finite),
            " finite value(s); ",
            count_label(focus_missing),
            " missing; ",
            count_label(focus_nonfinite),
            " non-finite."
          ),
          paste(
            "Missing values can be an intentional result of masking. Exact",
            "zero remains a valid finite observation and is not called missing."
          )
        ),
        preprocessed_integrity_item(
          "recipe",
          "Recipe traceability",
          recipe_status,
          paste0(
            value$recipe$label,
            "; ",
            count_label(value$recipe$enabled_count),
            " enabled step(s), revision r",
            value$recipe$dataset_revision,
            "."
          ),
          paste(value$recipe$detail, value$grouping$detail)
        )
      )
    })

    coverage_total <- function(column) {
      values <- preprocessed_focus_view()$coverage[[column]]
      if (length(values) == 0L || all(is.na(values))) return(NA_real_)
      sum(values, na.rm = TRUE)
    }

    quality_box <- function(title, value, icon_name, explanation) {
      display <- if (is.na(value)) {
        "Not estimable"
      } else {
        format(value, big.mark = ",", scientific = FALSE)
      }
      outcome <- if (is.na(value)) {
        "Review"
      } else if (value == 0) {
        "None detected"
      } else {
        "Review"
      }
      dashboard_value_box(
        title,
        display,
        icon_name,
        explanation,
        tone = dashboard_quality_tone(value),
        outcome = outcome
      )
    }

    output$quality_explicit_missing <- renderUI({
      focus <- preprocessed_focus_view()$focus_variable
      quality_box(
        paste0("Explicit missing ", focus, " epochs"),
        coverage_total("Explicit missing focus epochs"),
        "minus-circle",
        paste(
          "Regular timestamps that exist but have a missing value for the",
          "selected pre-processed focus metric. Zero is a positive result."
        )
      )
    })
    output$quality_implicit_gaps <- renderUI({
      quality_box(
        "Implicit gap epochs",
        coverage_total("Implicit gap epochs"),
        "arrows-alt",
        paste(
          "Complete dominant-epoch positions absent between consecutive",
          "timestamps, plus unrecorded full-day boundary epochs. Ordinary",
          "phase shifts do not count. Zero is a positive result."
        )
      )
    })
    output$quality_irregular <- renderUI({
      quality_box(
        "Timing-jitter intervals",
        coverage_total("Irregular timestamps"),
        "shuffle",
        paste(
          "Consecutive timestamp intervals that are not a whole multiple of",
          "the participant's dominant epoch. They are reported separately,",
          "not treated as missing. Zero is a positive result."
        )
      )
    })
    output$quality_dst <- renderUI({
      value <- preprocessed_focus_view()$quality$summary$dst_transitions
      if (is.null(value) || length(value) != 1L) value <- NA_real_
      quality_box(
        "DST transitions",
        as.numeric(value),
        "clock",
        paste(
          "Detected daylight-saving transitions requiring local-time review.",
          "Zero is shown as a positive result."
        )
      )
    })

    output$quality_note <- renderUI({
      value <- preprocessed_focus_view()$quality
      if (length(value$warnings) == 0L) {
        return(llw_status_callout(
          "complete",
          paste(
            "No quality warning is recorded for the active pre-processed",
            "table. Darkness, missing sensor values, gaps, non-wear, and sleep",
            "remain distinct states."
          ),
          heading = "Pre-processed quality checks",
          compact = TRUE
        ))
      }
      llw_status_callout(
        "warning",
        paste(
          length(value$warnings),
          "recorded warning(s):",
          paste(value$warnings, collapse = " | ")
        ),
        heading = "Pre-processed quality checks need review",
        compact = TRUE
      )
    })

    output$merged_import_note <- renderUI({
      if (
        !identical(
          snapshot()$record$source_manifest$source_type,
          "append_merge"
        )
      ) {
        return(tags$p(
          class = "llw-secondary small",
          "These checks describe the immutable imported source table."
        ))
      }
      llw_status_callout(
        "idle",
        paste(
          "For an appended dataset, these checks are recomputed on the combined",
          "source table. Source-specific manifests and checks remain retained",
          "under append provenance."
        ),
        heading = "Combined-source checks",
        compact = TRUE
      )
    })

    output$quality_diagnostics <- renderUI({
      raw_import_diagnostics_table_ui(
        snapshot()$quality$diagnostics
      )
    })

    output$variable_inventory <- DT::renderDataTable({
      inventory <- preprocessed_focus_view()$variable_inventory
      widget <- dashboard_datatable(
        inventory,
        "Pre-processed variable inventory and missingness",
        column_visibility = FALSE
      )
      widget$x$options$dom <- "ftip"
      widget$x$options$paging <- TRUE
      widget$x$options$pageLength <- table_contract$page_length
      widget
    }, server = table_contract$server)

    output$coverage_table <- DT::renderDataTable(
      dashboard_datatable(
        dashboard_coverage_display(preprocessed_focus_view()$coverage),
        paste0(
          "Participant-day focus coverage in ",
          snapshot()$display_timezone
        ),
        column_visibility = FALSE
      ),
      server = table_contract$server
    )

    output$prepared_state_note <- renderUI({
      value <- snapshot()
      llw_status_callout(
        if (value$recipe$unchanged) "complete" else "warning",
        paste0(
          value$recipe$detail,
          " Recipe schema v",
          value$recipe$schema_version,
          "; dataset/recipe revision r",
          value$recipe$dataset_revision,
          ". ",
          value$grouping$label,
          ": ",
          value$grouping$detail
        ),
        heading = value$recipe$label,
        compact = TRUE
      )
    })

    output$raw_state_note <- renderUI({
      value <- snapshot()
      llw_status_callout(
        "idle",
        paste0(
          "Source checksum: ",
          value$record$raw_checksum,
          ". This view is not modified by table search, sorting, visibility, ",
          "or pagination."
        ),
        heading = "Immutable source data",
        compact = TRUE
      )
    })

    preprocessed_column_selection <- reactiveVal(list(
      initialized = FALSE,
      selected = character()
    ))
    source_column_selection <- reactiveVal(list(
      initialized = FALSE,
      selected = character()
    ))

    table_column_spec <- function(stage) {
      value <- snapshot()
      if (identical(stage, "preprocessed")) {
        data <- value$prepared_data
      } else {
        data <- value$raw_data
      }
      main <- dashboard_main_columns(
        value,
        selected_focus_variable(),
        stage
      )
      list(
        data = data,
        main = main,
        additional = setdiff(names(data), main)
      )
    }

    column_selection_state <- function(stage) {
      if (identical(stage, "preprocessed")) {
        preprocessed_column_selection()
      } else {
        source_column_selection()
      }
    }

    selected_table_columns <- function(stage) {
      spec <- table_column_spec(stage)
      main_only_id <- if (identical(stage, "preprocessed")) {
        "preprocessed_main_only"
      } else {
        "source_main_only"
      }
      if (isTRUE(input[[main_only_id]] %||% TRUE)) {
        return(spec$main)
      }
      state <- column_selection_state(stage)
      additional <- if (isTRUE(state$initialized)) {
        intersect(state$selected, spec$additional)
      } else {
        spec$additional
      }
      requested <- unique(c(spec$main, additional))
      names(spec$data)[names(spec$data) %in% requested]
    }

    column_selector_ui <- function(stage) {
      spec <- table_column_spec(stage)
      preprocessed <- identical(stage, "preprocessed")
      main_only_id <- if (preprocessed) {
        "preprocessed_main_only"
      } else {
        "source_main_only"
      }
      selector_id <- if (preprocessed) {
        "preprocessed_columns"
      } else {
        "source_columns"
      }
      state <- isolate(column_selection_state(stage))
      if (isTRUE(input[[main_only_id]] %||% TRUE)) {
        return(tags$button(
          type = "button",
          class = paste(
            "btn btn-outline-secondary btn-sm",
            "llw-dashboard-column-selector"
          ),
          disabled = "disabled",
          title = "Turn off Main columns only to choose additional columns.",
          icon("columns"),
          "Choose columns"
        ))
      }
      if (length(spec$additional) == 0L) {
        return(tags$button(
          type = "button",
          class = paste(
            "btn btn-outline-secondary btn-sm",
            "llw-dashboard-column-selector"
          ),
          disabled = "disabled",
          title = "This dataset has no additional columns.",
          icon("columns"),
          "No additional columns"
        ))
      }
      selected <- if (isTRUE(state$initialized)) {
        intersect(state$selected, spec$additional)
      } else {
        spec$additional
      }
      bslib::popover(
        tags$button(
          type = "button",
          class = paste(
            "btn btn-outline-secondary btn-sm",
            "llw-dashboard-column-selector"
          ),
          icon("columns"),
          "Choose columns"
        ),
        title = "Visible additional columns",
        tags$p(
          class = "llw-secondary small",
          paste(
            "Grouping, Id, Datetime, and the focus metric remain visible.",
            "Select any additional columns to show."
          )
        ),
        selectizeInput(
          session$ns(selector_id),
          "Additional columns",
          choices = spec$additional,
          selected = selected,
          multiple = TRUE,
          options = list(closeAfterSelect = TRUE)
        )
      )
    }

    output$preprocessed_column_selector <- renderUI({
      column_selector_ui("preprocessed")
    })
    output$source_column_selector <- renderUI({
      column_selector_ui("source")
    })

    observeEvent(
      input$preprocessed_columns,
      {
        if (!isTRUE(input$preprocessed_main_only %||% TRUE)) {
          preprocessed_column_selection(list(
            initialized = TRUE,
            selected = input$preprocessed_columns %||% character()
          ))
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )
    observeEvent(
      input$source_columns,
      {
        if (!isTRUE(input$source_main_only %||% TRUE)) {
          source_column_selection(list(
            initialized = TRUE,
            selected = input$source_columns %||% character()
          ))
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )
    preprocessed_table_data <- reactive({
      value <- snapshot()
      data <- value$prepared_data
      columns <- selected_table_columns("preprocessed")
      data[, columns, drop = FALSE]
    })

    source_table_data <- reactive({
      value <- snapshot()
      data <- value$raw_data
      columns <- selected_table_columns("source")
      data[, columns, drop = FALSE]
    })

    output$prepared_table <- DT::renderDataTable(
      dashboard_datatable(
        preprocessed_table_data(),
        paste0(
          "Active pre-processed data; timestamps displayed in ",
          snapshot()$display_timezone
        ),
        column_visibility = FALSE,
        display_timezone = snapshot()$display_timezone
      ),
      server = table_contract$server
    )

    output$raw_table <- DT::renderDataTable(
      dashboard_datatable(
        source_table_data(),
        paste0(
          "Immutable source data; timestamps displayed in ",
          snapshot()$display_timezone
        ),
        column_visibility = FALSE,
        display_timezone = snapshot()$display_timezone
      ),
      server = table_contract$server
    )

    list(
      event = reactive(event_value()),
      recommendation = reactive(view_recommendation()),
      status = reactive({
        value <- snapshot_result()
        if (is.null(value)) {
          return(list(state = "empty", ready = FALSE))
        }
        if (inherits(value, "error")) {
          return(list(
            state = "error",
            ready = FALSE,
            message = llw_public_message(value)
          ))
        }
        chosen <- selection()
        shown <- preview()
        recommended <- view_recommendation()
        recommendation_status <- recommendation_state()
        list(
          state = "ready",
          ready = TRUE,
          dataset_id = value$record$id,
          plot_mode = chosen$mode,
          focus_variable = shown$focus_variable,
          missingness_scope = missingness_scope(),
          data_stage = shown$data_stage,
          plot_scale = if (identical(chosen$mode, "detailed")) {
            plot_scale()
          } else {
            "not_applicable"
          },
          symlog_threshold = if (
            identical(chosen$mode, "detailed") &&
              identical(plot_scale(), "symlog")
          ) {
            symlog_threshold()
          } else {
            NA_real_
          },
          y_axis_limits = if (
            identical(chosen$mode, "detailed") &&
              isTRUE(y_axis_limits()$active)
          ) {
            y_axis_limits()$limits
          } else {
            c(NA_real_, NA_real_)
          },
          requested_participants = length(chosen$participants),
          time_basis = chosen$time_basis,
          view_mode = chosen$view_mode,
          available_span_days = chosen$available_span_days,
          participant_page = chosen$participant_page,
          participant_pages = chosen$participant_pages,
          participants_per_page = chosen$participants_per_page,
          time_page = chosen$time_page,
          time_pages = chosen$time_pages,
          time_page_label = chosen$time_page_label,
          days_per_page = chosen$days_per_page,
          recommendation_active = recommendation_status$active,
          recommendation_summary = recommended$summary,
          recommendation_differences = recommendation_status$differences,
          facet_page = chosen$facet_page,
          facet_pages = chosen$facet_pages,
          source_plot_rows = shown$source_rows,
          displayed_plot_rows = shown$displayed_rows,
          plot_reduced = shown$reduced,
          prepared_rows = nrow(value$prepared_data),
          raw_rows = nrow(value$raw_data),
          recipe_state = value$recipe$state,
          grouping_state = value$grouping$state
        )
      })
    )
  })
}

dashboard_showcase_record <- function(
  participants = 2L,
  days = 2L,
  epoch_seconds = 900,
  display_name = "Small dashboard fixture"
) {
  participant_ids <- sprintf("P%02d", seq_len(participants))
  instants <- seq(
    as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    by = epoch_seconds,
    length.out = days * 86400 / epoch_seconds
  )
  data <- data.frame(
    Id = rep(participant_ids, each = length(instants)),
    Datetime = rep(instants, times = participants),
    MEDI = unlist(lapply(seq_len(participants), function(index) {
      phase <- seq_along(instants) / (86400 / epoch_seconds) * 2 * pi
      pmax(0, 120 + 100 * sin(phase - pi / 2) + index * 5)
    })),
    stringsAsFactors = FALSE
  )
  quality <- summarize_raw_import_quality(data, "UTC")
  new_dataset_record(
    raw_data = data,
    display_name = display_name,
    source_manifest = new_source_manifest(
      source_type = "dashboard_showcase",
      source_timezone = "UTC",
      details = list(epoch_seconds = epoch_seconds)
    ),
    analysis_settings = list(
      primary_variable = "MEDI",
      analysis_timezone = "UTC"
    ),
    provenance = list(
      LightLogR_version = installed_package_version("LightLogR"),
      raw_import_quality = quality,
      primary_variable_eligibility = quality$eligibility
    )
  )
}

dashboard_showcase_catalog <- function(project_root = NULL) {
  catalog <- dataset_example_catalog(project_root)
  keys <- c(
    "sample",
    "iztech",
    "actlumus_all",
    "speccy_all",
    "veet_02_als",
    "veet_02_pho"
  )
  catalog <- catalog[match(keys, catalog$key), , drop = FALSE]
  catalog <- catalog[!is.na(catalog$key) & catalog$available, , drop = FALSE]
  showcase_labels <- c(
    sample = "LightLogR testdata - sample.data.environment",
    iztech = "IZTECH light glasses - 151,200 rows",
    actlumus_all = "All ActLumus files - 839,607 rows / 20 IDs",
    speccy_all = "All Speccy files - 121,060 rows / 3 IDs",
    veet_02_als = "02_VEET_L ambient-light sensor - 173,007 rows",
    veet_02_pho = "02_VEET_L spectral sensor - 173,013 rows"
  )
  catalog$label <- unname(showcase_labels[catalog$key])
  catalog$description <- vapply(
    catalog$key,
    function(key) {
      switch(
        key,
        sample = paste(
          "Installed LightLogR sample with 69,120 recorded rows, two series,",
          "MEDI metadata, and Europe/Berlin local time."
        ),
        iztech = paste(
          "Pinned CC BY 4.0 MeLiDos IZTECH snapshot with 151,200 recorded",
          "rows, 37 variables, 17 anonymous participants, and",
          "Europe/Istanbul local time. Missing metadata remain unknown."
        ),
        actlumus_all = paste(
          "All 20 repository-provided ActLumus exports imported together",
          "through the validated LightLogR boundary: 839,607 rows, 27 columns,",
          "filename-stem IDs, and Europe/Berlin source time."
        ),
        speccy_all = paste(
          "All eight repository-provided Speccy exports imported together:",
          "121,060 rows and 92 columns. The expression ^(ID[0-9]{2}) combines",
          "file parts into ID01, ID02, and ID04. UTC is a visible development",
          "placeholder because no IANA timezone is recorded in the files."
        ),
        veet_02_als = paste(
          "The repository-provided 02_VEET_L source imported as ambient-light",
          "sensor data: 173,007 rows, 14 columns, focus default Lux, and",
          "Europe/Berlin source time."
        ),
        veet_02_pho = paste(
          "The same 02_VEET_L source imported as spectral-sensor data:",
          "173,013 rows, 18 columns, focus default Clear, and Europe/Berlin",
          "source time."
        )
      )
    },
    character(1)
  )
  rownames(catalog) <- NULL
  catalog
}

dataset_dashboard_app <- function(project_root = NULL, ...) {
  catalog <- dashboard_showcase_catalog(project_root)
  fixture_choices <- stats::setNames(catalog$key, catalog$label)
  fixture_choices <- c(
    fixture_choices,
    stats::setNames("empty", "No selected dataset")
  )
  ui <- lightlogweb_page(page_sidebar(
    title = "Real-data dashboard showcase",
    theme = lightlogweb_theme(),
    sidebar = sidebar(
      title = "Real local datasets",
      selectInput(
        "fixture",
        "Dataset state",
        choices = fixture_choices
      ),
      uiOutput("fixture_description"),
      tags$p(
        class = "llw-secondary small mb-0",
        paste(
          "These choices use the same immutable records as the dataset",
          "library. No rows are synthesized, filled, or imputed for this",
          "showcase."
        )
      ),
      tags$span(class = "visually-hidden", "Interface color mode"),
      input_dark_mode(id = "color_mode")
    ),
    lightlogweb_head(),
    lightlogweb_skip_link(),
    tags$main(
      id = "llw-main-content",
      class = "llw-main-shell llw-dashboard-shell",
      tabindex = "-1",
      tags$div(
        id = "llw-dashboard-showcase-loading",
        class = "llw-dashboard-loading-status",
        role = "status",
        `aria-live` = "polite",
        `aria-atomic` = "true",
        `aria-busy` = "false",
        hidden = "hidden",
        tags$span(
          class = "llw-dashboard-loading-status__icon",
          `aria-hidden` = "true",
          icon("spinner", class = "fa-spin")
        ),
        tags$div(
          tags$strong(
            id = "llw-dashboard-showcase-loading-label",
            "Loading selected dataset"
          ),
          tags$span(
            class = "llw-dashboard-loading-status__detail",
            paste(
              "The current dashboard remains visible until the replacement",
              "dataset is ready."
            )
          )
        )
      ),
      tags$script(HTML(
        "
        (() => {
          const statusId = 'llw-dashboard-showcase-loading';
          const labelId = 'llw-dashboard-showcase-loading-label';
          const showLoading = (select) => {
            const status = document.getElementById(statusId);
            const label = document.getElementById(labelId);
            if (!status || !label) return;
            const option = select.options[select.selectedIndex];
            const name = option ? option.textContent.trim() : 'selected dataset';
            label.textContent = `Loading ${name}`;
            status.hidden = false;
            status.setAttribute('aria-busy', 'true');
          };
          const hideLoading = () => {
            const status = document.getElementById(statusId);
            if (!status) return;
            status.hidden = true;
            status.setAttribute('aria-busy', 'false');
          };
          document.addEventListener('change', (event) => {
            if (event.target && event.target.id === 'fixture') {
              showLoading(event.target);
            }
          });
          if (window.jQuery) {
            window.jQuery(document).on(
              'shiny:inputchanged.llwDashboardLoading',
              (event) => {
                if (event.name === 'fixture') {
                  const select = document.getElementById('fixture');
                  if (select) showLoading(select);
                }
              }
            );
            window.jQuery(document).on(
              'shiny:value.llwDashboardLoading',
              (event) => {
                if (event.name === 'fixture_ready_signal') {
                  hideLoading();
                }
              }
            );
          }
        })();
        "
      )),
      tags$span(
        class = "visually-hidden",
        textOutput("fixture_ready_signal", inline = TRUE)
      ),
      datasetDashboardUI("dashboard"),
      card(
        class = "llw-dashboard-development-status",
        card_header("Development status (not shown in production)"),
        tableOutput("status")
      )
    )
  ))
  server <- function(input, output, session) {
    records <- new.env(parent = emptyenv())
    active_record <- reactive({
      key <- input$fixture %||% catalog$key[[1L]]
      if (identical(key, "empty")) return(NULL)
      if (!exists(key, envir = records, inherits = FALSE)) {
        record <- withProgress(
          message = "Importing the selected real dataset",
          detail = "This dataset is cached for the rest of this session.",
          value = 0.2,
          {
            value <- load_dataset_example(key, project_root)
            incProgress(0.8, detail = "Building dashboard summaries")
            value
          }
        )
        assign(key, record, envir = records)
      }
      get(key, envir = records, inherits = FALSE)
    })
    output$fixture_description <- renderUI({
      if (identical(input$fixture, "empty")) {
        return(tags$p(
          class = "llw-secondary small",
          "Use this state to inspect the dashboard's recoverable empty path."
        ))
      }
      selected <- catalog[catalog$key == input$fixture, , drop = FALSE]
      req(nrow(selected) == 1L)
      llw_status_callout(
        "idle",
        selected$description[[1L]],
        heading = "Recorded source",
        compact = TRUE
      )
    })
    dashboard <- datasetDashboardServer(
      "dashboard",
      dataset = active_record,
      active_panel = reactive("dashboard"),
      color_mode = reactive(input$color_mode %||% "light")
    )
    output$fixture_ready_signal <- renderText({
      key <- input$fixture %||% catalog$key[[1L]]
      record <- active_record()
      status <- dashboard$status()
      ready <- if (identical(key, "empty")) {
        identical(status$state, "empty")
      } else {
        isTRUE(status$ready) &&
          identical(status$dataset_id, record$id)
      }
      req(ready)
      paste(key, status$dataset_id %||% "empty", sep = ":")
    })
    outputOptions(
      output,
      "fixture_ready_signal",
      suspendWhenHidden = FALSE
    )
    output$status <- renderTable({
      value <- dashboard$status()
      data.frame(
        Field = names(value),
        Value = vapply(
          value,
          function(item) {
            paste(as.character(item), collapse = ", ")
          },
          character(1)
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
  }
  shinyApp(ui, server, ...)
}
