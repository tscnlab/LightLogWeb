# The recipe workspace shares the classic form controls and their server bindings.
preparation_flow_catalog <- function() {
  list(
    filter = list(
      title = "Keep the observations you need",
      note = "Remove rows by participant, time or a condition.",
      operations = list(
        participants = c(
          "Participants",
          "Choose people and recordings",
          "users"
        ),
        dates = c(
          "Date & time",
          "Choose dates or a daily time window",
          "calendar"
        ),
        duration = c(
          "Duration",
          "Keep a length from the start or end",
          "hourglass-half"
        ),
        photoperiod = c("Day / night", "Select light or dark periods", "sun"),
        variable = c(
          "Variable condition",
          "Keep rows matching a condition",
          "filter"
        ),
        coverage = c("Coverage", "Require enough available data", "chart-bar")
      )
    ),
    values = list(
      title = "Work on measurement values",
      note = "Keep the rows. Update a variable or write to a new one.",
      operations = list(
        invalidate = c("Valid range", "Mask values using a condition", "ruler"),
        calibrate = c(
          "Transform",
          "Scale, offset or transform values",
          "sliders"
        ),
        variability = c(
          "Variability",
          "Mask using a moving-window check",
          "wave-square"
        )
      )
    ),
    sampling = list(
      title = "Adjust time and sampling",
      note = "Change timestamps, declare gaps or combine readings.",
      operations = list(
        timezone = c(
          "Time zone",
          "Change the zone or clock interpretation",
          "globe"
        ),
        dst = c(
          "Device DST",
          "Correct a device's daylight-saving shift",
          "clock"
        ),
        gaps = c("Gaps", "Add rows for missing timestamps", "equals"),
        aggregate = c(
          "Aggregate",
          "Summarise into a longer interval",
          "layer-group"
        )
      )
    )
  )
}

prepare_flow_ui <- function(id) {
  ns <- shiny::NS(id)
  controls_ui <- prepare_ui(id)
  take <- function(selector)
    htmltools::tagQuery(controls_ui)$find(selector)$selectedTags()
  by_id <- function(name) take(paste0("#", ns(name)))

  editor <- htmltools::tagQuery(take(".llw-prepare-editor-card"))
  editor$find(".card-header")$before(shiny::tags$header(
    class = "llw-flow-panel-heading llw-flow-editor-heading",
    shiny::tags$div(
      shiny::tags$h3(
        id = ns("flow_heading_edit"),
        class = "llw-flow-destination",
        tabindex = "-1",
        shiny::textOutput(ns("flow_operation"), inline = TRUE)
      ),
      shiny::tags$span(class = "llw-flow-eyebrow", by_id("editor_title"))
    ),
    shiny::actionButton(
      ns("flow_change"),
      "Change operation",
      shiny::icon("arrows-rotate"),
      class = "btn-outline-secondary"
    )
  ))
  editor$find(".card-header")$remove()
  editor$find(".llw-prepare-save-actions")$remove()
  editor$find(paste0("#", ns("new_step")))$remove()
  # The operation catalogue drives the existing radio/tab controls. They stay
  # mounted for acknowledgements and switching an operation never rebuilds them.
  editor$find(paste0("#", ns("preparation_category")))$addClass(
    "llw-flow-binding d-none nav-hidden"
  )
  for (name in c("step_type", "value_type", "sampling_type"))
    editor$find(paste0("#", ns(name)))$addClass("llw-flow-binding d-none")

  editor$find(".llw-prepare-target")$prepend(shiny::tags$h4("Variables"))
  sections <- c(
    "input.value_type === 'variability'" = "Window and condition",
    "input.value_type === 'invalidate'" = "Condition",
    "input.value_type === 'calibrate'" = "Transformation",
    "input.sampling_type === 'aggregate'" = "Aggregation",
    "input.sampling_type === 'gaps'" = "Gap handling",
    "input.sampling_type === 'timezone'" = "Time interpretation"
  )
  editor$find(".shiny-panel-conditional")$each(function(tag, i) {
    condition <- tag$attribs[["data-display-if"]]
    # Only the main settings sections contain inputs. Explanatory conditional
    # text in disclosures keeps its existing hierarchy.
    controls <- htmltools::tagQuery(tag)
    if (condition %in% names(sections) &&
        (length(controls$find("input")$selectedTags()) ||
         length(controls$find("select")$selectedTags()))) {
      tag$attribs$class <- paste(tag$attribs$class, "llw-flow-settings-group")
      tag$children <- c(list(shiny::tags$h4(sections[[condition]])), tag$children)
    }
  })

  preview <- htmltools::tagQuery(take(".llw-prepare-impact-card"))
  preview$find(".card-header")$remove()
  preview$find(paste0("#", ns("action_status")))$remove()
  plot_controls <- preview$find(".llw-prepare-plot-controls")$selectedTags()
  day_actions <- preview$find(".llw-prepare-day-actions")$selectedTags()
  preview$find(".llw-prepare-plot-controls")$remove()
  preview$find(".llw-prepare-day-actions")$remove()
  preview$find(".llw-prepare-plot-layout")$before(
    shiny::tags$details(
      class = "llw-flow-preview-controls",
      shiny::tags$summary(shiny::icon("sliders"), "Comparison and day"),
      plot_controls,
      day_actions
    )
  )

  pick <- lapply(preparation_flow_catalog(), function(group) {
    shiny::tags$section(
      class = "llw-flow-operation-group",
      shiny::tags$h4(group$title),
      shiny::tags$p(group$note),
      shiny::tags$div(
        class = "llw-flow-operation-grid",
        lapply(names(group$operations), function(key) {
          item <- group$operations[[key]]
          shiny::actionButton(
            ns(paste0("flow_pick_", key)),
            shiny::tagList(
              shiny::icon(item[[3L]], class = "llw-flow-tile-icon"),
              shiny::tags$span(
                class = "llw-flow-tile-copy",
                shiny::tags$strong(item[[1L]]),
                shiny::tags$small(item[[2L]])
              ),
              shiny::icon("arrow-right", class = "llw-flow-tile-arrow")
            ),
            class = "llw-flow-operation-tile"
          )
        })
      )
    )
  })
  shiny::tags$section(
    id = ns("flow_root"),
    class = "llw-prepare llw-prepare-flow",
    `aria-label` = "Prepare dataset",
    htmltools::htmlDependency(
      name = "llw-prepare-flow-navigation",
      version = "0.1.0",
      src = c(file = lightlogweb_asset_dir()),
      script = "js/prepare-flow-navigation.js",
      all_files = FALSE
    ),
    shiny::tags$header(
      class = "llw-flow-intro",
      shiny::tags$h2("Prepare your data"),
      shiny::tags$p("Build a recipe. Review its effect. Apply when ready.")
    ),
    by_id("heading"),
    shiny::tags$div(
      class = "llw-flow-workspace",
      shiny::tags$aside(
        class = "llw-flow-rail",
        `aria-label` = "Recipe workspace",
        shiny::tags$div(
          class = "llw-flow-rail-intro",
          shiny::tags$header(
            class = "llw-flow-panel-heading",
            shiny::tags$h3(shiny::icon("list-check"), "Your recipe")
          ),
          by_id("chain_changes"),
          by_id("recipe_list")
        ),
        shiny::tags$div(
          class = "llw-flow-sequence",
          preparation_recipe_source_ui(),
          shiny::conditionalPanel(
            "output.has_steps === 'yes'",
            ns = ns,
            class = "llw-prepare-recipe-choices",
            by_id("recipe_choices")
          ),
          shiny::actionButton(
            ns("flow_add"), "Add step", shiny::icon("plus"),
            class = "btn-primary llw-flow-add-step"
          )
        ),
        shiny::conditionalPanel(
          "output.has_steps === 'yes'",
          ns = ns,
          shiny::tags$div(
            class = "llw-flow-selected-actions",
            shiny::tags$strong("Selected step"),
            take(".llw-prepare-recipe-actions")
          )
        ),
        by_id("pending_removals"),
        shiny::actionButton(
          ns("flow_result"),
          "Review result",
          shiny::icon("chart-line"),
          class = "llw-flow-review-link"
        ),
        shiny::tags$details(
          class = "llw-flow-recipe-tools",
          shiny::tags$summary("Recipe actions"),
          by_id("undo_edit"),
          by_id("reset"),
          shiny::tags$p(
            "Reset restores the original data and clears this recipe."
          )
        )
      ),
      shiny::tags$div(
        class = "llw-flow-feedback",
        by_id("action_status"),
        shiny::conditionalPanel(
          "output.flow_dirty === 'yes' && input.flow_view !== 'edit'",
          ns = ns,
          shiny::tags$div(
            class = "llw-flow-draft-note",
            role = "status",
            shiny::tags$span("You have unsaved step settings."),
            shiny::actionButton(
              ns("flow_resume"),
              "Return to step",
              shiny::icon("pen")
            )
          )
        )
      ),
      shiny::tags$div(
        class = "llw-flow-main",
        shiny::tags$div(
          class = "llw-flow-content",
          bslib::navset_hidden(
            id = ns("flow_view"),
            selected = "result",
            bslib::nav_panel(
              "Review",
              value = "result",
              shiny::tags$div(
                class = "llw-flow-stage-heading",
                shiny::tags$div(
                  shiny::tags$span(class = "llw-flow-eyebrow", "Review & apply"),
                  shiny::tags$h3(
                    id = ns("flow_heading_result"),
                    class = "llw-flow-destination",
                    tabindex = "-1",
                    "See the effect of your recipe"
                  ),
                  shiny::tags$p(
                    "Inspect the saved recipe, then apply it to your prepared dataset."
                  )
                ),
                shiny::conditionalPanel(
                  "output.flow_editing === 'yes' && output.flow_dirty !== 'yes'",
                  ns = ns,
                  shiny::actionButton(ns("flow_back"), "Back to settings",
                    shiny::icon("pen"))
                )
              )
            ),
            bslib::nav_panel(
              "Choose",
              value = "choose",
              shiny::tags$div(
                class = "llw-flow-stage-heading",
                shiny::tags$div(
                  shiny::tags$span(
                    class = "llw-flow-eyebrow",
                    "Choose an operation"
                  ),
                  shiny::tags$h3(
                    id = ns("flow_heading_choose"),
                    class = "llw-flow-destination",
                    tabindex = "-1",
                    "What would you like to do?"
                  )
                )
              ),
              pick
            ),
            bslib::nav_panel(
              "Configure",
              value = "edit",
              shiny::tags$div(
                class = "llw-prepare-workflow llw-flow-editor",
                editor$allTags(),
                shiny::tags$div(
                  class = "llw-flow-editor-actions",
                  shiny::conditionalPanel(
                    "output.flow_editing === 'yes'",
                    ns = ns,
                    by_id("save_step")
                  ),
                  by_id("save_new"),
                  by_id("new_step")
                )
              )
            )
          ),
          shiny::conditionalPanel(
            "input.flow_view !== 'choose'", ns = ns,
            class = "llw-flow-preview-pane",
            shiny::tags$section(
              class = "llw-flow-result-card",
              `aria-label` = "Recipe preview",
              shiny::tags$header(
                class = "llw-flow-panel-heading llw-flow-result-heading",
                shiny::tags$div(
                  shiny::tags$h3(shiny::icon("chart-line"), "Recipe preview"),
                  shiny::tags$p("Save a step to refresh. Apply when ready.")
                ),
                by_id("apply")
              ),
              shiny::conditionalPanel(
                "input.flow_view === 'edit'", ns = ns,
                shiny::actionButton(ns("flow_expand"), "Expand preview",
                  shiny::icon("expand"), class = "btn-outline-secondary btn-sm")
              ),
              preview$allTags(),
              shiny::tags$div(class = "llw-flow-axis-note", by_id("plot_axis_label"))
            )
          )
        )
      )
    )
  )
}

# UI navigation only. Operation selection goes through the same acknowledged
# form transaction; saves, previews and commits remain in prepare_server().
preparation_flow_navigation <- function(
  input,
  output,
  session,
  context,
  form_type,
  editing_id,
  loading_form,
  dirty,
  new_form_defaults,
  set_form
) {
  view <- shiny::reactiveVal("result")
  go <- function(to) {
    view(to)
    bslib::nav_select("flow_view", selected = to, session = session)
    session$onFlushed(
      function()
        session$sendCustomMessage(
          "llw-prepare-flow-navigate",
          list(
            root = session$ns("flow_root"),
            target = session$ns(paste0("flow_heading_", to))
          )
        ),
      once = TRUE
    )
  }
  start <- function() {
    shiny::req(context(), is.null(loading_form()))
    set_form(new_form_defaults())
    go("choose")
  }
  shiny::observeEvent(input$flow_add, {
    if (dirty()) {
      shiny::showModal(shiny::modalDialog(
        title = "Start a new step?",
        "Your current step has unsaved changes. Save them first, or discard them to start another step.",
        footer = shiny::tagList(
          shiny::modalButton("Keep editing"),
          shiny::actionButton(
            session$ns("flow_discard"),
            "Discard and start new step",
            class = "btn-danger"
          )
        )
      ))
    } else start()
  })
  shiny::observeEvent(input$flow_discard, {
    shiny::removeModal()
    start()
  })
  shiny::observeEvent(input$flow_change, go("choose"))
  shiny::observeEvent(input$flow_result, go("result"))
  shiny::observeEvent(input$flow_expand, go("result"))
  shiny::observeEvent(input$flow_back, go("edit"))
  shiny::observeEvent(input$flow_resume, go("edit"))
  for (category in names(preparation_flow_catalog()))
    for (operation in names(preparation_flow_catalog()[[category]]$operations))
      local({
        key <- operation
        group <- category
        shiny::observeEvent(input[[paste0("flow_pick_", key)]], {
          shiny::req(context(), is.null(loading_form()))
          # Selection itself is browsing, just like the original category tabs.
          # Existing parameter edits remain dirty; no form baseline is replaced.
          bslib::nav_select(
            "preparation_category",
            selected = group,
            session = session
          )
          control <- switch(
            group,
            filter = "step_type",
            values = "value_type",
            sampling = "sampling_type"
          )
          shiny::updateRadioButtons(
            session,
            control,
            selected = if (key == "dates") "time" else key
          )
          go("edit")
        })
      })
  output$flow_operation <- shiny::renderText({
    type <- form_type()
    key <- if (type %in% c("datetimes", "clock")) "dates" else if (
      type == "transform"
    )
      "calibrate" else type
    for (group in preparation_flow_catalog())
      if (!is.null(group$operations[[key]]))
        return(group$operations[[key]][[1L]])
    "Configure step"
  })
  output$flow_dirty <- shiny::renderText(if (dirty()) "yes" else "no")
  output$flow_editing <- shiny::renderText(
    if (is.null(editing_id())) "no" else "yes"
  )
  for (name in c("flow_dirty", "flow_editing"))
    shiny::outputOptions(output, name, suspendWhenHidden = FALSE)
  shiny::observe({
    shiny::updateActionButton(
      session,
      "flow_add",
      disabled = is.null(context()) || !is.null(loading_form())
    )
    shiny::updateActionButton(
      session,
      "save_new",
      label = if (is.null(editing_id())) "Add step and preview" else
        "Save as another step",
      disabled = is.null(context()) || !is.null(loading_form())
    )
    shiny::updateActionButton(
      session,
      "new_step",
      label = "Discard changes",
      disabled = !dirty()
    )
  })
  list(
    view = shiny::reactive(view()),
    # Saving in the editor updates the adjacent preview without moving focus
    # or scroll. A save requested from Review keeps the full-width result.
    saved = function() invisible(NULL),
    edit = function() go("edit"),
    cleared = function() go("result")
  )
}
