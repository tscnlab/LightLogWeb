# Session-scoped overlay editor. The parent owns the dataset and supplies the
# sole commit/undo callback; drafts never replace it.
# Place these controls inside the source-versus-overlay summary, not a second card.
metadata_overlay_ui <- function(id, contextual = FALSE) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    metadata_navigation_dependency(),
    class = "llw-overlay-controls",
    shiny::tags$div(
      class = "llw-overlay-actions",
      if (!contextual)
        bslib::input_task_button(
          ns("edit"),
          "Edit metadata",
          label_busy = "Opening editor...",
          class = "llw-overlay-edit"
        ),
      shiny::uiOutput(ns("undo_action"), inline = TRUE)
    ),
    shiny::uiOutput(ns("notice"))
  )
}

# A stable opener keeps keyboard focus intact when the source detail refreshes.
metadata_overlay_context_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "llw-overlay-context-actions",
    shiny::actionButton(
      ns("edit"),
      "Edit selected field",
      disabled = TRUE,
      title = "Edit the selected supported metadata field. Source IDs and relationships remain read-only.",
      class = "llw-overlay-edit btn-primary"
    ),
    shiny::actionButton(
      ns("add_missing"),
      "Add missing metadata",
      disabled = TRUE,
      title = "Add supported missing metadata anywhere in this library dataset, including appended sources. This does not depend on the selected field.",
      class = "llw-overlay-edit btn-outline-secondary"
    )
  )
}

metadata_overlay_field_choices <- function(catalog) {
  rows <- catalog[!duplicated(paste(catalog$table, catalog$field)), ]
  rows <- rows[rows$field != "longitude", ]
  labels <- rows$field_label
  labels[rows$field == "latitude"] <- "Coordinates (latitude and longitude)"
  stats::setNames(paste(rows$table, rows$field, sep = "/"), labels)
}

metadata_overlay_editor_targets <- function(context, field) {
  catalog <- context$catalog
  catalog[paste(catalog$table, catalog$field, sep = "/") == field, ]
}

metadata_overlay_item_choices <- function(rows, scope, source_scopes) {
  labels <- rows$entity_id
  if (length(source_scopes) > 1L) {
    labels <- paste(labels, "| Source", match(rows$source_scope, source_scopes))
  }
  if (identical(scope, "single")) {
    return(stats::setNames(rows$target_id, labels))
  }
  kind <- switch(
    rows$table[[1L]],
    participants = "participants",
    devices = "devices",
    variables = "variable occurrences",
    "datasets"
  )
  # Keep a wide inventory usable without silently reducing the batch. Every
  # exact target remains available in the paginated current-values table.
  shown <- utils::head(labels, 20L)
  if (length(labels) > length(shown)) {
    shown <- c(
      shown,
      paste(
        length(labels) - length(shown),
        "more (see all affected items below)"
      )
    )
  }
  stats::setNames(
    "all",
    paste0(
      "All ",
      length(labels),
      " ",
      kind,
      ": ",
      paste(shown, collapse = "; ")
    )
  )
}

metadata_overlay_current_table <- function(
  overlay,
  targets,
  source_scopes = unique(targets$source_scope)
) {
  current <- metadata_overlay_values(overlay, targets)
  text <- function(values) {
    result <- vapply(values, metadata_scalar_text, character(1))
    missing <- is.na(result)
    result[missing] <- ifelse(
      targets$field[missing] == "unit",
      "Unit not specified",
      "Not specified"
    )
    empty <- vapply(
      values,
      function(value) {
        is.character(value) &&
          length(value) == 1L &&
          !is.na(value) &&
          !nzchar(value)
      },
      logical(1)
    )
    result[empty] <- "Empty text"
    result
  }
  dplyr::tibble(
    Item = targets$entity_id,
    Field = targets$field_label,
    Current = text(current$values),
    Source_value = text(targets$source_value),
    Value_from = ifelse(
      current$overridden,
      "Committed overlay",
      "Source metadata"
    ),
    Source = paste0(
      "Source ",
      match(targets$source_scope, source_scopes),
      ": ",
      targets$source_label
    ),
    Source_scope = targets$source_scope,
    Target_id = targets$target_id
  )
}

metadata_overlay_value_summary <- function(values) {
  distinct <- unique(values)
  counts <- tabulate(match(values, distinct), nbins = length(distinct))
  shown <- seq_len(min(5L, length(distinct)))
  labels <- paste0(distinct[shown], " (", counts[shown], ")")
  if (length(distinct) > length(shown)) {
    labels <- c(
      labels,
      paste(
        length(distinct) - length(shown),
        "other values; see all affected items"
      )
    )
  }
  paste(labels, collapse = "; ")
}

# Returns status and preview reactives. Errors retain the draft and committed
# record. apply_callback(context, preview, undo) must be atomic and return the
# resulting validated dataset record, as new_session_store()$apply_overlay does.
metadata_overlay_server <- function(
  id,
  dataset,
  apply_callback,
  selection = NULL
) {
  if (!shiny::is.reactive(dataset) || !is.function(apply_callback)) {
    abort_llw(
      "The overlay editor requires a dataset reactive and an apply callback.",
      type = "validation"
    )
  }
  if (!is.null(selection) && !shiny::is.reactive(selection)) {
    abort_llw(
      "`selection` must be a reactive explorer selection.",
      type = "validation"
    )
  }
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    editing <- shiny::reactiveVal(NULL)
    focus_draft <- shiny::reactiveVal(NULL)
    focus_error <- shiny::reactiveVal(NULL)
    preview <- shiny::reactiveVal(NULL)
    notice <- shiny::reactiveVal(NULL)
    draft_error <- shiny::reactiveVal(NULL)
    last_action <- shiny::reactiveVal("Ready")
    confirm_close <- shiny::reactiveVal(FALSE)
    cache <- new.env(parent = emptyenv())
    get_context <- function(record) {
      if (is.null(record))
        abort_llw("Select a library dataset first.", type = "validation")
      prior <- cache[[record$id]]
      if (
        is.null(prior) ||
          !identical(
            prior$source_fingerprint,
            record$metadata_bundle$source_fingerprint
          )
      ) {
        cache[[record$id]] <- shiny::withProgress(
          message = "Preparing editable metadata",
          detail = "Building the local target list. Source records and measurements stay unchanged.",
          value = 0,
          metadata_overlay_context(record)
        )
      }
      cache[[record$id]]
    }
    open_focus <- function(dataset_id, variable) {
      if (!is.null(editing()) || !is.null(focus_draft()))
        return(invisible(NULL))
      tryCatch(
        {
          record <- dataset()
          if (is.null(record) || !identical(record$id, dataset_id))
            abort_llw(
              "The selected dataset changed. Open the editor again from its Dashboard.",
              type = "validation"
            )
          if (!is.null(record$draft))
            abort_llw(
              "Finish or discard the dataset preparation draft before editing metadata.",
              type = "validation"
            )
          context <- get_context(record)
          state <- metadata_overlay_focus_state(record, context, variable)
          focus_draft(list(
            record = record,
            context = context,
            variable = variable,
            state = state
          ))
          focus_error(NULL)
          field_input <- function(field, label) {
            value <- state$fields[[field]]
            shiny::tagList(
              shiny::textInput(
                ns(paste0("focus_", field)),
                label,
                value$value,
                updateOn = "blur"
              ),
              if (value$mixed)
                shiny::tags$p(
                  "Multiple current values. Leave empty to keep them unchanged."
                ),
              if (value$incomplete)
                shiny::tags$p(
                  "Some occurrences have no value. Leave empty to preserve this coverage."
                ),
              shiny::tags$p(
                class = "llw-secondary small",
                paste(
                  value$present,
                  "specified;",
                  value$missing,
                  "not specified."
                )
              )
            )
          }
          count <- sum(state$targets$field == "label")
          dialog <- shiny::modalDialog(
            title = "Focus variable metadata",
            size = "m",
            easyClose = FALSE,
            fade = FALSE,
            shiny::tags$div(
              id = ns("focus_draft"),
              class = "llw-overlay-draft",
              `data-escape-input` = ns("focus_escape"),
              shiny::tags$p(
                shiny::tags$strong(record$display_name),
                " | ",
                shiny::tags$code(variable)
              ),
              shiny::tags$p(paste(
                "Applies to all",
                count,
                "exact variable occurrences across",
                length(unique(state$targets$source_scope)),
                "retained source scope(s) in this library dataset. Dashboard filters do not reduce this scope."
              )),
              shiny::tags$div(
                class = "llw-overlay-value-input",
                field_input("label", "Label"),
                field_input("unit", "Unit")
              ),
              shiny::tags$p(
                class = "llw-secondary small",
                "Clearing a prefilled field removes its overlay override and uses the source value, or leaves it unspecified if absent. An empty mixed field is unchanged. Units are declarations only: measurements are never converted. Source metadata remain unchanged."
              ),
              shiny::tags$details(
                shiny::tags$summary("Exact affected items and current values"),
                DT::DTOutput(ns("focus_values"))
              ),
              shiny::uiOutput(ns("focus_error"))
            ),
            footer = shiny::tagList(
              shiny::actionButton(
                ns("focus_discard"),
                "Discard",
                class = "btn-outline-secondary"
              ),
              bslib::input_task_button(
                ns("focus_save"),
                "Save",
                label_busy = "Saving metadata..."
              )
            )
          )
          dialog <- htmltools::tagQuery(dialog)$find(".modal-dialog")$addClass(
            "modal-dialog-scrollable"
          )$allTags()
          shiny::showModal(dialog)
          last_action("Editing focus variable")
        },
        error = function(cnd) {
          shiny::showNotification(
            conditionMessage(cnd),
            type = "error",
            duration = 10
          )
          last_action("Not applied")
        }
      )
      invisible(NULL)
    }
    output$focus_values <- DT::renderDT(
      {
        draft <- focus_draft()
        shiny::req(draft)
        DT::datatable(
          metadata_overlay_current_table(
            draft$state$overlay,
            draft$state$targets
          ),
          rownames = FALSE,
          escape = TRUE,
          selection = "none",
          options = list(pageLength = 5L, scrollX = TRUE, order = list())
        )
      },
      server = TRUE
    )
    output$focus_error <- shiny::renderUI(message_ui(
      focus_error(),
      error = TRUE
    ))
    discard_focus <- function() {
      if (is.null(focus_draft())) return(invisible(NULL))
      focus_draft(NULL)
      focus_error(NULL)
      shiny::removeModal()
      last_action("Draft discarded")
    }
    shiny::observeEvent(input$focus_discard, discard_focus(), ignoreInit = TRUE)
    shiny::observeEvent(input$focus_escape, discard_focus(), ignoreInit = TRUE)
    shiny::observeEvent(
      input$focus_save,
      {
        draft <- focus_draft()
        if (is.null(draft)) return(invisible(NULL))
        tryCatch(
          {
            current <- dataset()
            if (
              is.null(current) ||
                !identical(current$id, draft$record$id) ||
                !identical(current$revision, draft$record$revision)
            ) {
              abort_llw(
                "This draft is stale or the dataset changed. Discard and reopen from the Dashboard to use current values. Nothing has been saved.",
                type = "validation"
              )
            }
            patch <- metadata_overlay_focus_preview(
              current,
              draft$context,
              draft$variable,
              input$focus_label,
              input$focus_unit
            )
            result <- apply_callback(draft$context, patch, undo = FALSE)
            focus_draft(NULL)
            focus_error(NULL)
            shiny::removeModal()
            notice(paste(
              "Focus metadata saved. Overlay revision",
              result$metadata_bundle$overlay$revision,
              ". Source records and measurements are unchanged."
            ))
            shiny::showNotification(
              if (any(patch$changed)) "Focus variable metadata saved." else
                "No metadata values changed.",
              type = "message"
            )
            last_action(
              if (any(patch$changed)) "Applied" else "No values changed"
            )
          },
          error = function(cnd) {
            focus_error(conditionMessage(cnd))
            last_action("Not applied")
          }
        )
      },
      ignoreInit = TRUE
    )
    missing_targets <- shiny::reactive({
      if (is.null(dataset())) return(character())
      metadata_overlay_missing_targets(get_context(dataset()))
    })
    presentation <- shiny::reactive({
      record <- dataset()
      if (is.null(record)) return(NULL)
      metadata_overlay_presentation(
        get_context(record),
        record$metadata_bundle$overlay
      )
    })
    selected_field <- shiny::reactive({
      if (is.null(selection)) return(NULL)
      selected <- selection()
      if (is.null(dataset()) || is.null(selected$group)) {
        return(list(
          editable = FALSE,
          reason = if (length(missing_targets()))
            "Choose a metadata field in the explorer, or use Add missing metadata for supported fields with no retained source field." else
            "Choose a metadata field in the explorer. No additional missing metadata targets are available."
        ))
      }
      metadata_overlay_explorer_selection(
        get_context(dataset()),
        selected,
        dataset()$metadata_bundle$overlay
      )
    })
    shiny::observe({
      if (is.null(selection)) return(invisible(NULL))
      shiny::updateActionButton(
        session,
        "edit",
        disabled = !isTRUE(selected_field()$editable)
      )
      shiny::updateActionButton(
        session,
        "add_missing",
        disabled = length(missing_targets()) == 0L
      )
    })
    draft_field <- shiny::reactive({
      if (!is.null(editing()$selection)) editing()$selection$field else
        input$field
    })
    draft_matching_id <- shiny::reactive({
      if (!is.null(editing()$selection)) editing()$selection$matching_id else
        input$matching_id
    })
    message_ui <- function(value, error = FALSE) {
      if (is.null(value)) return(NULL)
      shiny::tags$div(
        class = if (error) "llw-status-banner llw-status-banner--warning" else
          "llw-status-banner",
        role = if (error) "alert" else "status",
        value
      )
    }
    fail <- function(cnd, draft = FALSE) {
      text <- conditionMessage(cnd)
      if (draft) draft_error(text) else notice(text)
      last_action("Not applied")
      invisible(NULL)
    }
    draft_current <- function() {
      draft <- editing()
      if (is.null(draft))
        abort_llw("Open the metadata editor first.", type = "validation")
      current <- dataset()
      if (is.null(current) || !identical(current$id, draft$record$id)) {
        abort_llw(
          "The selected dataset changed. Return to the draft dataset or discard this draft.",
          type = "validation"
        )
      }
      metadata_overlay_assert_context(current, draft$context)
      if (!identical(current$revision, draft$record$revision)) {
        abort_llw(
          "The draft is stale. Use Refresh current values, then review the changes again. Your inputs are retained.",
          type = "validation"
        )
      }
      current
    }
    draft_signature <- function() {
      list(
        field = draft_field(),
        matching_id = draft_matching_id(),
        target_id = input$target_id,
        scope = input$scope,
        operation = input$operation,
        text_value = input$text_value,
        latitude = input$latitude,
        longitude = input$longitude,
        resolve_legacy = isTRUE(input$resolve_legacy)
      )
    }
    output$notice <- shiny::renderUI(message_ui(notice()))
    output$undo_action <- shiny::renderUI({
      record <- dataset()
      history <- record$history %||% list()
      if (
        length(history) == 0L ||
          !identical(attr(history[[1L]], "llw_change_type"), "metadata_overlay")
      )
        return(NULL)
      bslib::input_task_button(
        ns("undo"),
        "Undo latest metadata edit",
        type = "secondary",
        label_busy = "Undoing metadata edit..."
      )
    })
    last_record_id <- NULL
    shiny::observeEvent(
      dataset(),
      {
        id <- dataset()$id %||% NULL
        if (!identical(id, last_record_id)) {
          notice(NULL)
          last_record_id <<- id
        }
      },
      ignoreInit = FALSE
    )

    open_editor <- function(missing_only = FALSE) {
      if (!is.null(editing()) || !is.null(focus_draft()))
        return(invisible(NULL))
      tryCatch(
        {
          record <- dataset()
          context <- get_context(record)
          selected <- if (missing_only) NULL else selected_field()
          eligible <- if (missing_only) missing_targets() else NULL
          if (
            !missing_only && !is.null(selection) && !isTRUE(selected$editable)
          ) {
            abort_llw(selected$reason, type = "validation")
          }
          if (
            nrow(context$catalog) == 0L ||
              (missing_only && length(eligible) == 0L)
          ) {
            abort_llw(
              "No unambiguous editable identities are retained for this dataset. Source metadata and the data remain available.",
              type = "validation"
            )
          }
          if (!is.null(record$draft)) {
            abort_llw(
              "Finish or discard the dataset preparation draft before editing metadata.",
              type = "validation"
            )
          }
          editing(list(
            record = record,
            context = context,
            selection = selected,
            eligible_ids = eligible
          ))
          preview(NULL)
          draft_error(NULL)
          confirm_close(FALSE)
          available <- if (missing_only)
            context$catalog[context$catalog$target_id %in% eligible, ] else
            context$catalog
          fields <- metadata_overlay_field_choices(available)
          initial <- if ("participants/sex" %in% fields) "participants/sex" else
            unname(fields[[1L]])
          dialog <- shiny::modalDialog(
            title = if (missing_only) "Add missing metadata" else
              "Edit LightLogWeb overlay",
            size = "xl",
            easyClose = FALSE,
            fade = FALSE,
            shiny::tags$div(
              id = ns("draft"),
              class = "llw-overlay-draft",
              `data-escape-input` = ns("escape"),
              shiny::tags$p(shiny::tags$strong(record$display_name)),
              shiny::tags$p(
                if (is.null(selected))
                  "Choose a field and scope, enter a value, then review every affected item before Apply. Escape or Discard closes without applying." else
                  "Choose the scope, enter a value, then review every affected item before Apply. Escape or Discard closes without applying."
              ),
              if (missing_only)
                shiny::tags$p(
                  class = "llw-status-banner",
                  "Only approved fields without a selectable source field are offered. Targets use existing imported identities. All matching items means the eligible missing metadata in this library dataset, including appended sources. No source entities or relationships are created."
                ),
              if (!is.null(selected))
                shiny::tags$div(
                  class = "llw-overlay-fixed-field",
                  shiny::tags$strong(paste(
                    selected$label,
                    selected$source_name
                  )),
                  shiny::tags$p(
                    shiny::tags$code(selected$source_field),
                    " | Selected in the metadata explorer. This changes the LightLogWeb overlay only."
                  )
                ),
              if (is.null(selected))
                shiny::selectInput(
                  ns("field"),
                  "Metadata field",
                  fields,
                  selected = initial,
                  selectize = FALSE
                ),
              if (is.null(selected))
                shiny::conditionalPanel(
                  "input.field === 'variables/label' || input.field === 'variables/unit'",
                  shiny::selectizeInput(
                    ns("matching_id"),
                    "Source variable",
                    choices = NULL,
                    options = list(maxOptions = 100L)
                  ),
                  ns = ns
                ),
              shiny::radioButtons(
                ns("scope"),
                "Edit scope",
                c(
                  "All matching items in this library dataset (default)" = "all",
                  "Single item only" = "single"
                ),
                selected = "all"
              ),
              shiny::selectizeInput(
                ns("target_id"),
                "Apply to",
                choices = NULL,
                options = list(maxOptions = 100L)
              ),
              shiny::tags$section(
                class = "llw-overlay-current-values",
                `aria-label` = "Current values",
                shiny::uiOutput(ns("current_values")),
                shiny::tags$details(
                  shiny::tags$summary(
                    "All affected items and current values"
                  ),
                  DT::DTOutput(ns("current_values_table"))
                )
              ),
              shiny::radioButtons(
                ns("operation"),
                "Change",
                c(
                  "Set overlay value" = "set",
                  "Remove override and use source value" = "remove"
                ),
                selected = "set"
              ),
              shiny::conditionalPanel(
                "input.operation === 'set'",
                shiny::uiOutput(
                  ns("value_input"),
                  class = "llw-overlay-value-input"
                ),
                ns = ns
              ),
              shiny::uiOutput(ns("field_help")),
              shiny::uiOutput(ns("legacy_warning")),
              shiny::uiOutput(ns("draft_error")),
              shiny::tags$div(
                class = "llw-overlay-actions",
                bslib::input_task_button(
                  ns("review"),
                  "Review changes",
                  label_busy = "Checking all targets..."
                ),
                bslib::input_task_button(
                  ns("refresh"),
                  "Refresh current values",
                  type = "secondary",
                  label_busy = "Refreshing current values..."
                )
              ),
              shiny::uiOutput(ns("preview_summary")),
              DT::DTOutput(ns("preview_table")),
              shiny::uiOutput(ns("apply_action"))
            ),
            footer = shiny::uiOutput(ns("close_actions"))
          )
          dialog <- htmltools::tagQuery(dialog)$find(".modal-dialog")$addClass(
            "modal-dialog-scrollable"
          )$allTags()
          shiny::showModal(dialog)
          last_action("Editing draft")
        },
        error = fail
      )
    }
    shiny::observeEvent(input$edit, open_editor(), ignoreInit = TRUE)
    shiny::observeEvent(
      input$add_missing,
      open_editor(missing_only = TRUE),
      ignoreInit = TRUE
    )

    field_rows <- shiny::reactive({
      shiny::req(editing(), draft_field())
      rows <- metadata_overlay_editor_targets(
        editing()$context,
        draft_field()
      )
      eligible <- editing()$eligible_ids
      if (is.null(eligible)) rows else rows[rows$target_id %in% eligible, ]
    })
    matching_rows <- shiny::reactive({
      rows <- field_rows()
      rows[!duplicated(rows$cohort), ]
    })
    shiny::observeEvent(matching_rows(), {
      if (!is.null(editing()$selection)) return(invisible(NULL))
      rows <- matching_rows()
      shiny::req(nrow(rows) > 0L)
      labels <- ifelse(
        rows$table == "variables",
        rows$name,
        rows$field_label
      )
      selected <- input$matching_id
      if (length(selected) != 1L || !selected %in% rows$target_id)
        selected <- rows$target_id[[1L]]
      shiny::updateSelectizeInput(
        session,
        "matching_id",
        choices = stats::setNames(rows$target_id, labels),
        selected = selected,
        server = TRUE
      )
    })
    cohort_rows <- shiny::reactive({
      shiny::req(draft_matching_id() %in% matching_rows()$target_id)
      rows <- field_rows()
      cohort <- rows$cohort[match(draft_matching_id(), rows$target_id)]
      rows[rows$cohort == cohort, ]
    })
    item_choices <- shiny::reactive({
      shiny::req(input$scope %in% c("all", "single"))
      metadata_overlay_item_choices(
        cohort_rows(),
        input$scope,
        unique(editing()$context$catalog$source_scope)
      )
    })
    shiny::observeEvent(item_choices(), {
      choices <- item_choices()
      selected <- input$target_id
      if (length(selected) != 1L || !selected %in% choices) {
        preferred <- editing()$selection$target_id
        selected <- if (length(preferred) == 1L && preferred %in% choices)
          preferred else unname(choices[[1L]])
      }
      shiny::updateSelectizeInput(
        session,
        "target_id",
        choices = choices,
        selected = selected,
        server = TRUE
      )
    })
    selected_anchor <- shiny::reactive({
      shiny::req(input$target_id %in% item_choices())
      if (identical(input$scope, "all")) draft_matching_id() else
        input$target_id
    })
    targets <- shiny::reactive({
      metadata_overlay_target_scope(
        editing()$context,
        selected_anchor(),
        input$scope,
        eligible_ids = editing()$eligible_ids
      )
    })
    draft_overlay <- shiny::reactive({
      draft <- editing()
      shiny::req(draft)
      metadata_overlay_upgrade(
        draft$record$metadata_bundle$overlay,
        draft$context$catalog
      )
    })
    current_values <- shiny::reactive({
      metadata_overlay_current_table(
        draft_overlay(),
        targets(),
        unique(editing()$context$catalog$source_scope)
      )
    })
    output$current_values <- shiny::renderUI({
      rows <- targets()
      values <- current_values()
      count <- length(unique(paste(rows$source_scope, rows$entity_id)))
      shiny::tagList(
        shiny::tags$h4("Current values"),
        shiny::tags$div(
          role = "status",
          lapply(unique(values$Field), function(field) {
            selected <- values[values$Field == field, ]
            shiny::tags$div(
              class = "llw-overlay-current-field",
              shiny::tags$strong(field),
              shiny::tags$p(
                "Current: ",
                metadata_overlay_value_summary(selected$Current)
              ),
              shiny::tags$p(
                "Retained source: ",
                metadata_overlay_value_summary(selected$Source_value)
              )
            )
          }),
          shiny::tags$p(paste(
            count,
            "item(s) across",
            length(unique(rows$source_scope)),
            "source scope(s).",
            sum(values$Value_from == "Committed overlay"),
            "committed field override(s)."
          ))
        ),
        shiny::tags$p(
          class = "text-muted",
          paste(
            "Committed overlay revision",
            draft_overlay()$revision,
            ". Unapplied draft values are not shown. Counts in parentheses are field occurrences."
          )
        )
      )
    })
    output$current_values_table <- DT::renderDT(
      {
        table <- current_values()
        names(table) <- gsub("_", " ", names(table), fixed = TRUE)
        DT::datatable(
          table,
          rownames = FALSE,
          escape = TRUE,
          selection = "none",
          options = list(
            pageLength = 5L,
            lengthMenu = c(5L, 10L, 25L, 100L),
            scrollX = TRUE,
            order = list()
          )
        )
      },
      server = TRUE
    )
    output$value_input <- shiny::renderUI({
      shiny::req(draft_field())
      if (identical(draft_field(), "sites/latitude")) {
        bslib::layout_columns(
          shiny::numericInput(
            ns("latitude"),
            "Latitude",
            value = NA_real_,
            min = -90,
            max = 90,
            updateOn = "blur"
          ),
          shiny::numericInput(
            ns("longitude"),
            "Longitude",
            value = NA_real_,
            min = -180,
            max = 180,
            updateOn = "blur"
          )
        )
      } else
        shiny::textInput(
          ns("text_value"),
          "New overlay value",
          value = "",
          updateOn = "blur"
        )
    })
    output$field_help <- shiny::renderUI({
      text <- switch(
        draft_field() %||% "",
        "variables/unit" = "Use a UCUM code or precise custom unit. This changes only the declared unit; it never converts measurements. Similar labels do not make different source variables equivalent.",
        "participants/sex" = "Participant sex is free text in the retained schema. Use the exact value you intend to assert. Sex and gender are separate fields; no recoding is performed.",
        "sites/source_timezone" = "Use an IANA timezone, for example Europe/Berlin. This annotation never reinterprets timestamps or changes import, analysis or display timezone settings.",
        "datasets/title" = "This is a factual dataset title. It does not rename the library entry.",
        "Values are user-supplied LightLogWeb metadata. IDs, relationships and source facts remain unchanged."
      )
      shiny::tags$p(class = "text-muted", text)
    })
    output$legacy_warning <- shiny::renderUI({
      rows <- targets()
      overlay <- draft_overlay()
      values <- unique(unlist(metadata_overlay_legacy_conflicts(overlay, rows)))
      if (length(values) == 0L) return(NULL)
      shiny::tagList(
        message_ui(
          paste(
            "Older values without a unique source:",
            paste(values, collapse = "; "),
            ". They are not assigned automatically. Explicit confirmation below is required."
          ),
          error = TRUE
        ),
        shiny::checkboxInput(
          ns("resolve_legacy"),
          "Explicitly replace the older assignment for these targets. Keep the older values in the retained overlay record.",
          FALSE
        )
      )
    })
    output$draft_error <- shiny::renderUI(message_ui(
      draft_error(),
      error = TRUE
    ))
    shiny::observeEvent(
      draft_signature(),
      {
        current <- preview()
        if (
          !is.null(current) && !identical(current$signature, draft_signature())
        )
          preview(NULL)
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$review,
      {
        preview(NULL)
        draft_error(NULL)
        tryCatch(
          {
            record <- draft_current()
            if (
              !isTRUE(input$scope %in% c("all", "single")) ||
                !isTRUE(draft_matching_id() %in% matching_rows()$target_id) ||
                !isTRUE(input$target_id %in% item_choices())
            ) {
              abort_llw(
                "Choose a metadata field and an available item before reviewing changes.",
                type = "validation"
              )
            }
            anchor <- selected_anchor()
            value <- if (identical(draft_field(), "sites/latitude")) {
              c(latitude = input$latitude, longitude = input$longitude)
            } else input$text_value
            patch <- metadata_overlay_preview(
              record,
              editing()$context,
              anchor,
              value,
              input$scope,
              input$operation,
              isTRUE(input$resolve_legacy),
              eligible_ids = editing()$eligible_ids
            )
            preview(list(patch = patch, signature = draft_signature()))
            last_action("Preview ready")
          },
          error = function(cnd) fail(cnd, draft = TRUE),
          finally = dialog_focus("review")
        )
      },
      ignoreInit = TRUE
    )
    output$preview_summary <- shiny::renderUI({
      current <- preview()
      if (is.null(current)) return(NULL)
      patch <- current$patch
      shiny::tagList(
        shiny::tags$h3("Review every affected item"),
        shiny::tags$p(
          role = "status",
          paste(
            sum(patch$changed),
            "field value(s) will change out of",
            nrow(patch$targets),
            "reviewed. Table search and pagination do not reduce this scope."
          )
        ),
        shiny::tags$p(
          "Original means retained source value. Current includes committed overlays. Proposed is the result after Apply. Exact source scopes and target IDs are available by scrolling the table horizontally."
        )
      )
    })
    output$preview_table <- DT::renderDT(
      {
        current <- preview()
        if (is.null(current)) return(NULL)
        table <- metadata_overlay_preview_table(current$patch)
        if (!any(nzchar(table$Older_unqualified_value)))
          table$Older_unqualified_value <- NULL
        names(table) <- gsub("_", " ", names(table), fixed = TRUE)
        DT::datatable(
          table,
          rownames = FALSE,
          escape = TRUE,
          selection = "none",
          options = list(
            pageLength = 5L,
            lengthMenu = c(5L, 10L, 25L, 100L),
            scrollX = TRUE,
            autoWidth = TRUE,
            order = list(),
            search = list(regex = FALSE)
          )
        )
      },
      server = TRUE
    )
    output$apply_action <- shiny::renderUI({
      current <- preview()
      if (is.null(current)) return(NULL)
      count <- sum(current$patch$changed)
      bslib::input_task_button(
        ns("apply"),
        if (count > 0L) paste("Apply", count, "field change(s)") else
          "Confirm unchanged values",
        label_busy = "Applying the reviewed changes..."
      )
    })
    shiny::observeEvent(
      input$apply,
      {
        tryCatch(
          {
            draft_current()
            current <- preview()
            if (
              is.null(current) ||
                !identical(current$signature, draft_signature())
            ) {
              abort_llw(
                "The inputs changed. Review changes again before applying.",
                type = "validation"
              )
            }
            result <- apply_callback(
              editing()$context,
              current$patch,
              undo = FALSE
            )
            count <- sum(current$patch$changed)
            preview(NULL)
            editing(NULL)
            confirm_close(FALSE)
            shiny::removeModal()
            notice(paste(
              count,
              "field value(s) changed. Committed overlay revision",
              result$metadata_bundle$overlay$revision,
              ". Source records and measurements are unchanged."
            ))
            last_action(if (count > 0L) "Applied" else "No values changed")
          },
          error = function(cnd) fail(cnd, draft = TRUE)
        )
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$refresh,
      {
        tryCatch(
          {
            draft <- editing()
            shiny::req(draft)
            record <- dataset()
            if (!identical(record$id, draft$record$id))
              abort_llw(
                "Return to the draft dataset before refreshing, or discard the draft.",
                type = "validation"
              )
            metadata_overlay_assert_context(record, draft$context)
            draft$record <- record
            editing(draft)
            preview(NULL)
            draft_error(
              "Current values refreshed. Your inputs are retained. Review changes again before applying."
            )
          },
          error = function(cnd) fail(cnd, draft = TRUE)
        )
      },
      ignoreInit = TRUE
    )
    discard_draft <- function() {
      if (is.null(editing())) return(invisible(NULL))
      editing(NULL)
      preview(NULL)
      draft_error(NULL)
      confirm_close(FALSE)
      shiny::removeModal()
      notice("Draft discarded. The committed overlay is unchanged.")
      last_action("Draft discarded")
    }
    dialog_focus <- function(mode) {
      session$onFlushed(
        function()
          session$sendCustomMessage(
            "llw-overlay-dialog-focus",
            list(
              target = ns("draft"),
              keep = ns("keep_editing"),
              review = ns("review"),
              mode = mode
            )
          ),
        once = TRUE
      )
    }
    output$close_actions <- shiny::renderUI({
      if (!confirm_close())
        return(shiny::actionButton(
          ns("discard"),
          "Discard draft",
          class = "btn-outline-secondary"
        ))
      shiny::tags$div(
        class = "llw-overlay-close-confirmation",
        role = "group",
        `aria-label` = "Discard unsaved changes?",
        shiny::tags$p(
          role = "alert",
          "Discard unsaved changes? Nothing has been applied."
        ),
        shiny::actionButton(
          ns("keep_editing"),
          "Keep editing",
          class = "btn-primary"
        ),
        shiny::actionButton(
          ns("confirm_discard"),
          "Discard changes",
          class = "btn-outline-secondary"
        )
      )
    })
    shiny::observeEvent(
      input$escape,
      {
        if (
          is.null(editing()) ||
            !is.list(input$escape) ||
            !is.logical(input$escape$dirty) ||
            length(input$escape$dirty) != 1L ||
            is.na(input$escape$dirty)
        )
          return(invisible(NULL))
        if (confirm_close()) {
          confirm_close(FALSE)
          dialog_focus("resume")
        } else if (isTRUE(input$escape$dirty) || !is.null(preview())) {
          confirm_close(TRUE)
          dialog_focus("confirm")
        } else discard_draft()
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$keep_editing,
      {
        confirm_close(FALSE)
        dialog_focus("resume")
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(input$discard, discard_draft(), ignoreInit = TRUE)
    shiny::observeEvent(
      input$confirm_discard,
      {
        if (confirm_close()) discard_draft()
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$undo,
      {
        tryCatch(
          {
            if (!is.null(editing()) || !is.null(focus_draft()))
              abort_llw(
                "Finish or discard the open draft before undoing.",
                type = "validation"
              )
            result <- apply_callback(get_context(dataset()), undo = TRUE)
            notice(paste(
              "Latest metadata edit undone. Committed overlay revision",
              result$metadata_bundle$overlay$revision,
              ". Other changes and source records are protected."
            ))
            last_action("Undo applied")
          },
          error = fail
        )
      },
      ignoreInit = TRUE
    )
    list(
      open_focus = open_focus,
      presentation = presentation,
      preview = shiny::reactive(
        if (is.null(preview())) NULL else preview()$patch
      ),
      status = shiny::reactive(list(
        editing = !is.null(editing()) || !is.null(focus_draft()),
        confirming_discard = confirm_close(),
        preview_ready = !is.null(preview()),
        last_action = last_action(),
        error = if (!is.null(focus_draft())) focus_error() else draft_error()
      ))
    )
  })
}

metadata_overlay_app <- function(
  records = metadata_explorer_showcase_records(),
  with_explorer = FALSE
) {
  if (!is.list(records) || length(records) == 0L)
    abort_llw(
      "The overlay showcase requires dataset records.",
      type = "validation"
    )
  records <- lapply(records, validate_dataset_record)
  names(records) <- vapply(records, `[[`, character(1), "id")
  ui <- lightlogweb_page(bslib::page_fluid(
    theme = lightlogweb_theme(),
    lightlogweb_head(),
    shiny::tags$main(
      class = "llw-main-shell",
      llw_view_header(
        "Milestone 7 Slice 3 showcase",
        if (with_explorer) "Explore and edit metadata" else "Overlay authoring",
        "Offline, disposable session. Edit factual metadata without changing source metadata or measurements."
      ),
      shiny::selectInput(
        "showcase_record",
        "Library dataset",
        stats::setNames(
          names(records),
          vapply(records, `[[`, character(1), "display_name")
        ),
        selectize = FALSE
      ),
      if (with_explorer)
        metadata_explorer_ui(
          "metadata",
          field_actions = metadata_overlay_context_ui("overlay")
        ) else shiny::uiOutput("overlay_summary"),
      shiny::tags$aside(
        class = "llw-showcase-status",
        `aria-label` = "Development status",
        shiny::tags$h2("Development status"),
        shiny::verbatimTextOutput("status")
      )
    )
  ))
  server <- function(input, output, session) {
    store <- new_session_store("local")
    for (record in records)
      store$dispatch(new_session_event("add", value = record))
    store$dispatch(new_session_event(
      "select",
      dataset_id = names(records)[[1L]]
    ))
    shiny::observeEvent(
      input$showcase_record,
      {
        if (input$showcase_record %in% names(records)) {
          store$select_dataset(input$showcase_record)
        }
      },
      ignoreInit = TRUE
    )
    explorer <- if (with_explorer)
      metadata_explorer_server(
        "metadata",
        store$selected_dataset,
        editable = TRUE,
        presentation = shiny::reactive(editor$presentation()),
        overlay_actions = metadata_overlay_ui("overlay", contextual = TRUE)
      ) else NULL
    if (!with_explorer) {
      output$overlay_summary <- shiny::renderUI({
        metadata_source_overlay_ui(
          store$selected_dataset()$metadata_bundle,
          editable = TRUE,
          overlay_actions = metadata_overlay_ui("overlay")
        )
      })
    }
    selection <- if (with_explorer)
      shiny::reactive(list(
        group = explorer$selected_group(),
        focus_path = explorer$hierarchy_context()$node$path
      )) else NULL
    editor <- metadata_overlay_server(
      "overlay",
      store$selected_dataset,
      store$apply_overlay,
      selection = selection
    )
    output$status <- shiny::renderPrint({
      record <- store$selected_dataset()
      list(
        editor = editor$status(),
        explorer = if (with_explorer) explorer$status() else NULL,
        library_dataset_id = record$id,
        overlay_revision = record$metadata_bundle$overlay$revision,
        source_fingerprint = record$metadata_bundle$source_fingerprint,
        unchanged_measurement_bytes = identical(
          record$raw_payload,
          records[[record$id]]$raw_payload
        )
      )
    })
  }
  shiny::shinyApp(ui, server)
}
