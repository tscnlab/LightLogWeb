test_that("the flow presentation retains every accepted operation control", {
  all_tags <- function(x) {
    if (inherits(x, "shiny.tag"))
      c(
        list(x),
        unlist(lapply(x$children, all_tags), recursive = FALSE)
      ) else if (is.list(x)) unlist(lapply(x, all_tags), recursive = FALSE) else
      list()
  }
  accepted <- all_tags(prepare_ui("prepare"))
  experiment <- all_tags(prepare_flow_ui("prepare"))
  ids <- function(tags)
    vapply(
      Filter(function(x) !is.null(x$attribs$id), tags),
      function(x) x$attribs$id,
      character(1)
    )
  outputs <- function(tags)
    Filter(
      function(tag)
        grepl("shiny-(html|text|plot)-output", tag$attribs$class %||% ""),
      tags
    )
  expect_equal(anyDuplicated(ids(experiment)), 0L)
  expect_true(all(ids(outputs(accepted)) %in% ids(outputs(experiment))))
  contract <- function(tags) {
    controls <- Filter(
      function(tag)
        tag$name %in%
          c("input", "select", "textarea") ||
          (tag$name == "button" && !is.null(tag$attribs$id)),
      tags
    )
    fields <- c(
      "id",
      "name",
      "type",
      "value",
      "min",
      "max",
      "step",
      "multiple",
      "disabled",
      "checked"
    )
    result <- lapply(controls, function(tag) {
      values <- setNames(
        lapply(fields, function(field) tag$attribs[[field]]),
        fields
      )
      values$options <- lapply(
        Filter(function(x) x$name == "option", all_tags(tag)),
        as.character
      )
      values
    })
    names(result) <- vapply(
      result,
      function(x) paste(x$id %||% "", x$name %||% "", x$value %||% ""),
      character(1)
    )
    result[order(names(result))]
  }
  before <- contract(accepted)
  after <- contract(experiment)
  expect_identical(before, after[names(before)])
})

test_that("the source and Add cards bound the sequence outside the sortable steps", {
  ns <- shiny::NS("prepare")
  page <- htmltools::tagQuery(prepare_flow_ui("prepare"))
  cards <- page$find(".llw-flow-sequence")$selectedTags()[[1L]]$children
  expect_length(cards, 3L)
  expect_length(htmltools::tagQuery(cards[[1L]])$find("button.llw-flow-source")$selectedTags(), 1L)
  expect_length(htmltools::tagQuery(cards[[2L]])$find("#prepare-recipe_choices")$selectedTags(), 1L)
  expect_identical(cards[[3L]]$attribs$id, ns("flow_add"))
  expect_length(page$find("#prepare-flow_add")$selectedTags(), 1L)
  expect_length(page$find(".llw-flow-source")$selectedTags(), 1L)
  ids <- c("step_first", "step_second")
  ui <- preparation_recipe_choices_ui(ns,
    c("First step", "Second step"), ids, ids[[1L]], flow = TRUE)
  query <- htmltools::tagQuery(ui)
  group <- query$find("#prepare-recipe_order")$selectedTags()[[1L]]
  # The package's onLoad/onSort handler reads every direct element child.
  captured <- vapply(group$children, function(tag)
    tag$attribs[["data-rank-id"]] %||% "", character(1))
  expect_identical(captured, ids)
  expect_length(query$find("button")$selectedTags(), 0L)
})

test_that("the flow saves the duration range and CV sequence and protects drafts", {
  record <- shiny::reactiveVal(m8_record(days = 7L))
  original <- shiny::isolate(record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0L),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record),
      active = shiny::reactive(TRUE),
      flow = TRUE
    ),
    {
      acknowledge <- function() {
        for (i in seq_len(12L)) {
          values <- loading_form()
          if (is.null(values)) return(invisible(NULL))
          do.call(
            m8_set_form,
            c(
              list(
                session = session,
                context = editor_context(),
                type = values$step_type
              ),
              values[setdiff(names(values), "step_type")]
            )
          )
        }
        stop("Form acknowledgements did not settle")
      }
      ready <- function()
        m8_wait(session, function() {
          acknowledge()
          !is.null(preview()) && !dirty() && is.null(loading_form())
        })
      m8_wait(session, function() !is.null(context()))
      m8_set_form(session, context())
      ready()
      baseline <- preview()
      expect_null(output$recipe_list)
      session$setInputs(flow_add = 1L)
      acknowledge()
      expect_identical(flow_navigation$view(), "choose")
      session$setInputs(flow_pick_duration = 1L, step_type = "duration")
      expect_identical(flow_navigation$view(), "edit")
      expect_identical(preview(), baseline)
      session$setInputs(duration_length = 4, duration_scope = "participant")
      session$setInputs(save_new = 1L)
      expect_identical(flow_navigation$view(), "edit")
      ready()
      expect_length(recipe()$steps, 1L)
      expect_match(output$recipe_list$html, "1 saved step")
      expect_match(output$recipe_list$html, "recipe_order_help")
      session$setInputs(flow_add = 2L)
      acknowledge()
      session$setInputs(
        flow_pick_invalidate = 1L,
        preparation_category = "values",
        value_type = "invalidate"
      )
      session$setInputs(measurement_upper = 5000)
      session$setInputs(save_new = 2L)
      expect_identical(flow_navigation$view(), "edit")
      ready()
      expect_length(recipe()$steps, 2L)
      expect_equal(recipe()$steps[[2L]]$parameters$arguments$upper, 5000)
      session$setInputs(flow_add = 3L)
      acknowledge()
      session$setInputs(
        flow_pick_variability = 1L,
        preparation_category = "values",
        value_type = "variability"
      )
      session$setInputs(variability_statistic = "cv", variability_threshold = 5)
      session$setInputs(save_new = 3L)
      ready()
      expect_identical(flow_navigation$view(), "edit")
      expect_length(recipe()$steps, 3L)
      expect_equal(recipe()$steps[[3L]]$parameters$arguments$threshold, 5)
      expect_identical(recipe(), preview()$candidate$recipe)
      # Quick iterations replace the selected step, refresh the preview and
      # leave the editor open. Expanded review uses the very same preview.
      ids <- vapply(recipe()$steps, `[[`, character(1), "id")
      session$setInputs(variability_threshold = 6, save_step = 1L)
      ready()
      expect_identical(flow_navigation$view(), "edit")
      expect_identical(vapply(recipe()$steps, `[[`, character(1), "id"), ids)
      expect_equal(recipe()$steps[[3L]]$parameters$arguments$threshold, 6)
      session$setInputs(variability_threshold = 5, save_step = 2L)
      ready()
      expect_equal(preview()$candidate$recipe$steps[[3L]]$parameters$arguments$threshold, 5)
      session$setInputs(flow_expand = 1L)
      expect_identical(flow_navigation$view(), "result")
      session$setInputs(flow_back = 1L)
      expect_identical(flow_navigation$view(), "edit")
      session$setInputs(apply = 1L)
      expect_identical(record()$raw_payload, original$raw_payload)
      expect_identical(record()$recipe, recipe())
      saved <- recipe()
      session$setInputs(selected_step = saved$steps[[3L]]$id, edit = 1L)
      acknowledge()
      expect_identical(flow_navigation$view(), "edit")
      session$setInputs(variability_threshold = 6)
      expect_true(dirty())
      session$setInputs(flow_result = 1L)
      expect_identical(flow_navigation$view(), "result")
      expect_true(dirty())
      session$setInputs(flow_resume = 1L)
      expect_identical(flow_navigation$view(), "edit")
      expect_equal(form()$variability_threshold, 6)
      session$setInputs(flow_add = 4L)
      expect_identical(flow_navigation$view(), "edit")
      expect_equal(form()$variability_threshold, 6)
      expect_identical(recipe(), saved)
      session$setInputs(flow_discard = 1L)
      acknowledge()
      expect_identical(flow_navigation$view(), "choose")
      expect_false(dirty())
      expect_identical(recipe(), saved)
      session$setInputs(
        flow_pick_invalidate = 2L,
        preparation_category = "values",
        value_type = "invalidate"
      )
      session$setInputs(measurement_lower = 10, measurement_upper = 1)
      session$setInputs(save_new = 4L)
      expect_identical(flow_navigation$view(), "edit")
      expect_identical(recipe(), saved)
      expect_match(output$editor_error, "lower|upper|bound", ignore.case = TRUE)
      session$setInputs(new_step = 1L)
      ready()
      expect_identical(flow_navigation$view(), "result")
      expect_identical(recipe(), saved)
      expect_identical(preview()$candidate$recipe, saved)
      expect_false(dirty())
      expect_null(editor_error())
    }
  )
})
