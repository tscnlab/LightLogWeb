pkgload::load_all(".", export_all = TRUE, quiet = TRUE)

glc_source_showcase_registry_url <- function() {
  "https://tscnlab.github.io/glc-registry/registry.json"
}

glc_source_showcase_deep_link_query <- function(packages, raw_query = "") {
  validate_glc_registry(packages)
  parsed <- parse_glc_repository_query(raw_query %||% "")
  if (identical(parsed$state, "valid")) return(raw_query)
  if (!identical(parsed$state, "absent")) return(raw_query)

  candidates <- as.character(
    packages$repository[packages$has_latest_pass %in% TRUE]
  )
  preferred <- "tscnlab/melidos-iztech-glc-dataset"
  repository <- if (preferred %in% candidates) {
    preferred
  } else if (length(candidates) > 0L) {
    candidates[[1L]]
  } else {
    NULL
  }
  if (is.null(repository)) return(NULL)
  paste0("?repo=", utils::URLencode(repository, reserved = TRUE))
}

glc_source_showcase_preview_worker <- function(payload, spec) {
  review <- payload$transfer_review
  LightLogWeb:::validate_glc_transfer_review(review)
  allocated <- review$allocated_files
  rows_per_file <- as.integer(review$allocation$per_file_n_max)
  output_names <- unique(as.character(review$projection$output_columns$name))

  fixture_column <- function(name, count, file_index) {
    lower <- tolower(name)
    if (grepl("date|time", lower)) {
      return(as.POSIXct(
        "2026-08-01 12:00:00",
        tz = "UTC"
      ) + seq_len(count) + (file_index - 1L) * 60L)
    }
    if (grepl("(^|_)id$|participant|file_group|file\\.name", lower)) {
      return(paste0("fixture-", file_index, "-", seq_len(count)))
    }
    as.numeric(seq_len(count) + (file_index - 1L) * count)
  }

  file_data <- lapply(seq_len(nrow(allocated)), function(index) {
    values <- stats::setNames(
      lapply(output_names, fixture_column, count = rows_per_file,
        file_index = index),
      output_names
    )
    as.data.frame(values, check.names = FALSE, stringsAsFactors = FALSE)
  })
  collected <- do.call(rbind, file_data)
  rownames(collected) <- NULL
  collection <- data.frame(
    dataset_id = as.character(allocated$dataset_id),
    file_group_id = as.character(allocated$file_group_id),
    files = I(lapply(
      allocated$declared_path,
      function(path) file.path("deterministic-fixture", path)
    )),
    runtime_input_ids = I(lapply(
      seq_len(nrow(allocated)),
      function(index) sprintf("fixture://preview-input/%03d", index)
    )),
    data = I(file_data),
    stringsAsFactors = FALSE
  )

  exports <- LightLogWeb:::glcdp_export_contract()
  api <- stats::setNames(
    lapply(exports, function(name) function(...) NULL),
    exports
  )
  api$glc_open <- function(...) list(fixture = TRUE)
  api$glc_read <- function(...) collection
  api$glc_collect <- function(x, standardize) collected
  LightLogWeb:::glcdp_transfer_preview_execute(
    payload,
    api = api,
    execution_mode = "fixture"
  )
}

glc_source_showcase_app <- function() {
  registry_url <- glc_source_showcase_registry_url()
  glcdp_provenance <- glcdp_runtime_provenance()
  runtime_profile <- resolve_runtime_profile(
    profile = "hosted",
    workers = 1L,
    is_interactive = TRUE
  )

  ui <- lightlogweb_page(page_fluid(
    lightlogweb_head(),
    lightlogweb_skip_link(),
    tags$main(
      id = "llw-main-content",
      class = "llw-main-shell",
      tabindex = "-1",
      llw_view_header(
        "Milestone 6 · Slice 2C-A",
        "Review and exercise one bounded typed preview",
          paste(
            "This standalone review app reads the current public GLC registry",
            "through the pinned glcdp planner revision. After metadata-only",
            "inspection, full-schema structural planning, and narrowing, the",
            "user can review a bounded transfer contract. The showcase uses",
            "injected local preview records and never accesses live measurements."
        )
      ),
      glcPreviewUI(
        "glc_preview",
        source_ui = glcSourceUI("glc_source")
      ),
      card(
        card_header("Development-only registry checks"),
        uiOutput("arrival_mode_control"),
        uiOutput("arrival_mode_status"),
        tags$p(
          tags$strong("Registry source: "),
          tags$a(
            registry_url,
            href = registry_url,
            target = "_blank",
            rel = "noreferrer"
          )
        ),
        tags$p(
          class = "llw-secondary",
          paste(
            "The registry loads when this page starts. Its records and row",
            "count are live and may change independently of this app. These",
            "controls exercise recovery behavior and are not production UI."
          )
        ),
        checkboxInput(
          "fail_refresh",
          "Simulate a connection failure on the next refresh",
          value = FALSE
        ),
        tags$p(
          class = "llw-secondary",
          paste(
            "After a successful load, enable the simulation and return to Step",
            "1 to use Refresh registry. The last successful registry and",
            "selection remain visible. Disable it and refresh again to retry."
          )
        ),
        tags$div(
          id = "live_registry_review",
          class = "shiny-html-output",
          llw_status_callout(
            "queued",
            paste(
              "Contacting the official public registry through glcdp.",
              "No package data are being opened or transferred."
            ),
            heading = "Loading live registry",
            live = TRUE,
            compact = TRUE
          )
        )
      ),
      card(
        card_header("Slice 2C-A safety boundary"),
        tags$p(
          class = "mb-1",
          tags$strong("Pinned public API: "),
          "glcdp ",
          tags$code(glcdp_provenance$version),
          " at ",
          tags$code(glcdp_provenance$revision)
        ),
        tags$p(
          class = "mb-1",
          tags$strong("Inspection transfer: "),
          paste(
            "validated package information and core metadata only; the",
            "size is not available and is shown as an informational note before",
            "the explicit inspection action."
          )
        ),
        tags$p(
          class = "mb-1",
          tags$strong("Showcase preview: "),
          paste(
            "the explicit preview action uses deterministic injected local",
            "records. It exercises the public read-and-collect contract without",
            "opening a live package or measurement file."
          )
        ),
        tags$p(
          class = "mb-0",
          tags$strong("Closed actions: "),
          paste(
            "no full read, import, dataset creation, persistence, download,",
            "promotion, or later-slice action is available."
          )
        )
      )
    ),
    theme = lightlogweb_theme()
  ))

  server <- function(input, output, session) {
    runtime <- new_session_runtime(
      runtime_profile,
      session = session
    )
    registry_loader <- function(registry_url, refresh = FALSE) {
      if (isTRUE(refresh) && isTRUE(input$fail_refresh)) {
        stop(structure(
          list(
            message = "Showcase connection failure requested.",
            call = NULL
          ),
          class = c("glcdp_registry_error", "error", "condition")
        ))
      }
      withProgress(
        message = if (refresh) {
          "Refreshing the live GLC registry"
        } else {
          "Loading the live GLC registry"
        },
        detail = "Reading the official public registry through glcdp.",
        value = 0.2,
        glcdp_registry_load(registry_url, refresh = refresh)
      )
    }

    arrival_mode_default <- function() "fresh"
    arrival_status <- reactiveVal(list(
      state = "idle",
      heading = "Fresh workflow",
      message = "No package is preselected. Choose one source in Step 1."
    ))

    output$arrival_mode_control <- renderUI({
      tagList(
        radioButtons(
          "arrival_mode",
          "Simulated arrival path",
          choices = c(
            "Fresh workflow: begin at source selection" = "fresh",
            "Deep-link workflow: preselect and inspect" = "deep_link"
          ),
          selected = arrival_mode_default(),
          inline = TRUE
        ),
        tags$p(
          class = "small llw-secondary",
          paste(
            "This development-only control resets the wizard so both entry",
            "paths can be reviewed. Fresh clears the source selection.",
            "Deep-link applies the strict repo= query and begins inspection.",
            "It is not production UI."
          )
        )
      )
    })

    source_state <- glcSourceServer(
      "glc_source",
      registry_url = registry_url,
      raw_query = "",
      registry_loader = registry_loader
    )
    observeEvent(
      list(input$arrival_mode, source_state$registry()),
      {
        mode <- input$arrival_mode %||% arrival_mode_default()
        if (identical(mode, "deep_link")) {
          packages <- source_state$registry()
          if (is.null(packages)) {
            arrival_status(list(
              state = "queued",
              heading = "Preparing deep-link workflow",
              message = "Waiting for the public registry before choosing a source."
            ))
            return(invisible(NULL))
          }
          query <- glc_source_showcase_deep_link_query(
            packages,
            session$clientData$url_search %||% ""
          )
          if (is.null(query)) {
            source_state$clear_selection()
            arrival_status(list(
              state = "warning",
              heading = "No validated deep-link source",
              message = paste(
                "The current registry has no package with an exact passing",
                "revision. Switch to Fresh or retry after refreshing the registry."
              )
            ))
            return(invisible(NULL))
          }
          matched <- source_state$apply_deep_link(query)
          deep_link <- source_state$deep_link()
          arrival_status(list(
            state = if (isTRUE(matched)) "complete" else "warning",
            heading = if (isTRUE(matched)) {
              "Deep-link workflow applied"
            } else {
              "Deep-link workflow needs attention"
            },
            message = if (isTRUE(matched)) {
              paste(
                "Preselected",
                deep_link$repository,
                "from the strict repository query and started metadata-only inspection."
              )
            } else {
              deep_link$message %||%
                paste(
                  "The simulated query could not be applied.",
                  "Switch to Fresh or select another package."
                )
            }
          ))
        } else {
          source_state$clear_selection()
          arrival_status(list(
            state = "idle",
            heading = "Fresh workflow",
            message = "No package is preselected. Choose one source in Step 1."
          ))
        }
      },
      ignoreNULL = FALSE
    )

    output$arrival_mode_status <- renderUI({
      status <- arrival_status()
      llw_status_callout(
        status$state,
        status$message,
        heading = status$heading,
        live = TRUE,
        compact = TRUE
      )
    })
    glcPreviewServer(
      "glc_preview",
      source_selection = source_state$selection,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      preview_worker = glc_source_showcase_preview_worker,
      auto_inspect = reactive({
        identical(
          input$arrival_mode %||% arrival_mode_default(),
          "deep_link"
        )
      })
    )

    output$live_registry_review <- renderUI({
      packages <- source_state$registry()
      status <- source_state$status()
      if (is.null(packages)) {
        return(llw_status_callout(
          status$state,
          paste(
            "The official public registry is being contacted.",
            "No package data are being opened or transferred."
          ),
          heading = status$heading,
          live = TRUE,
          compact = TRUE
        ))
      }

      candidates <- packages$repository[packages$has_latest_pass]
      if (length(candidates) == 0L) {
        candidates <- packages$repository
      }
      example_repository <- if (length(candidates) == 0L) {
        NULL
      } else {
        candidates[[1L]]
      }
      example_query <- if (is.null(example_repository)) {
        NULL
      } else {
        paste0(
          "?repo=",
          utils::URLencode(example_repository, reserved = TRUE)
        )
      }

      tagList(
        llw_status_callout(
          if (identical(status$state, "warning")) "warning" else "complete",
          paste(
            "The records shown below came from the official registry in this",
            "session. Search and selection remain metadata-only operations."
          ),
          heading = "Live registry connected",
          live = TRUE,
          compact = TRUE
        ),
        if (!is.null(example_repository)) {
          tags$p(
            class = "mt-3 mb-0",
            "Current exact deep-link example: ",
            tags$code(example_query),
            " for ",
            tags$code(example_repository),
            "."
          )
        }
      )
    })
  }

  shinyApp(ui = ui, server = server)
}

# Run from the package root with:
# Rscript -e 'source("dev/milestone-6-glc-source-app.R"); shiny::runApp(glc_source_showcase_app())'
