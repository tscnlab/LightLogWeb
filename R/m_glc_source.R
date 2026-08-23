glc_deep_link_states <- function() {
  c("absent", "valid", "matched", "unmatched", "invalid")
}

glc_default_registry_url <- function() {
  configured <- getOption(
    "LightLogWeb.glc_registry_url",
    "https://tscnlab.github.io/glc-registry/registry.json"
  )
  assert_scalar_string(configured, "GLC registry URL")
  configured
}

new_glc_deep_link <- function(
  state = glc_deep_link_states(),
  repository = NULL,
  message = NULL
) {
  state <- match.arg(state)
  if (!is.null(repository)) {
    assert_glc_repository(repository)
  }
  if (!is.null(message)) {
    assert_scalar_string(message, "message")
  }
  value <- structure(
    list(state = state, repository = repository, message = message),
    class = c("llw_glc_deep_link", "list")
  )
  assert_serializable_value(value, "GLC deep link")
  value
}

invalid_glc_deep_link <- function(message) {
  new_glc_deep_link(state = "invalid", message = message)
}

decode_glc_query_component <- function(value) {
  assert_scalar_string(value, "query component", allow_empty = TRUE)
  if (
    grepl("[[:cntrl:]]", value) ||
      grepl("%(?![0-9A-Fa-f]{2})", value, perl = TRUE)
  ) {
    abort_llw("Malformed query encoding.", type = "validation")
  }
  decoded <- utils::URLdecode(gsub("+", " ", value, fixed = TRUE))
  if (grepl("[[:cntrl:]]", decoded)) {
    abort_llw("Malformed query encoding.", type = "validation")
  }
  decoded
}

parse_glc_repository_query <- function(raw_query) {
  if (
    is.null(raw_query) || identical(raw_query, "") || identical(raw_query, "?")
  ) {
    return(new_glc_deep_link("absent"))
  }
  if (
    !is.character(raw_query) ||
      length(raw_query) != 1L ||
      is.na(raw_query) ||
      grepl("[[:cntrl:]]", raw_query)
  ) {
    return(invalid_glc_deep_link(
      "The GLC deep link has an invalid query string."
    ))
  }

  query <- if (startsWith(raw_query, "?")) {
    substring(raw_query, 2L)
  } else {
    raw_query
  }
  if (!nzchar(query)) {
    return(new_glc_deep_link("absent"))
  }
  if (grepl("?", query, fixed = TRUE) || grepl("#", query, fixed = TRUE)) {
    return(invalid_glc_deep_link(
      "The GLC deep link has an invalid query string."
    ))
  }

  parameters <- strsplit(query, "&", fixed = TRUE)[[1L]]
  decoded <- tryCatch(
    lapply(parameters, function(parameter) {
      separator <- regexpr("=", parameter, fixed = TRUE)[[1L]]
      if (separator < 0L) {
        name <- parameter
        value <- ""
      } else {
        name <- substr(parameter, 1L, separator - 1L)
        value <- substr(parameter, separator + 1L, nchar(parameter))
      }
      list(
        name = decode_glc_query_component(name),
        value = decode_glc_query_component(value)
      )
    }),
    llw_error = identity,
    error = identity
  )
  if (inherits(decoded, "error")) {
    return(invalid_glc_deep_link(
      "The GLC deep link contains malformed percent encoding."
    ))
  }

  repo_values <- vapply(
    decoded[vapply(decoded, `[[`, character(1), "name") == "repo"],
    `[[`,
    character(1),
    "value"
  )
  if (length(repo_values) == 0L) {
    return(new_glc_deep_link("absent"))
  }
  if (length(repo_values) > 1L) {
    return(invalid_glc_deep_link(
      "The GLC deep link contains more than one `repo` parameter."
    ))
  }
  repository <- repo_values[[1L]]
  if (!is_glc_repository(repository)) {
    return(invalid_glc_deep_link(
      paste(
        "The GLC deep link must contain one repository in the",
        "conservative `owner/repository` form."
      )
    ))
  }
  new_glc_deep_link("valid", repository = repository)
}

glc_registry_required_columns <- function() {
  c(
    "id",
    "repository",
    "current_status",
    "current_commit",
    "latest_pass_commit",
    "latest_pass_validated_at",
    "has_latest_pass",
    "is_current_pass",
    "attestation_verified",
    "registry_generated_at"
  )
}

validate_glc_registry <- function(packages) {
  if (!is.data.frame(packages)) {
    abort_llw(
      "The GLC registry must be a tabular value returned by `glc_packages()`.",
      type = "validation"
    )
  }
  missing <- setdiff(glc_registry_required_columns(), names(packages))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "The GLC registry is missing field(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  if (
    !is.character(packages$repository) ||
      any(!vapply(packages$repository, is_glc_repository, logical(1))) ||
      anyDuplicated(packages$repository)
  ) {
    abort_llw(
      "The GLC registry contains an invalid or repeated repository.",
      type = "validation"
    )
  }
  if (
    !is.logical(packages$has_latest_pass) ||
      anyNA(packages$has_latest_pass)
  ) {
    abort_llw(
      "The GLC registry has invalid passing-revision flags.",
      type = "validation"
    )
  }
  passing_revisions <- packages$latest_pass_commit[packages$has_latest_pass]
  if (
    !is.character(packages$latest_pass_commit) ||
      any(!grepl("^[0-9a-fA-F]{40}$", passing_revisions))
  ) {
    abort_llw(
      "The GLC registry contains a non-exact passing revision.",
      type = "validation"
    )
  }
  invisible(packages)
}

glcdp_runtime_provenance <- function() {
  description <- utils::packageDescription("glcdp")
  revision <- description$RemoteSha %||%
    description$GithubSHA1 %||%
    NA_character_
  value <- structure(
    list(
      version = as.character(description$Version),
      revision = if (is.null(revision) || !nzchar(revision)) {
        NA_character_
      } else {
        tolower(revision)
      }
    ),
    class = c("llw_glcdp_runtime_provenance", "list")
  )
  assert_serializable_value(value, "glcdp runtime provenance")
  value
}

glc_registry_select_repository <- function(
  packages,
  repository,
  registry_url,
  dependency = glcdp_runtime_provenance()
) {
  validate_glc_registry(packages)
  assert_glc_repository(repository)
  assert_scalar_string(registry_url, "registry_url")
  matches <- which(packages$repository == repository)
  if (length(matches) == 0L) {
    return(NULL)
  }
  if (length(matches) != 1L) {
    abort_llw(
      "The GLC registry repository match is not unique.",
      type = "validation"
    )
  }
  row <- packages[matches, , drop = FALSE]
  has_latest_pass <- isTRUE(row$has_latest_pass[[1L]])
  value <- structure(
    list(
      registry_url = registry_url,
      registry_generated_at = row$registry_generated_at[[1L]],
      registry_id = row$id[[1L]],
      repository = row$repository[[1L]],
      requested_revision = "latest_pass",
      resolved_revision = if (has_latest_pass) {
        tolower(row$latest_pass_commit[[1L]])
      } else {
        NA_character_
      },
      has_latest_pass = has_latest_pass,
      current_validation_status = row$current_status[[1L]],
      current_revision = row$current_commit[[1L]],
      is_current_pass = isTRUE(row$is_current_pass[[1L]]),
      latest_pass_validated_at = row$latest_pass_validated_at[[1L]],
      attestation_verified = row$attestation_verified[[1L]],
      glcdp_version = dependency$version,
      glcdp_revision = dependency$revision
    ),
    class = c("llw_glc_registry_selection", "list")
  )
  assert_serializable_value(value, "GLC registry selection")
  value
}

glcdp_registry_load <- function(registry_url, refresh = FALSE) {
  assert_scalar_string(registry_url, "registry_url")
  assert_flag(refresh, "refresh")
  glcdp::glc_packages(registry = registry_url, refresh = refresh)
}

glcdp_registry_search <- function(packages, query = NULL) {
  if (!is.null(query)) {
    assert_scalar_string(query, "query", allow_empty = TRUE)
  }
  glcdp::glc_search_packages(query = query, packages = packages)
}

glc_registry_select_action_id <- function(repository) {
  assert_glc_repository(repository)
  encoded_repository <- paste0(
    sprintf("%02x", as.integer(charToRaw(enc2utf8(repository)))),
    collapse = ""
  )
  paste0("select_repository_", encoded_repository)
}

glc_registry_revision_url <- function(repository, revision) {
  assert_glc_repository(repository)
  if (
    !is.character(revision) ||
      length(revision) != 1L ||
      is.na(revision) ||
      !grepl("^[0-9a-fA-F]{40}$", revision)
  ) {
    abort_llw(
      "A GLC repository link requires an exact 40-character revision.",
      type = "validation"
    )
  }
  paste0(
    "https://github.com/",
    repository,
    "/tree/",
    tolower(revision)
  )
}

glc_registry_table_tag <- function(
  packages,
  selected_repository = NULL,
  ns = identity
) {
  validate_glc_registry(packages)
  if (!is.null(selected_repository)) {
    assert_glc_repository(selected_repository)
  }
  if (!is.function(ns)) {
    abort_llw("`ns` must be a function.", type = "validation")
  }
  if (nrow(packages) == 0L) {
    return(tags$p(
      class = "llw-secondary p-3 mb-0",
      "No registered packages match this search."
    ))
  }

  rows <- lapply(seq_len(nrow(packages)), function(index) {
    repository <- packages$repository[[index]]
    selected <- identical(repository, selected_repository)
    has_latest_pass <- isTRUE(packages$has_latest_pass[[index]])
    revision <- packages$latest_pass_commit[[index]]
    select_action <- if (selected) {
      tags$button(
        type = "button",
        class = "btn btn-sm btn-primary llw-glc-select-button",
        disabled = NA,
        `aria-label` = paste(repository, "is selected"),
        icon("check"),
        "Selected"
      )
    } else {
      actionButton(
        ns(glc_registry_select_action_id(repository)),
        "Select",
        class = "btn-sm btn-primary llw-glc-select-button",
        `aria-label` = paste("Select", repository)
      )
    }
    revision_link <- if (has_latest_pass) {
      tags$a(
        href = glc_registry_revision_url(repository, revision),
        target = "_blank",
        rel = "noopener noreferrer",
        class = "btn btn-sm btn-outline-secondary llw-icon-button",
        title = paste("Open exact passing revision", revision, "on GitHub"),
        `aria-label` = paste(
          "Open",
          repository,
          "at exact passing revision",
          revision,
          "on GitHub"
        ),
        icon("github")
      )
    } else {
      tags$span(
        class = "text-body-secondary",
        `aria-label` = "No passing revision is available",
        "\u2014"
      )
    }
    tags$tr(
      class = if (selected) "table-primary" else NULL,
      `data-selected` = if (selected) "true" else "false",
      tags$td(select_action),
      tags$td(revision_link),
      tags$td(repository),
      tags$td(glc_display_value(packages$id[[index]])),
      tags$td(glc_display_value(packages$current_status[[index]])),
      tags$td(
        if (has_latest_pass) {
          paste0(substr(revision, 1L, 12L), "\u2026")
        } else {
          "None recorded"
        }
      ),
      tags$td(
        if (is.na(packages$attestation_verified[[index]])) {
          "Unknown"
        } else if (packages$attestation_verified[[index]]) {
          "Verified"
        } else {
          "Not verified"
        }
      ),
      tags$td(glc_display_value(packages$latest_pass_validated_at[[index]]))
    )
  })

  tags$div(
    class = "table-responsive text-nowrap llw-glc-registry-table-scroll",
    tags$table(
      class = "table table-striped table-hover align-middle mb-0",
      tags$caption(
        class = "visually-hidden",
        "Registered GLC packages and their exact passing revisions"
      ),
      tags$thead(tags$tr(
        tags$th(scope = "col", "Select"),
        tags$th(scope = "col", "GitHub"),
        tags$th(scope = "col", "Repository"),
        tags$th(scope = "col", "Registry ID"),
        tags$th(scope = "col", "Current validation"),
        tags$th(scope = "col", "Latest passing revision"),
        tags$th(scope = "col", "Attestation"),
        tags$th(scope = "col", "Passing validation time")
      )),
      tags$tbody(rows)
    )
  )
}

glc_display_value <- function(value, unknown = "Unknown") {
  if (
    length(value) != 1L ||
      is.na(value) ||
      !nzchar(as.character(value))
  ) {
    return(unknown)
  }
  as.character(value)
}

glcSourceUI <- function(id) {
  ns <- NS(id)
  tags$section(
    class = "llw-glc-source",
    `aria-labelledby` = ns("title"),
    tags$style(HTML(paste(
      ".llw-glc-source .llw-glc-registry-controls .shiny-input-container {",
      "margin-bottom: 0; width: 100%; }",
      ".llw-glc-source .llw-glc-registry-controls .form-control,",
      ".llw-glc-source .llw-glc-refresh-button {",
      "height: 2.75rem; min-height: 2.75rem; }",
      ".llw-glc-source .llw-glc-select-button {",
      "align-items: center; display: inline-flex; justify-content: center;",
      "width: 6.75rem; }",
      ".llw-glc-source .llw-glc-registry-table-scroll {",
      "overflow-x: auto; overflow-y: hidden; }"
    ))),
    tags$div(
      class = "mb-4",
      tags$h2(id = ns("title"), "Discover validated GLC packages"),
      tags$p(
        class = "llw-secondary",
        paste(
          "Search the registry and select one exact repository.",
          "Selection alone never reads, downloads, or creates a dataset."
        )
      )
    ),
    card(
      card_header("Current registry"),
      uiOutput(ns("registry_status")),
      tags$div(
        class = paste(
          "d-flex flex-column flex-md-row align-items-md-end gap-2",
          "llw-glc-registry-controls px-3 pt-2 pb-3"
        ),
        tags$div(
          class = "flex-grow-1 llw-glc-registry-search",
          textInput(
            ns("search"),
            "Search registry",
            placeholder = "Repository or registry ID",
            width = "100%"
          )
        ),
        tags$div(
          class = "flex-shrink-0 llw-glc-registry-refresh",
          actionButton(
            ns("refresh"),
            "Refresh registry",
            icon = icon("arrows-rotate"),
            class = "btn-primary llw-glc-refresh-button"
          )
        )
      ),
      uiOutput(ns("registry_table"))
    )
  )
}

glcSourceServer <- function(
  id,
  registry_url,
  raw_query = NULL,
  registry_loader = glcdp_registry_load,
  registry_search = glcdp_registry_search,
  registry_select = glc_registry_select_repository
) {
  assert_scalar_string(registry_url, "registry_url")
  if (
    !is.function(registry_loader) ||
      !is.function(registry_search) ||
      !is.function(registry_select)
  ) {
    abort_llw(
      "GLC registry loaders, searches, and selectors must be functions.",
      type = "validation"
    )
  }

  moduleServer(id, function(input, output, session) {
    query_source <- if (shiny::is.reactive(raw_query)) {
      raw_query
    } else if (is.null(raw_query)) {
      reactive(session$clientData$url_search)
    } else {
      query_value <- raw_query
      reactive(query_value)
    }

    registry_value <- reactiveVal(NULL)
    selection_value <- reactiveVal(NULL)
    deep_link_value <- reactiveVal(new_glc_deep_link("absent"))
    error_value <- reactiveVal(NULL)
    status_value <- reactiveVal(list(
      state = "queued",
      heading = "Loading registry",
      message = "Contacting the official public registry."
    ))
    initial_load_started <- FALSE
    selection_observers <- list()

    select_repository <- function(packages, repository) {
      selection <- registry_select(
        packages,
        repository,
        registry_url
      )
      selection_value(selection)
      selection
    }

    clear_selection <- function() {
      selection_value(NULL)
      deep_link_value(new_glc_deep_link("absent"))
      invisible(NULL)
    }

    apply_deep_link <- function(raw_query_value) {
      deep_link <- parse_glc_repository_query(raw_query_value)
      deep_link_value(deep_link)
      selection_value(NULL)
      packages <- registry_value()
      if (
        is.null(packages) ||
          !identical(deep_link$state, "valid")
      ) {
        return(invisible(FALSE))
      }
      matched <- select_repository(packages, deep_link$repository)
      if (is.null(matched)) {
        deep_link_value(new_glc_deep_link(
          "unmatched",
          repository = deep_link$repository,
          message = paste(
            "The repository is well formed but does not exactly match",
            "the current registry."
          )
        ))
        return(invisible(FALSE))
      }
      deep_link_value(new_glc_deep_link(
        "matched",
        repository = deep_link$repository,
        message = paste(
          "The exact registry repository was preselected.",
          "No package data were read or downloaded."
        )
      ))
      invisible(TRUE)
    }

    clear_selection_observers <- function() {
      for (observer in selection_observers) {
        observer$destroy()
      }
      selection_observers <<- list()
      invisible(NULL)
    }

    replace_selection_observers <- function(packages) {
      clear_selection_observers()
      observers <- vector("list", nrow(packages))
      for (index in seq_len(nrow(packages))) {
        observers[[index]] <- local({
          repository <- packages$repository[[index]]
          action_id <- glc_registry_select_action_id(repository)
          observeEvent(
            input[[action_id]],
            {
              current <- registry_value()
              if (
                is.null(current) ||
                  !repository %in% current$repository
              ) {
                return(invisible(NULL))
              }
              select_repository(current, repository)
            },
            ignoreInit = TRUE
          )
        })
      }
      selection_observers <<- observers
      invisible(NULL)
    }

    session$onSessionEnded(clear_selection_observers)

    load_registry_now <- function(refresh = FALSE, apply_deep_link = FALSE) {
      previous <- registry_value()
      status_value(list(
        state = "queued",
        heading = if (refresh) "Refreshing registry" else "Loading registry",
        message = "Requesting the current registry view."
      ))
      result <- tryCatch(
        registry_loader(registry_url = registry_url, refresh = refresh),
        error = identity
      )
      if (!inherits(result, "error")) {
        result <- tryCatch(
          {
            validate_glc_registry(result)
            result
          },
          error = identity
        )
      }
      if (inherits(result, "error")) {
        error_value(result)
        status_value(list(
          state = if (is.null(previous)) "error" else "warning",
          heading = if (is.null(previous)) {
            "Registry unavailable"
          } else {
            "Refresh failed"
          },
          message = if (is.null(previous)) {
            paste(
              "The GLC registry could not be loaded.",
              "No source was selected and no data were transferred."
            )
          } else {
            paste(
              "The last successfully loaded registry remains visible.",
              "No selection or session data were changed."
            )
          }
        ))
        return(invisible(FALSE))
      }

      error_value(NULL)
      registry_value(result)
      replace_selection_observers(result)
      current <- selection_value()
      if (!is.null(current)) {
        current <- select_repository(result, current$repository)
      }
      if (is.null(current)) {
        selection_value(NULL)
      }

      deep_link <- deep_link_value()
      if (apply_deep_link && identical(deep_link$state, "valid")) {
        matched <- select_repository(result, deep_link$repository)
        if (is.null(matched)) {
          deep_link_value(new_glc_deep_link(
            "unmatched",
            repository = deep_link$repository,
            message = paste(
              "The repository is well formed but does not exactly match",
              "the current registry."
            )
          ))
        } else {
          deep_link_value(new_glc_deep_link(
            "matched",
            repository = deep_link$repository,
            message = paste(
              "The exact registry repository was preselected.",
              "No package data were read or downloaded."
            )
          ))
        }
      }

      status_value(list(
        state = "complete",
        heading = "Registry ready",
        message = paste(
          nrow(result),
          if (nrow(result) == 1L) "package is" else "packages are",
          "available in the current registry view."
        )
      ))
      invisible(TRUE)
    }

    initialize_registry <- function() {
      if (isTRUE(initial_load_started)) return(invisible(FALSE))
      initial_load_started <<- TRUE
      isolate({
        initial_query <- query_source()
        deep_link_value(parse_glc_repository_query(initial_query))
        load_registry_now(refresh = FALSE, apply_deep_link = TRUE)
      })
    }

    session$onFlushed(initialize_registry, once = TRUE)

    observeEvent(
      input$refresh,
      {
        load_registry_now(refresh = TRUE, apply_deep_link = FALSE)
      },
      ignoreInit = TRUE
    )

    filtered_registry <- reactive({
      packages <- registry_value()
      req(packages)
      query <- trimws(input$search %||% "")
      result <- registry_search(
        packages = packages,
        query = if (nzchar(query)) query else NULL
      )
      validate_glc_registry(result)
      result
    })

    output$registry_status <- renderUI({
      status <- status_value()
      deep_link <- deep_link_value()
      deep_link_message <- if (identical(deep_link$state, "absent")) {
        NULL
      } else {
        deep_link$message %||% "The GLC deep link was inspected."
      }
      state <- if (
        identical(status$state, "complete") &&
          deep_link$state %in% c("invalid", "unmatched")
      ) {
        "warning"
      } else {
        status$state
      }
      heading <- if (!identical(status$state, "complete")) {
        status$heading
      } else {
        switch(
          deep_link$state,
          invalid = "Registry ready; deep link rejected",
          unmatched = "Registry ready; deep link not found",
          status$heading
        )
      }
      llw_status_callout(
        state,
        paste(c(status$message, deep_link_message), collapse = " "),
        heading = heading,
        live = TRUE,
        compact = TRUE
      )
    })

    output$registry_table <- renderUI({
      selection <- selection_value()
      glc_registry_table_tag(
        filtered_registry(),
        selected_repository = if (is.null(selection)) {
          NULL
        } else {
          selection$repository
        },
        ns = session$ns
      )
    })

    # The source selector is embedded in a hidden wizard panel. Keep its live
    # registry outputs current so returning from a later step cannot expose
    # indefinitely suspended, empty output containers.
    outputOptions(output, "registry_status", suspendWhenHidden = FALSE)
    outputOptions(output, "registry_table", suspendWhenHidden = FALSE)

    output$selection_detail <- renderUI({
      selection <- selection_value()
      if (is.null(selection)) {
        return(llw_status_callout(
          "idle",
          paste(
            "Choose one exact repository from the current registry.",
            "This step does not open package contents."
          ),
          heading = "No source selected",
          compact = TRUE
        ))
      }
      status <- if (selection$has_latest_pass) "complete" else "warning"
      detail <- data.frame(
        Field = c(
          "Repository",
          "Registry ID",
          "Current validation",
          "Latest passing revision",
          "Attestation",
          "Passing validation time",
          "Registry generated",
          "glcdp dependency"
        ),
        Value = c(
          selection$repository,
          selection$registry_id,
          selection$current_validation_status,
          if (selection$has_latest_pass) {
            selection$resolved_revision
          } else {
            "None recorded"
          },
          if (is.na(selection$attestation_verified)) {
            "Unknown"
          } else if (selection$attestation_verified) {
            "Verified"
          } else {
            "Not verified"
          },
          glc_display_value(selection$latest_pass_validated_at),
          glc_display_value(selection$registry_generated_at),
          paste0(
            selection$glcdp_version,
            if (!is.na(selection$glcdp_revision)) {
              paste0(" at ", selection$glcdp_revision)
            } else {
              ""
            }
          )
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      tagList(
        llw_status_callout(
          status,
          if (selection$has_latest_pass) {
            paste(
              "The exact latest passing revision is fixed for the next",
              "inspection step. No package data were read or downloaded."
            )
          } else {
            paste(
              "This registry entry has no passing revision and cannot proceed",
              "to package inspection. No fallback revision will be used."
            )
          },
          heading = if (selection$has_latest_pass) {
            "Validated source selected"
          } else {
            "No passing revision"
          },
          compact = TRUE
        ),
        tags$div(
          class = "table-responsive mt-3",
          tags$table(
            class = "table table-sm align-middle mb-0",
            tags$tbody(lapply(seq_len(nrow(detail)), function(index) {
              tags$tr(
                tags$th(scope = "row", detail$Field[[index]]),
                tags$td(class = "text-break", detail$Value[[index]])
              )
            }))
          )
        )
      )
    })

    list(
      registry = reactive(registry_value()),
      filtered_registry = filtered_registry,
      selection = reactive(selection_value()),
      deep_link = reactive(deep_link_value()),
      status = reactive(status_value()),
      error = reactive(error_value()),
      initialize = initialize_registry,
      clear_selection = clear_selection,
      apply_deep_link = apply_deep_link
    )
  })
}
