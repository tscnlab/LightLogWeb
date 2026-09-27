glc_source_test_registry <- function() {
  value <- data.frame(
    id = c("validated", "warning", "no-pass"),
    repository = c(
      "fixture/validated",
      "fixture/warning",
      "fixture/no-pass"
    ),
    current_status = c("pass", "warning", "fail"),
    current_commit = c(strrep("a", 40L), strrep("b", 40L), strrep("c", 40L)),
    latest_pass_commit = c(strrep("a", 40L), strrep("d", 40L), NA_character_),
    latest_pass_validated_at = c(
      "2026-07-30T10:00:00Z",
      "2026-07-29T10:00:00Z",
      NA_character_
    ),
    has_latest_pass = c(TRUE, TRUE, FALSE),
    is_current_pass = c(TRUE, FALSE, FALSE),
    attestation_verified = c(TRUE, FALSE, NA),
    registry_generated_at = rep("2026-07-30T12:00:00Z", 3L),
    stringsAsFactors = FALSE
  )
  class(value) <- c("glc_registry", class(value))
  value
}

glc_source_test_search <- function(packages, query = NULL) {
  if (is.null(query)) {
    return(packages)
  }
  needle <- tolower(query)
  keep <- grepl(needle, tolower(packages$id), fixed = TRUE) |
    grepl(needle, tolower(packages$repository), fixed = TRUE)
  result <- packages[keep, , drop = FALSE]
  class(result) <- class(packages)
  result
}

test_that("GLC registry defaults to the official endpoint and preserves overrides", {
  previous <- options(LightLogWeb.glc_registry_url = NULL)
  tryCatch(
    {
      expect_identical(
        glc_default_registry_url(),
        "https://registry.globallightcommons.org/registry.json"
      )
      options(
        LightLogWeb.glc_registry_url = "https://example.org/registry.json"
      )
      expect_identical(
        glc_default_registry_url(),
        "https://example.org/registry.json"
      )
      options(LightLogWeb.glc_registry_url = "")
      expect_error(glc_default_registry_url(), class = "llw_validation_error")
    },
    finally = options(previous)
  )
})

test_that("registry loading forwards the exact endpoint and refresh request", {
  calls <- list()
  testthat::local_mocked_bindings(
    glc_packages = function(registry, refresh = FALSE) {
      calls[[length(calls) + 1L]] <<- list(
        registry = registry,
        refresh = refresh
      )
      glc_source_test_registry()
    },
    .package = "glcdp"
  )
  endpoint <- "https://registry.globallightcommons.org/registry.json"
  expect_identical(glcdp_registry_load(endpoint), glc_source_test_registry())
  expect_identical(
    glcdp_registry_load("https://example.org/registry.json", refresh = TRUE),
    glc_source_test_registry()
  )
  expect_identical(
    calls,
    list(
      list(registry = endpoint, refresh = FALSE),
      list(registry = "https://example.org/registry.json", refresh = TRUE)
    )
  )
})

test_that("GLC deep links decode once and accept at most one strict repository", {
  expect_identical(parse_glc_repository_query(NULL)$state, "absent")
  expect_identical(parse_glc_repository_query("?other=value")$state, "absent")

  valid <- parse_glc_repository_query(
    "?other=value&repo=fixture%2Fvalidated"
  )
  expect_identical(valid$state, "valid")
  expect_identical(valid$repository, "fixture/validated")

  invalid <- c(
    "?repo=fixture%252Fvalidated",
    "?repo=fixture%2",
    "?repo=fixture/validated&repo=fixture/warning",
    "?repo=https%3A%2F%2Fexample.org%2Ffixture%2Fvalidated",
    "?repo=fixture%2Fvalidated%2Fextra",
    "?repo=fixture%2F..",
    "?repo=fixture%2Fvalidated+",
    "?repo=fixture%2Fvalidated%0A",
    "?%72epo=fixture%2Fvalidated&repo=fixture%2Fvalidated"
  )
  states <- vapply(
    invalid,
    function(query) parse_glc_repository_query(query)$state,
    character(1)
  )
  expect_true(all(states == "invalid"))
})

test_that("registry selection is exact and retains immutable public provenance", {
  registry <- glc_source_test_registry()
  dependency <- list(
    version = "1.0.0",
    revision = "bf97cb30555569886f08db0bdb25421c546ccf0a"
  )
  selection <- glc_registry_select_repository(
    registry,
    "fixture/warning",
    "fixture://registry",
    dependency = dependency
  )

  expect_s3_class(selection, "llw_glc_registry_selection")
  expect_identical(selection$repository, "fixture/warning")
  expect_identical(selection$requested_revision, "latest_pass")
  expect_identical(selection$resolved_revision, strrep("d", 40L))
  expect_false(selection$is_current_pass)
  expect_true(selection$has_latest_pass)
  expect_identical(selection$glcdp_revision, dependency$revision)
  expect_null(nonserializable_path(selection))
  expect_null(glc_registry_select_repository(
    registry,
    "fixture/missing",
    "fixture://registry",
    dependency = dependency
  ))
  expect_null(glc_registry_select_repository(
    registry,
    "Fixture/warning",
    "fixture://registry",
    dependency = dependency
  ))

  no_pass <- glc_registry_select_repository(
    registry,
    "fixture/no-pass",
    "fixture://registry",
    dependency = dependency
  )
  expect_false(no_pass$has_latest_pass)
  expect_true(is.na(no_pass$resolved_revision))
})

test_that("registry validation rejects ambiguous or non-exact source rows", {
  duplicate <- glc_source_test_registry()
  duplicate$repository[[2L]] <- duplicate$repository[[1L]]
  expect_error(
    validate_glc_registry(duplicate),
    class = "llw_validation_error",
    regexp = "invalid or repeated"
  )

  moving <- glc_source_test_registry()
  moving$latest_pass_commit[[1L]] <- "main"
  expect_error(
    validate_glc_registry(moving),
    class = "llw_validation_error",
    regexp = "non-exact passing revision"
  )
})

test_that("registry row actions use stable identity and exact GitHub links", {
  repository <- "fixture/validated"
  revision <- strrep("A", 40L)

  expect_identical(
    glc_registry_select_action_id(repository),
    paste0(
      "select_repository_",
      paste0(
        sprintf("%02x", as.integer(charToRaw(repository))),
        collapse = ""
      )
    )
  )
  expect_false(identical(
    glc_registry_select_action_id(repository),
    glc_registry_select_action_id("fixture/warning")
  ))
  expect_identical(
    glc_registry_revision_url(repository, revision),
    paste0(
      "https://github.com/fixture/validated/tree/",
      tolower(revision)
    )
  )
  expect_error(
    glc_registry_revision_url(repository, "main"),
    class = "llw_validation_error",
    regexp = "exact 40-character"
  )

  registry <- glc_source_test_registry()
  html <- htmltools::renderTags(glc_registry_table_tag(
    registry,
    selected_repository = "fixture/warning",
    ns = function(id) paste0("glc-", id)
  ))$html
  expect_match(html, ">Select</th>.*>GitHub</th>")
  expect_match(html, "data-selected=\"true\"", fixed = TRUE)
  expect_match(html, "fa-check", fixed = TRUE)
  expect_match(html, "Selected", fixed = TRUE)
  expect_equal(
    length(strsplit(html, "llw-glc-select-button", fixed = TRUE)[[1L]]) - 1L,
    nrow(registry)
  )
  expect_false(grepl("btn-outline-primary", html, fixed = TRUE))
  expect_match(html, "aria-label=\"Select fixture/validated\"", fixed = TRUE)
  expect_match(
    html,
    paste0(
      "https://github.com/fixture/validated/tree/",
      strrep("a", 40L)
    ),
    fixed = TRUE
  )
  expect_match(html, "noopener noreferrer", fixed = TRUE)
  expect_false(grepl(
    "https://github.com/fixture/no-pass",
    html,
    fixed = TRUE
  ))
  expect_match(html, "No passing revision is available", fixed = TRUE)
})

test_that("GLC source module preselects one exact deep link without data access", {
  calls <- new.env(parent = emptyenv())
  calls$loads <- 0L
  calls$searches <- 0L
  loader <- function(registry_url, refresh = FALSE) {
    calls$loads <- calls$loads + 1L
    glc_source_test_registry()
  }
  searcher <- function(packages, query = NULL) {
    calls$searches <- calls$searches + 1L
    glc_source_test_search(packages, query)
  }

  shiny::testServer(
    glcSourceServer,
    args = list(
      registry_url = "fixture://registry",
      raw_query = "?repo=fixture%2Fvalidated",
      registry_loader = loader,
      registry_search = searcher
    ),
    {
      returned <- session$getReturned()
      expect_identical(returned$status()$state, "queued")
      expect_identical(returned$status()$heading, "Loading registry")
      initialization <- new.env(parent = emptyenv())
      later::later(
        function() {
          initialization$result <- tryCatch(
            returned$initialize(),
            error = identity
          )
        },
        delay = 0
      )
      later::run_now(timeoutSecs = 0.1)
      expect_true(exists("result", initialization, inherits = FALSE))
      expect_false(inherits(initialization$result, "error"))
      session$flushReact()

      expect_equal(calls$loads, 1L)
      expect_identical(returned$deep_link()$state, "matched")
      expect_identical(returned$selection()$repository, "fixture/validated")
      expect_identical(returned$selection()$resolved_revision, strrep("a", 40L))
      expect_true(returned$selection()$has_latest_pass)
      expect_identical(returned$status()$state, "complete")
      expect_null(returned$error())
      table_html <- htmltools::renderTags(output$registry_table)$html
      expect_match(table_html, "data-selected=\"true\"", fixed = TRUE)
      expect_match(table_html, "fa-check", fixed = TRUE)
      expect_match(table_html, "Selected", fixed = TRUE)
      status_html <- htmltools::renderTags(output$registry_status)$html
      expect_match(status_html, "Registry ready", fixed = TRUE)
      expect_match(
        status_html,
        "The exact registry repository was preselected",
        fixed = TRUE
      )
      expect_equal(
        stringr::str_count(status_html, 'class="llw-status '),
        1L
      )

      session$setInputs(search = "warning")
      session$flushReact()
      expect_equal(nrow(returned$filtered_registry()), 1L)
      expect_identical(
        returned$filtered_registry()$repository,
        "fixture/warning"
      )
      expect_gte(calls$searches, 1L)
    }
  )
})

test_that("GLC source module parses the launch query only once", {
  query <- shiny::reactiveVal("?repo=fixture%2Fvalidated")

  shiny::testServer(
    glcSourceServer,
    args = list(
      registry_url = "fixture://registry",
      raw_query = query,
      registry_loader = function(registry_url, refresh = FALSE) {
        glc_source_test_registry()
      },
      registry_search = glc_source_test_search
    ),
    {
      returned <- session$getReturned()
      returned$initialize()
      session$flushReact()
      expect_identical(returned$deep_link()$state, "matched")

      query("?repo=fixture%2Fwarning")
      session$flushReact()
      expect_identical(returned$deep_link()$state, "matched")
      expect_identical(returned$selection()$repository, "fixture/validated")
    }
  )
})

test_that("GLC source controller switches fresh and deep-link review paths", {
  calls <- new.env(parent = emptyenv())
  calls$loads <- 0L

  shiny::testServer(
    glcSourceServer,
    args = list(
      registry_url = "fixture://registry",
      raw_query = "",
      registry_loader = function(registry_url, refresh = FALSE) {
        calls$loads <- calls$loads + 1L
        glc_source_test_registry()
      },
      registry_search = glc_source_test_search
    ),
    {
      returned <- session$getReturned()
      returned$initialize()
      session$flushReact()
      expect_null(returned$selection())
      expect_identical(returned$deep_link()$state, "absent")

      expect_true(returned$apply_deep_link(
        "?repo=fixture%2Fvalidated"
      ))
      session$flushReact()
      expect_identical(returned$selection()$repository, "fixture/validated")
      expect_identical(returned$deep_link()$state, "matched")
      expect_equal(calls$loads, 1L)

      returned$clear_selection()
      session$flushReact()
      expect_null(returned$selection())
      expect_identical(returned$deep_link()$state, "absent")

      expect_false(returned$apply_deep_link(
        "?repo=fixture%252Fvalidated"
      ))
      session$flushReact()
      expect_null(returned$selection())
      expect_identical(returned$deep_link()$state, "invalid")
      expect_equal(calls$loads, 1L)
    }
  )
})

test_that("invalid and unmatched deep links remain recoverable preselection states", {
  cases <- list(
    invalid = "?repo=fixture%252Fvalidated",
    unmatched = "?repo=fixture%2Fmissing"
  )
  for (expected_state in names(cases)) {
    shiny::testServer(
      glcSourceServer,
      args = list(
        registry_url = "fixture://registry",
        raw_query = cases[[expected_state]],
        registry_loader = function(registry_url, refresh = FALSE) {
          glc_source_test_registry()
        },
        registry_search = glc_source_test_search
      ),
      {
        returned <- session$getReturned()
        returned$initialize()
        session$flushReact()
        expect_identical(returned$deep_link()$state, expected_state)
        expect_null(returned$selection())
        expect_identical(returned$status()$state, "complete")
      }
    )
  }
})

test_that("a selected package without a passing revision cannot proceed", {
  shiny::testServer(
    glcSourceServer,
    args = list(
      registry_url = "fixture://registry",
      raw_query = "",
      registry_loader = function(registry_url, refresh = FALSE) {
        glc_source_test_registry()
      },
      registry_search = glc_source_test_search
    ),
    {
      returned <- session$getReturned()
      returned$initialize()
      session$flushReact()
      action_id <- glc_registry_select_action_id("fixture/no-pass")
      do.call(session$setInputs, stats::setNames(list(1L), action_id))
      session$flushReact()

      expect_identical(returned$selection()$repository, "fixture/no-pass")
      expect_false(returned$selection()$has_latest_pass)
      expect_true(is.na(returned$selection()$resolved_revision))
      expect_match(
        htmltools::renderTags(output$selection_detail)$html,
        "No fallback revision will be used",
        fixed = TRUE
      )
    }
  )
})

test_that("stable row actions survive search, reorder, and removal once", {
  calls <- new.env(parent = emptyenv())
  calls$loads <- 0L
  calls$selections <- 0L
  orders <- list(
    1:3,
    c(3L, 1L, 2L),
    c(2L, 3L, 1L),
    c(1L, 3L)
  )
  loader <- function(registry_url, refresh = FALSE) {
    calls$loads <- calls$loads + 1L
    registry <- glc_source_test_registry()
    result <- registry[orders[[calls$loads]], , drop = FALSE]
    class(result) <- class(registry)
    result
  }
  selector <- function(packages, repository, registry_url) {
    calls$selections <- calls$selections + 1L
    glc_registry_select_repository(packages, repository, registry_url)
  }

  shiny::testServer(
    glcSourceServer,
    args = list(
      registry_url = "fixture://registry",
      raw_query = "",
      registry_loader = loader,
      registry_search = glc_source_test_search,
      registry_select = selector
    ),
    {
      returned <- session$getReturned()
      returned$initialize()
      session$flushReact()
      warning_action <- glc_registry_select_action_id("fixture/warning")
      validated_action <- glc_registry_select_action_id("fixture/validated")

      session$setInputs(search = "warning")
      session$flushReact()
      expect_identical(
        returned$filtered_registry()$repository,
        "fixture/warning"
      )
      do.call(session$setInputs, stats::setNames(list(1L), warning_action))
      session$flushReact()
      expect_identical(calls$selections, 1L)
      expect_identical(returned$selection()$repository, "fixture/warning")

      session$setInputs(refresh = 1L)
      session$flushReact()
      expect_identical(calls$selections, 2L)
      expect_identical(returned$selection()$repository, "fixture/warning")
      do.call(session$setInputs, stats::setNames(list(2L), warning_action))
      session$flushReact()
      expect_identical(calls$selections, 3L)

      session$setInputs(search = "validated")
      session$flushReact()
      session$setInputs(search = "warning")
      session$flushReact()
      session$setInputs(refresh = 2L)
      session$flushReact()
      expect_identical(calls$selections, 4L)
      do.call(session$setInputs, stats::setNames(list(3L), warning_action))
      session$flushReact()
      expect_identical(calls$selections, 5L)

      session$setInputs(refresh = 3L)
      session$flushReact()
      expect_identical(calls$selections, 6L)
      expect_null(returned$selection())
      expect_equal(nrow(returned$filtered_registry()), 0L)

      do.call(session$setInputs, stats::setNames(list(4L), warning_action))
      session$flushReact()
      expect_identical(calls$selections, 6L)
      expect_null(returned$selection())

      session$setInputs(search = "")
      session$flushReact()
      do.call(session$setInputs, stats::setNames(list(1L), validated_action))
      session$flushReact()
      expect_identical(calls$selections, 7L)
      expect_identical(returned$selection()$repository, "fixture/validated")
      expect_identical(calls$loads, 4L)
    }
  )
})

test_that("failed registry refresh preserves the last successful view", {
  calls <- new.env(parent = emptyenv())
  calls$fail <- FALSE
  calls$loads <- 0L
  loader <- function(registry_url, refresh = FALSE) {
    calls$loads <- calls$loads + 1L
    if (refresh && calls$fail) {
      stop(structure(
        list(message = "private fixture failure", call = NULL),
        class = c("glcdp_registry_error", "error", "condition")
      ))
    }
    glc_source_test_registry()
  }

  shiny::testServer(
    glcSourceServer,
    args = list(
      registry_url = "fixture://registry",
      raw_query = "?repo=fixture%2Fvalidated",
      registry_loader = loader,
      registry_search = glc_source_test_search
    ),
    {
      returned <- session$getReturned()
      returned$initialize()
      session$flushReact()
      before <- returned$registry()
      selected <- returned$selection()

      calls$fail <- TRUE
      session$setInputs(refresh = 1)
      session$flushReact()

      expect_equal(calls$loads, 2L)
      expect_identical(returned$registry(), before)
      expect_identical(returned$selection(), selected)
      expect_identical(returned$status()$state, "warning")
      expect_s3_class(returned$error(), "glcdp_registry_error")
      expect_false(grepl(
        "private fixture failure",
        returned$status()$message,
        fixed = TRUE
      ))
    }
  )
})

test_that("GLC source UI states that selection performs no data transfer", {
  html <- htmltools::renderTags(glcSourceUI("glc"))$html
  expect_match(html, "Discover validated GLC packages", fixed = TRUE)
  expect_match(
    html,
    "never reads, downloads, or creates a dataset",
    fixed = TRUE
  )
  expect_match(html, "Refresh registry", fixed = TRUE)
  expect_match(html, "Search registry", fixed = TRUE)
  expect_false(grepl("Selected source", html, fixed = TRUE))
  expect_false(grepl("deep_link_status", html, fixed = TRUE))
  expect_match(html, "llw-glc-registry-controls", fixed = TRUE)
  expect_match(html, "llw-glc-registry-table-scroll", fixed = TRUE)
  expect_match(html, "overflow-x: auto; overflow-y: hidden", fixed = TRUE)
  expect_match(html, "btn-primary llw-glc-refresh-button", fixed = TRUE)
  expect_match(html, "height: 2.75rem", fixed = TRUE)
  expect_false(grepl("btn-outline-secondary", html, fixed = TRUE))
  expect_false(grepl("Selected repository", html, fixed = TRUE))
  expect_lt(
    regexpr("Search registry", html, fixed = TRUE)[[1L]],
    regexpr("glc-registry_table", html, fixed = TRUE)[[1L]]
  )
  expect_lt(
    regexpr("Refresh registry", html, fixed = TRUE)[[1L]],
    regexpr("glc-registry_table", html, fixed = TRUE)[[1L]]
  )
})
