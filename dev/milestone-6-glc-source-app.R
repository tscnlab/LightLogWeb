pkgload::load_all(".", export_all = TRUE, quiet = TRUE)

glc_source_showcase_registry_url <- function() {
  "https://tscnlab.github.io/glc-registry/registry.json"
}

glc_source_showcase_app <- function() {
  registry_url <- glc_source_showcase_registry_url()

  ui <- lightlogweb_page(page_fluid(
    lightlogweb_head(),
    lightlogweb_skip_link(),
    tags$main(
      id = "llw-main-content",
      class = "llw-main-shell",
      tabindex = "-1",
      llw_view_header(
        "Milestone 6 · Slice 1",
        "Live GLC registry and exact repository selection",
        paste(
          "This standalone review app reads the current public GLC registry",
          "through the pinned glcdp 1.0.0 API. Discovery, search, and source",
          "selection do not open packages or transfer package data."
        )
      ),
      card(
        card_header("Live registry connection"),
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
            "The registry is loaded when this page starts. Its records and",
            "row count are live and may change independently of this app."
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
            "After a successful load, enable the simulation and use Refresh",
            "registry below. The last successful registry and selection remain",
            "visible. Disable it and refresh again to retry the live endpoint."
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
      glcSourceUI("glc_source")
    ),
    theme = lightlogweb_theme()
  ))

  server <- function(input, output, session) {
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

    source_state <- glcSourceServer(
      "glc_source",
      registry_url = registry_url,
      registry_loader = registry_loader
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
