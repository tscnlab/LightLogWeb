test_that("dataset manager returns explicit stable-ID events", {
  first <- m1_record("First")
  second <- m1_record("Second")
  records <- stats::setNames(list(first, second), c(first$id, second$id))
  datasets <- shiny::reactiveVal(records)
  selected <- shiny::reactiveVal(first$id)

  shiny::testServer(
    datasetManagerServer,
    args = list(datasets = datasets, selected_dataset_id = selected),
    {
      returned <- session$getReturned()
      session$setInputs(
        import_newdata = 0,
        import_testdata = 0,
        dataset_select = first$id,
        rename_dataset_real = 0,
        delete_dataset_real = 0
      )
      session$setInputs(import_newdata = 1)
      expect_identical(returned$event()$type, "open_import")

      session$setInputs(dataset_select = second$id)
      expect_identical(returned$event()$type, "select")
      expect_identical(returned$event()$dataset_id, second$id)

      session$setInputs(rename_name = "Renamed", rename_dataset_real = 1)
      expect_identical(returned$event()$type, "rename")
      expect_identical(returned$event()$dataset_id, first$id)
      expect_identical(returned$event()$value, "Renamed")

      session$setInputs(delete_dataset_real = 1)
      expect_identical(returned$event()$type, "remove")
      expect_identical(returned$event()$dataset_id, first$id)
    }
  )
})

test_that("dashboard communicates its empty-state action through an event", {
  dataset <- shiny::reactiveVal(NULL)
  panel <- shiny::reactiveVal("dashboard")

  shiny::testServer(
    datasetDashboardServer,
    args = list(dataset = dataset, active_panel = panel),
    {
      returned <- session$getReturned()
      session$setInputs(to_import = 0)
      session$setInputs(to_import = 1)
      expect_identical(returned$event()$type, "open_import")

      dataset(m1_record())
      session$flushReact()
      expect_identical(output$raw_rows, "3")
      expect_identical(output$prepared_rows, "3")
    }
  )
})

test_that("import validation failures are returned as recoverable task state", {
  runtime <- new_session_runtime(
    resolve_runtime_profile("hosted", workers = 0),
    session = NULL
  )
  on.exit(runtime$cleanup(), add = TRUE)

  shiny::testServer(importServer, args = list(runtime = runtime), {
    returned <- session$getReturned()
    session$setInputs(
      device = "",
      tz = "UTC",
      dataset_name = "",
      id = "automated",
      not_before = as.Date("2001-01-01"),
      options = character(),
      version = "",
      import = 0
    )
    session$setInputs(import = 1)
    session$flushReact()

    expect_identical(returned$status()$state, "error")
    expect_s3_class(returned$error(), "llw_validation_error")
    expect_match(
      llw_public_message(returned$error()),
      "Choose files"
    )

    session$setInputs(import = 2)
    session$flushReact()
    expect_identical(returned$status()$state, "error")
  })
})
