test_that("the packaged application can be constructed", {
  app <- LightLogWeb(profile = "hosted", workers = 0)

  expect_s3_class(app, "shiny.appobj")
  expect_type(app$httpHandler, "closure")
  expect_type(app$serverFuncSource, "closure")
  expect_identical(
    names(formals(LightLogWeb)),
    c("profile", "max_upload_mb", "workers")
  )

  app_environment <- environment(app$serverFuncSource())
  production_html <- htmltools::renderTags(app_environment$ui)$html
  expect_identical(app_environment$import_presentation, "wizard")
  expect_match(production_html, "llw-import-wizard", fixed = TRUE)
  expect_match(production_html, "Append datasets safely", fixed = TRUE)
  expect_match(production_html, "Ready-to-use example", fixed = TRUE)
  expect_false(grepl("import_accordion", production_html, fixed = TRUE))
})

test_that("every changed module retains a constructible development app", {
  apps <- list(
    core_architecture_app(),
    dataset_manager_app(),
    dataset_dashboard_app(),
    append_merge_app(),
    import_app(),
    import_wizard_app()
  )

  expect_true(all(vapply(apps, inherits, logical(1), "shiny.appobj")))
  expect_identical(formals(import_app)$workers, 1)
})

test_that("upload apps apply their configured request limit when they start", {
  main_app <- LightLogWeb(
    profile = "hosted",
    max_upload_mb = 16,
    workers = 0
  )
  showcase <- import_app(max_upload_mb = 16)

  expect_type(main_app$onStart, "closure")
  expect_type(showcase$onStart, "closure")
  expect_equal(
    environment(main_app$onStart)$max_upload_bytes,
    16 * 1024^2
  )
  expect_equal(
    environment(showcase$onStart)$max_upload_bytes,
    16 * 1024^2
  )
})
