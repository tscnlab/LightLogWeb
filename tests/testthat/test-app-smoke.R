test_that("the packaged application can be constructed", {
  app <- LightLogWeb(profile = "hosted", workers = 0)

  expect_s3_class(app, "shiny.appobj")
  expect_type(app$httpHandler, "closure")
  expect_type(app$serverFuncSource, "closure")
  expect_identical(
    names(formals(LightLogWeb)),
    c("profile", "max_upload_mb", "workers")
  )
})

test_that("every changed module retains a constructible development app", {
  apps <- list(
    core_architecture_app(),
    dataset_manager_app(),
    dataset_dashboard_app(),
    import_app()
  )

  expect_true(all(vapply(apps, inherits, logical(1), "shiny.appobj")))
})
