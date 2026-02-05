test_that("resolve_www_path uses local fallback directories", {
  tmp <- tempfile("lightlogweb-")
  dir.create(tmp)
  dir.create(file.path(tmp, "inst", "app", "www"), recursive = TRUE)

  expect_equal(
    resolve_www_path(base_path = tmp),
    file.path(tmp, "inst", "app", "www")
  )
})

test_that("resolve_www_path errors when no path is available", {
  tmp <- tempfile("lightlogweb-empty-")
  dir.create(tmp)

  expect_error(
    resolve_www_path(base_path = tmp),
    "Could not find LightLogWeb web resources"
  )
})
