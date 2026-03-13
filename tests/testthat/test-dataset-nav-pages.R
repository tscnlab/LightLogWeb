test_that("LightLogWeb:::new_dataset_page creates stable identifiers", {
  page <- LightLogWeb:::new_dataset_page("example", 4)

  expect_equal(page$nav_value, "dataset_4")
  expect_equal(page$module_id, "dashboard_dataset_4")
  expect_equal(page$dataset_name(), "example")

  page$dataset_name("renamed")
  expect_equal(page$dataset_name(), "renamed")
})

test_that("LightLogWeb:::reconcile_dataset_pages detects added and removed datasets", {
  pages <- list(alpha = list(), beta = list())

  page_diff <- LightLogWeb:::reconcile_dataset_pages(
    pages = pages,
    dataset_names = c("beta", "gamma")
  )

  expect_equal(page_diff$added, "gamma")
  expect_equal(page_diff$removed, "alpha")
  expect_null(page_diff$renamed)
})

test_that("LightLogWeb:::reconcile_dataset_pages detects single rename", {
  pages <- list(alpha = list())

  page_diff <- LightLogWeb:::reconcile_dataset_pages(
    pages = pages,
    dataset_names = "beta",
    selected_dataset = "beta"
  )

  expect_equal(page_diff$added, "beta")
  expect_equal(page_diff$removed, "alpha")
  expect_equal(page_diff$renamed$from, "alpha")
  expect_equal(page_diff$renamed$to, "beta")
})
