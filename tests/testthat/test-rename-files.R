test_that("rename_files falls back to copying when rename fails", {
  src <- tempfile(fileext = ".csv")
  writeLines("x", src)

  file <- data.frame(
    datapath = src,
    name = "original.csv",
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    file.rename = function(from, to) FALSE
  )

  out <- rename_files(file)

  expect_true(file.exists(out))
  expect_false(file.exists(src))
})
