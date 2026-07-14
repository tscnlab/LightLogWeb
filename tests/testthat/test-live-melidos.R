test_that("pinned live MeLiDos integration matches the fixture schema", {
  skip_if_not(Sys.getenv("LIGHTLOGWEB_LIVE_MELIDOS") == "true", "live MeLiDos tests are opt-in")
  skip_if_not_installed("melidosData")
  live <- melidosData::load_data("light_glasses_1minute", site = "TUM")
  expect_true(all(c("Id", "Datetime", "MEDI", "position") %in% names(live)))
  imported <- llw_import(data = utils::head(live, 1440), variable = "MEDI", timezone = "Europe/Berlin")
  expect_s3_class(imported, "llw_dataset")
  expect_s3_class(llw_quality(imported), "llw_result")
})
