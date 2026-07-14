test_that("metric catalog is curated, linked, and complete", {
  registry <- llw_metric_registry()
  expect_equal(nrow(registry), 15)
  expect_true(all(grepl("^https://tscnlab.github.io/LightLogR/reference/.+\\.html$", registry$documentation)))
  expect_true(all(c("defaults", "requirement", "units", "grouping", "caution") %in% names(registry)))
  expect_false(any(c("nvRC", "nvRD", "spectral") %in% registry$id))
})

test_that("every metric wrapper agrees with direct LightLogR output", {
  data <- sample_participant()
  dataset <- llw_dataset(data)
  registry <- llw_metric_registry()

  for (id in registry$id) {
    definition <- registry[registry$id == id, , drop = FALSE]
    fn <- getExportedValue("LightLogR", id)
    args <- definition$defaults[[1]]
    args[[definition$light_arg[[1]]]] <- data$MEDI
    time_arg <- definition$time_arg[[1]]
    if (!is.na(time_arg)) args[[time_arg]] <- data$Datetime
    if ("as.df" %in% names(formals(fn))) args$as.df <- TRUE
    direct <- rlang::exec(fn, !!!args)
    wrapped <- llw_metrics(dataset, id)$value[[id]]
    if (is.data.frame(direct)) {
      expect_equal(wrapped, tibble::as_tibble(direct), ignore_attr = TRUE, info = id)
    } else {
      expect_equal(wrapped[[id]], unname(direct), ignore_attr = TRUE, info = id)
      expect_equal(wrapped$Datetime, data$Datetime, ignore_attr = TRUE, info = id)
    }
  }
})

test_that("published sample.data.environment baselines are preserved", {
  data <- LightLogR::sample.data.environment
  dataset <- llw_group(llw_dataset(data), dimensions = "participant")
  result <- llw_metrics(dataset, c("duration_above_threshold", "dose", "intradaily_variability", "interdaily_stability"))$value
  participant <- function(table) table[as.character(table$Id) == "Participant", , drop = FALSE]

  expect_equal(as.numeric(participant(result$duration_above_threshold)[[2]], units = "hours"), 27.3667, tolerance = 1e-4)
  expect_equal(participant(result$dose)[[2]], 107369.9, tolerance = 0.1)
  expect_equal(participant(result$intradaily_variability)[[2]], 0.5176546, tolerance = 1e-7)
  expect_equal(participant(result$interdaily_stability)[[2]], 0.3215539, tolerance = 1e-7)
})

test_that("metric requests validate parameters and duplicates", {
  dataset <- llw_dataset(sample_participant())
  expect_error(llw_metrics(dataset, list(list(id = "dose", parameters = list(not_real = 1)))), "Unknown parameters")
  expect_error(llw_metrics(dataset, c("dose", "dose")), "requested more than once")
  expect_error(llw_metrics(dataset, "spectral_integration"), "Unknown metric")
})
