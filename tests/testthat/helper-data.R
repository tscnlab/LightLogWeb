make_regular_light <- function(start = "2025-01-01 00:00:00",
                               days = 2,
                               epoch = 60,
                               tz = "UTC",
                               id = "P01") {
  first <- as.POSIXct(start, tz = tz)
  last <- first + lubridate::days(days)
  datetime <- seq(first, last - epoch, by = epoch)
  tibble::tibble(
    Id = id,
    Datetime = datetime,
    MEDI = pmax(0, 500 * sin(2 * pi * (as.numeric(format(datetime, "%H", tz = tz)) - 6) / 24))
  )
}

sample_participant <- function() {
  data <- LightLogR::sample.data.environment
  data[as.character(data$Id) == "Participant", , drop = FALSE]
}

example_module <- function(version = "1.0.0", required_columns = c("Id", "Datetime", "MEDI")) {
  llw_module(
    id = "daily_mean",
    title = "Daily mean",
    version = version,
    ui = function(id) shiny::NS(id)("value"),
    server = function(id, context) invisible(NULL),
    validate = function(data, metadata, params) is.numeric(if (is.null(params$offset)) 0 else params$offset),
    run = function(data, metadata, params) {
      value <- mean(data[[metadata$variable]], na.rm = TRUE) + if (is.null(params$offset)) 0 else params$offset
      llw_result("scalar", value, label = "Daily mean", units = metadata$variable_unit, parameters = params, source = "daily_mean")
    },
    code = function(params) paste0("mean(data[[metadata$variable]], na.rm = TRUE) + ", if (is.null(params$offset)) 0 else params$offset),
    required_columns = required_columns,
    docs_url = "https://example.org/daily-mean"
  )
}
