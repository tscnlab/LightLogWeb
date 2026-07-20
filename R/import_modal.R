import_success_summary <- function(data) {
  if (!is.data.frame(data)) {
    abort_llw("Imported results must be a data frame.", type = "import")
  }
  groups <- if (dplyr::is_grouped_df(data)) {
    dplyr::n_groups(data)
  } else if ("Id" %in% names(data)) {
    length(unique(data$Id))
  } else if (nrow(data) > 0L) {
    1L
  } else {
    0L
  }
  list(groups = groups, rows = nrow(data))
}

show_import_success_modal <- function(data) {
  summary <- import_success_summary(data)
  rows <- formatC(
    summary$rows,
    decimal.mark = ".",
    big.mark = " ",
    digits = 0,
    format = "f"
  )

  showModal(
    modalDialog(
      title = "Import complete",
      easyClose = TRUE,
      llw_status_callout(
        "complete",
        paste0(
          summary$groups,
          " group(s) and ",
          rows,
          " observations are ready for review."
        ),
        heading = "Import successful"
      ),
      p(
        class = "mt-3 mb-0",
        paste(
          "Review the import message, overview plot, and preview table before",
          "adding this dataset to the session."
        )
      )
    )
  )
}
