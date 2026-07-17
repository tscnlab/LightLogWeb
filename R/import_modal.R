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
      title = icon("check", style = "font-size: 60px;"),
      easyClose = TRUE,
      strong("Import successful!"),
      p(
        summary$groups,
        " group(s) were imported, totaling ",
        rows,
        " observations."
      ),
      p(
        "Please check the import message and overview plot and continue to the analysis tab if satisfied."
      )
    ),
  )
}
