import_modal_successfull <- function(data) {
  groups <- data |> dplyr::group_keys() |> nrow()
  rows <-
    nrow(data) |>
    unclass() |>
    formatC(decimal.mark=".", big.mark=" ", digits = 0, format = "f")

  showModal(
    modalDialog(
      title = icon("check", style = "font-size: 60px;"),
      easyClose = TRUE,
      strong("Import successful!"),
      p(glue::glue("{groups} groups were imported, totaling {rows} observations.")),
      p("Please check the import message and overview plot and continue to the analysis tab if satisfied.")
    ),
  )
}
