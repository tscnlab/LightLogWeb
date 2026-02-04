import_data <- function(input, Id_preview, new_names) {

  import_msg <- character()
  imported_data <- NULL

  conditional_arg <-
    list(print_n = Inf)
  if(input$device == "VEET") {
    conditional_arg <-
      append(conditional_arg, list(modality = input$veet_modality))
  }
  conditional_arg <-
    if(input$id == "manual") {
      append(conditional_arg, list(manual.id = input$Id_manual))
    } else if(input$id == "extract"){
      append(conditional_arg, list(auto.id = input$Id_extract))
    } else {
      append(conditional_arg,
             list(auto.id = paste0(".{",nchar(Id_preview()[1])  ,"}"))
      )
    }

  rlang::inject({
    import_msg <- capture.output({
      tryCatch({
        imported_data <- import_Dataset(
          device = input$device,
          filename = new_names(),
          tz = input$tz,
          not.before = input$not_before,
          dst_adjustment = "dst_jumps" %in% input$options,
          remove_duplicates = "remove_duplicates" %in% input$options,
          auto.plot = FALSE,
          version = input$version,
          !!!conditional_arg
        )
      }, error = \(x) {
        cat("\nImport failed.\nPlease check files and settings.\nDetailed error message:\n")
        x
      }
      )
    })
  })

  #adjust import string if necessary
  if (length(import_msg) > 0 && startsWith(import_msg[1], "\r")) {
    import_msg <- import_msg[-1]
  }

  list(
    data = imported_data,
    msg = import_msg
  )

}
