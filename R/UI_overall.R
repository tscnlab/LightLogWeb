#defining the main body
footer <-
  shiny::tags$footer(
    shiny::a(
      shiny::tags$image(src='extr/logo_banner2.png', width = "60%"),
      href = "https://www.melidos.eu", target="_blank"),
    shiny::br(), shiny::br(),
    style = "text-align:center;"
  )
