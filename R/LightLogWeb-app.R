#' Bring LightLogR to the web with shiny
#'
#' @returns Open a viewer with the shiny app
#' @export
#'
#' @examples
#' if(interactive()) {
#' LightLogWeb()
#' }
LightLogWeb <- function(...) {

  #setting the upload limit to 100 MB
  options(shiny.maxRequestSize=100*1024^2)
  #add a resource path to the www folder
  shiny::addResourcePath(
    "extr", system.file("app/www", package = "LightLogWeb"))
  # on.exit(shiny::removeResourcePath("extr"), add = TRUE)

  ui <- bslib::page_navbar(
    # App title ----
    title = shiny::h1("LightLogWeb"),
    footer = footer,
    selected = "Import",
    fillable = FALSE,
    bslib::nav_spacer(),
    bslib::nav_panel("Import",
                     importUI("import")
    )
  )

  # Server ------------------------------------------------------------------

  #Server
  server <- function(input, output, session) {

    #allow reconnect
    session$allowReconnect(TRUE)

    #Import
    light <- importServer("import")

    #close the waiting screen
    # waiter::waiter_hide()

  }

  # App ---------------------------------------------------------------------
  shiny::shinyApp(ui, server, ...)
}

