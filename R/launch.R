#' Launch a shiny app that lets you do the data matching interactively
#' @export
#' @importFrom shiny runApp
launch <- function() {
  shiny::runApp(system.file("shiny", package = "epimatch"),
                display.mode = "normal",
                launch.browser = TRUE)
}
