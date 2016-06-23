#' Launch a shiny app that lets you do the data matching interactively
#' @export
launch <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Install 'shiny' via 'install.packages(\"shiny\")' to run this function.",
         call. = FALSE)
  }
  if (!requireNamespace("shinyjs", quietly = TRUE)) {
    stop("Install 'shiny' via 'install.packages(\"shinyjs\")' to run this function.",
         call. = FALSE)
  }
  shiny::runApp(system.file("shiny", package = "epimatch"),
                display.mode = "normal",
                launch.browser = TRUE)
}
