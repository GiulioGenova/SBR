#' @export
run_app <- function(app) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shinyApps", package = "SBR"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(app) || !nzchar(app) ||
      !app %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shinyApps", app, package = "SBR")
  shiny::runApp(appDir, display.mode = "normal",launch.browser = T)
}
