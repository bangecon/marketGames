#' @export
runMultipdGameGameApp <- function() {
  appDir <- system.file("shiny-examples", "multipdGameApp", package = "marketGames")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `marketGames`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
