#' @export
runEntryGameApp <- function() {
  appDir <- system.file("shiny-examples", "entryGameApp", package = "marketGames")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `marketGames`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
