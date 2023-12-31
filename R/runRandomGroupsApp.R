#' @export
runRandomGroupsApp <- function() {
  appDir <- system.file("shiny-examples", "randomGroupsApp", package = "marketGames")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `marketGames`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
