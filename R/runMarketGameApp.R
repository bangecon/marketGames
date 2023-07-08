##' Run a `marketGames` app.
##'
##' This is a wrapper function for the game-specific `run_Game_App()` functions in the marketGames package.
##'
##' @param game  (required) the name of the `marketGames` app you want to run, which can be one of:
##'   1. `equlibriumGame` to tabulate results for a simple supply-and-demand equilibrium game.
##'   2. `entryGame` for a two-sector entry-and-exit game.
##'   3. `bertrandGame` for a Bertrand duopoly game.
##'   4. `cournotGame` for a Cournot duopoly game.
##'   5. `stackelbergGame` for a sequential leader-follower game.
##'   6. `multipdGame` for a multi-player prisoner's dilemma game.
##'   7. `staghuntGame` for a two-person stag hunt gmae.
##'
##' @export

runMarketGameApp <- function() {
  appDir <- system.file("shiny-examples", "equilibriumGameApp", package = "marketGames")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `marketGames`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
