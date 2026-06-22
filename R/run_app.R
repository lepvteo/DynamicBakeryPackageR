# -----
# Launch the Shiny application
# -----

#' Launch the DynamicBakeryPackageR Shiny application
#'
#' Starts the interactive dynamic pricing simulation dashboard.
#'
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "DynamicBakeryPackageR")
  shiny::runApp(app_dir)
}