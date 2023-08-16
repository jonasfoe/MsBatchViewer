#' MsBatchViewer
#'
#' @import future
#' @import purrr
#' @import ggplot2
#' @import tidyr
#' @import dplyr
#' @import shiny
#' @import shinydashboard
#' @import shinyFiles
#' @import shinyjqui
#' @import stringr
#' @importFrom rlang .data
"_PACKAGE"

#' @export
MsBatchViewer <- function(...) {
  # options(shiny.trace = TRUE)
  # options(shiny.reactlog = TRUE)

  shinyOptions(shiny.useragg = TRUE)

  shinyApp(ui = shinyAppUI(), server = shinyAppServer, ...)
}
