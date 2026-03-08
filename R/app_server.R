#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  targetFilesName <<- "inst/app/data/targetsList.csv"
  targetFiles <<- read.table(targetFilesName,
                             sep = ";",
                             header = T)
}
