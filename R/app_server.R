#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  targetFiles <- utils::read.table(targetFilesName,
                                    sep = ";",
                                    header = T)
  agilentData <- agilentDataServer(admin = TRUE,
                                   theTargets = targetFiles)
}
