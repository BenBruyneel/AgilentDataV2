#' filesTable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
filesTableUI <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' filesTable Server Functions
#'
#' @noRd 
filesTableServer <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_filesTable_ui("filesTable_1")
    
## To be copied in the server
# mod_filesTable_server("filesTable_1")
