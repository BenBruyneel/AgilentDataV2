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

