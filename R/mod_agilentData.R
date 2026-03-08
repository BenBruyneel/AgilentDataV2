#' agilentData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
agilentDataUI <- function(id = "AgilentData",
                          admin = TRUE,
                          theTargets = targetFiles,
                          insideTagsDiv = NA) {
  ns <- NS(id)
  result <- tagList(
    shiny::tabsetPanel(
      id = ns("Top"),
      shiny::tabPanel(
        title = "Select",
        icon = shiny::icon("clipboard-check")
        # filesTableUI(id = ns("fileTable"), insideTagsDiv = insideTagsDiv)
      ),
      shiny::tabPanel(
        title = "Processing",
        icon = shiny::icon("bolt")
        # processUI(id = ns("process"),
        #           admin = admin,
        #           insideTagsDiv = insideTagsDiv)
      ),
      shiny::tabPanel(
        title = "Results",
        icon = shiny::icon("table-list"),
        # review2UI(id = ns("results"), insideTagsDiv = insideTagsDiv)
      )
    )
  )
  return(result)
}
    
#' agilentData Server Functions
#'
#' @noRd 
agilentDataServer <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_agilentData_ui("agilentData_1")
    
## To be copied in the server
# mod_agilentData_server("agilentData_1")
