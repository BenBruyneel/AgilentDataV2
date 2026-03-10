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
                          theTargets,
                          insideTagsDiv = NA) {
  ns <- NS(id)
  result <- tagList(
    shiny::tabsetPanel(
      id = ns("Top"),
      shiny::tabPanel(
        title = "Select",
        icon = shiny::icon("clipboard-check"),
        filesTableUI(id = ns("fileTable"),
                     insideTagsDiv = insideTagsDiv)
      ),
      shiny::tabPanel(
        title = "Processing",
        icon = shiny::icon("bolt"),
        processUI(id = ns("process"),
                  admin = admin,
                  insideTagsDiv = insideTagsDiv)
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
agilentDataServer <- function(id = "AgilentData",
                              theTargets,
                              admin = TRUE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    filesTable <- filesTableServer(id = "fileTable")
    
    processing <- processServer(id = "process",
                                admin = admin,
                                theTargets = reactive({theTargets}),
                                toProcess = filesTable)
    
  })
}
