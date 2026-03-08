#' filesSelectDefinition UI Function
#'
#' @description A shiny Module for (de-)selecting file names (character vectors
#'  to more precisely)
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param insideTagsDiv character vector that enables embedding the element
#'  'inside' a certain css class, default is NA
#' @param name character vector, name of the selection
#' @param label character vector, name to show in UI
#' @param defaultPositive character vector, default value for selection
#' @param defaultNegative character vector, default value for deselection
#' @param checked logical vector defines whether the (de-)selection is active
#' @param width character vector, width of the UI element 
#'
#' @noRd 
filesSelectDefinitionUI <- function(id = "msfiles",
                                    insideTagsDiv = NA,
                                    name = "msPeakFilesStrings",
                                    label = "MS peak files",
                                    defaultPositiveValue = "_MS.csv$",
                                    defaultNegativeValue = "",
                                    checked = TRUE,
                                    width = "150px"){
  ns <- NS(id)
  result <- shiny::tagList(
    tags$div(
      selectWidgetsUI(
        id = ns(name),
        widgets = list(
          widgetsUI(
            widget = textInput,
            name = "stringPositive",
            label = "Select",
            value = defaultPositiveValue,
            width = width
          ),
          widgetsUI(
            widget = textInput,
            name = "stringNegative",
            label = "De-select",
            value = defaultNegativeValue,
            width = width
          )
        ),
        checked = checked,
        checkedLabel = label,
        insideTagsDiv = insideTagsDiv
      )
    )
  )
  return(result)
}
    
#' filesSelectDefinition Server Functions
#'
#' @noRd 
filesSelectDefinitionServer <- function(id = "msfiles",
                                        name = "msPeakFilesStrings"){
  moduleServer(
    id,
    function(input, output, session){
      filesSelect <- selectWidgetsServer(id = name,
                                         widgets = c("stringPositive","stringNegative"))
      
      returnValue <- reactive({
        result <- filesSelect()
        result[result == ""] <- NA
        return(result)
      })
      
      return(returnValue)
      
    }
  )
}

# ---- minimal functional app ----

tryCatch(
  {
    library(shiny)
    library(shinyjs)
    
    ui <- fluidPage(
      shinyjs::useShinyjs(),
      tags$head(
        tags$style(
          HTML("
      .sidebarpanel {
        background-color: rgba(255,255,255,1);
        border-style : solid;
        border-color : rgba(255,255,255,1);
      }
      .boxMiddle {
      width: 100%;
      display: flex;
      justify-content: center;
      }
      ")
        )
      ),
      fluidPage(
        sidebarPanel(
          class = "sidebarpanel",
          width = 2,
          filesSelectDefinitionUI(id = "msFiles",
                                  insideTagsDiv = "boxMiddle"),
          hr()
        )
      )
    )
    
    server <- function(input, output, session){
      
      test <- filesSelectDefinitionServer(id = "msFiles")
      
      observeEvent(test(), {
        print(test())
      })
      
    }
    
    shinyApp(ui = ui, server = server)
  },
  error = function(e){}
)

