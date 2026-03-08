
#' @title widgetsUI
#'
#' @description
#'  Generic helper function that generates a UI element to be used in a UI
#'   definition
#' 
#'
#' @param widget shiny/widget element, e.g. radioButtons or selectInput. This
#'  parameter must be the element itself, not a character vector with the name
#'  or the 'function result'. so 'selectInput', not '"selectInput"' and also not
#'  'selectInput()'
#' @param name name of the widget. This will be how it is recognized in the
#'  server section of a module or app
#' @param output loical vector that defines whether the element is an input
#'  (default) or output element
#' @param insideTagsDiv character vector that enables embedding the element
#'  'inside' a certain css class, default is NA
#' @param ... allows passing on additional parameters to the element when used.
#'  E.g. choices or selected in the case of selectInput
#'
#' @returns tag list to be used in an UI function
#'
#' @noRd
widgetsUI <- function(widget,
                      name, 
                      output = FALSE,
                      insideTagsDiv = NA,
                      ...){
  function(id){
    ns <- NS(id)
    return(
      shiny::tagList(
        ifelseProper(!output,
                     shiny::tags$div(class = insideTagsDiv,
                                     widget(inputId = ns(name), ...)),
                     shiny::tags$div(class = insideTagsDiv,
                                     widget(outputId = ns(name), ...))
        )
      )
    )
  }
}

#' selectWidgets UI Function
#'
#' @description
#'  A shiny Module
#'  
#'  Creates a group widgets that can serve as (optional) options for an app.
#'  A checkboxInput is added to the top to serve as criterium to use or not use
#'  the options
#'  
#'
#' @param id,input,output,session Internal parameters for {shiny}
#' @param widgets a list of widgetsUI elements
#' @param checked logical vector that defines whether the checkboxInput option
#'  should be 'checked' (default is TRUE)
#' @param checkedLabel if not NULL (default) then this is the name put next to
#'  the checkboxInput UI element
#' @param insideTagsDiv character vector that enables embedding the element
#'  'inside' a certain css class, default is NA
#' @param hideSelect logical vector. If TRUE then no checkboxInput element will
#'  be added. Default is FALSE
#'  
#' @noRd 
selectWidgetsUI <- function(id,
                            widgets,
                            checked = TRUE,
                            checkedLabel = NULL,
                            insideTagsDiv = NA,
                            hideSelect = FALSE){
  ns <- NS(id)
  if (hideSelect){
    result <- tagList()  
  } else {
    result <- tagList(
      tags$div(class = insideTagsDiv,
               checkboxInput(inputId = ns("active"),
                             label = checkedLabel,
                             value = checked)
      )
    )
  }
  for (counter in 1:length(widgets)){
    result <- tagAppendChild(result, widgets[[counter]](id = id))
  }
  return(result)
}
    
#' selectWidgets Server Functions
#'
#' @param widgets character vector. Defines the names of the widgets defined in
#'  the widgetsUI elements used
#' @param leaveOutWidgets if not NA (default) then character vector of names of
#'  widgets not to be returned/used by the module
#' @param sqlOutput if not NA (default) than this should be a function that
#'  'transforms' the output/result into an SQL string. Can be potentially other
#'  things as well
#' @param hideSelect logical vector. If TRUE then the checkboxInput element used
#'  for selection the elements ('switching on' the options) is hidden via
#'  shinyjs::hide. Default is FALSE
#' @param selectPresent logical vector. Default is TRUE: the selectoption is
#'  present.
#' @param overRuleSelected reactive function that enables 'switching on/off' the
#'  from 'outside' the module server
#' @param noSelectionNA logical vector, default is TRUE: if select is 'off' then
#'  the server will return NA. If FALSE, it will return FALSE in that case.
#'
#' @noRd 
selectWidgetsServer <- function(id,
                                widgets,
                                leaveOutWidgets = NA,
                                sqlOutput = NA,
                                hideSelect = FALSE, selectPresent = TRUE,
                                overRuleSelected = reactive({return(FALSE)}),
                                noSelectionNA = TRUE){
  
  moduleServer(
    id,
    function(input, output, session){
      
      if (hideSelect & selectPresent){
        shinyjs::hide("active")
      }
      
      observeEvent(input$active,{
        if (input$active & !overRuleSelected()){
          for (counter in 1:length(widgets)){
            shinyjs::enable(widgets[counter])
          }
        } else {
          for (counter in 1:length(widgets)){
            shinyjs::disable(widgets[counter])
          }
        }
      })
      
      observeEvent(overRuleSelected(),{
        if (overRuleSelected()){
          for (counter in 1:length(widgets)){
            shinyjs::disable(widgets[counter])
          }
        } else {
          if ("active" %in%  names(input)){  # when hideSelect = TRUE, input$active may/does not exist: revisit
            if (input$active){
              for (counter in 1:length(widgets)){
                shinyjs::enable(widgets[counter])
              } 
            }
          } else {
            for (counter in 1:length(widgets)){
              shinyjs::enable(widgets[counter])
            } 
          } 
        }
      })
      
      returnValue <- reactive({
        if (!hideSelect){
          if (!input$active){
            if (!noSelectionNA & !identical(sqlOutput, NA)){
              return(sqlOutput(NA))
            }
            if (noSelectionNA){
              return(NA)
            } else {
              return(FALSE)
            }
          }
        }
        if (!identical(widgets, NA)){
          result <- list()
          if (!is.na(leaveOutWidgets)){
            theWidgets <- widgets[!(widgets %in% leaveOutWidgets)]
          } else {
            theWidgets <- widgets
          }
          if (length(theWidgets)>0){
            for (counter in 1:length(theWidgets)){
              result[[counter]] <- input[[theWidgets[counter]]]
            }
            names(result) <- theWidgets
            if (identical(sqlOutput, NA)){
              return(result)
            } else {
              return(sqlOutput(result))
            }
          } else {
            # sqlOutput not possible
            return(TRUE)
          }
        } else {
          if (identical(sqlOutput, NA)){
            return(NA)
          } else {
            return(sqlOutput(NA))
          }
        }
      })
      
      return(returnValue)
      
    }
  )
}
    
## To be copied in the UI
# mod_selectWidgets_ui("selectWidgets_1")
    
## To be copied in the server
# mod_selectWidgets_server("selectWidgets_1")

# ---- minimal functional app ----

tryCatch(
  {
    library(shiny)
    library(shinyjs)
    library(DT)
    
    ui <- fluidPage(
      shinyjs::useShinyjs(),
      tags$head(
        tags$style(
          HTML('
      .boxMiddle {
          width: 100%;
          display: flex;
          justify-content: center;
      }
      ')
        )
      ),
      column(4,
             selectWidgetsUI(
               id = "isotopeCLuster",
               widgets = list(widgetsUI(widget = checkboxInput,
                                        name = "positive",
                                        insideTagsDiv = "boxMiddle",
                                        label = "Positive",
                                        width = "150px",
                                        value = TRUE),
                              widgetsUI(widget = selectInput, name = "adduct",
                                        insideTagsDiv = "boxMiddle",
                                        label = "Adduct", width = "150px",
                                        choices = c("-","H"),
                                        selected = "H"),
                              widgetsUI(widget = sliderInput, name = "resolution",
                                        insideTagsDiv = "boxMiddle",
                                        label = "Resolution", width = "150px",
                                        min = 1000, max = 1E6, step = 1000, value = 20000),
                              widgetsUI(widget = sliderInput, name = "threshold",
                                        insideTagsDiv = "boxMiddle",
                                        label = "Threshold (%)", width = "150px",
                                        min = 0.001, max = 1, step = 0.001, value = 0.1),
                              widgetsUI(widget = sliderInput, name = "mzAccuracy",
                                        insideTagsDiv = "boxMiddle",
                                        label = "m/z Accuracy", width = "150px",
                                        min = 1, max = 10, step = 1, value = 4),
                              widgetsUI(widget = sliderInput, name = "abundanceAccuracy",
                                        insideTagsDiv = "boxMiddle",
                                        label = "Abundance Accuracy", width = "150px",
                                        min = 0, max = 10, step = 1, value = 2)
               ),
               checked = TRUE,
               checkedLabel = "Calculate isotope cluster",
               insideTagsDiv = "boxMiddle",
               hideSelect = FALSE
             )
      ),
      column(7,
             uiOutput(outputId = "isotopeTableOutput")
      ),
      column(1)
    )
    
    server <- function(input, output, session){
      isotopeClusterWidget <- selectWidgetsServer("isotopeCLuster",
                                                  hideSelect = FALSE,
                                                  selectPresent = FALSE,
                                                  widgets = c("positive", "adduct", "resolution", "threshold", "mzAccuracy", "abundanceAccuracy"))
      
      output$isotopeTableOutput <- renderUI({
        tempResult <- isotopeClusterWidget()
        if (!identical(tempResult, NA)){
          tempResult <- data.frame(parameter = names(tempResult), value = unname(unlist(tempResult)))
          renderDT(tempResult, server = TRUE, options = list(), rownames = FALSE)
        } else {
          renderText("No Data...")
        }
      })
      
    }
    
    shinyApp(ui = ui, server = server)
  },
  error = function(e){}
)

