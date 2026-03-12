#' review2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param insideTagsDiv character vector that enables embedding the element
#'  'inside' a certain css class, default is NA
#' @param buttonClass character vector that defines bootstrap css class name
#'  for button(s) to be used. Default is 'btn-primary'
#' @param width character vector, width of the UI element 
#' @param targetWidth character vector, width of the selectTargets UI element.
#'  Needs to be a bit wider than other elements (150px) to accomodate the names
#'  of the targets table. Default is. '200px'
#'  
#' @noRd 
#'
#' @importFrom shiny NS tagList 
review2UI <- function(id = "review2",
                      insideTagsDiv = NA,
                      buttonClass = "btn-primary",
                      selectionWidth = "200px",
                      width = "150px"){
  ns <- NS(id)
  result <- tagList(
    fluidPage(
      id = ns("review2"),
      sidebarPanel(
        class = "sidebarpanel",
        width = 2,
        br(),
        tags$div(
          class = insideTagsDiv,
          selectInput(
            inputId = ns("selectSample"),
            label = "Sample",
            choices = c(1),
            selected = 1,
            multiple = FALSE,
            width = selectionWidth
          )
        ),
        br(),
        tags$div(
          class = insideTagsDiv,
          downloadButton(
            outputId = ns("downloadComplete"),
            label = "Sample",
            class = buttonClass,
            icon = icon("download"),
            width = width
          )
        ),
        br(),
        tags$div(
          class = insideTagsDiv,
          downloadButton(
            outputId = ns("downloadCompleteAll"),
            label = "All Samples",
            class = buttonClass,
            icon = icon("download"),
            width = width
          )
        )
      ),
      mainPanel(
        width = 10,
        fluidRow(
          br(),
          gt::gt_output(outputId = ns("combinedTable"))
        )
      )
    )
  )
  return(result)
}
    
#' review2 Server Functions
#'
#' @param resultList data which is transferred from process module to this one
#'
#' @noRd 
review2Server <- function(id = "review2",
                          resultList = reactive({
                            list()
                          })){
  moduleServer(
    id,
    function(input, output, session){
      
      rv <- reactiveValues()
      
      rv$resultList <- list()
      
      observeEvent(resultList(),{
        rv$resultList <- resultList()
        if (length(rv$resultList) == 0){
          shinyjs::disable("selectSample")
          shinyjs::disable("downloadComplete")
          shinyjs::disable("downloadCompleteAll")
        } else {
          updateSelectInput(inputId = "selectSample",
                            choices = names(rv$resultList),
                            selected = names(rv$resultList)[1])
          shinyjs::enable("selectSample")
          shinyjs::enable("downloadComplete")
          shinyjs::enable("downloadCompleteAll")
        }
      })
      
      output$combinedTable <- gt::render_gt({
        if (length(rv$resultList) != 0){
          if (input$selectSample %in% names(rv$resultList))
            showTable(rv$resultList[[input$selectSample]]$result,
                      useTitle = input$selectSample)
        }
      })
      
      output$downloadComplete <- downloadHandler(
        filename = function(){
          samplename <- input$selectSample
          paste(c(samplename, '.csv'), collapse = '')
        },
        content = function(file){
          write.table(rv$resultList[[input$selectSample]]$result  %>% 
                        dplyr::arrange(desc(Control), locationSort(Location), ModificationSort(Modification)),
                      file = file,
                      row.names = FALSE,
                      col.names = TRUE,
                      na = "", sep = ",")
        }
      )
      
      output$downloadCompleteAll <- downloadHandler(
        filename = function(){
          paste(c('All-',as.character(Sys.Date()),'.zip'), collapse = '')
        },
        content = function(file){
          for (counter in 1:length(rv$resultList)){
            curDir <- base::tempdir()
            samplename <- names(rv$resultList)[counter]
            sampleName <- paste(c(curDir,"/",samplename, '.csv'), collapse = '')
            write.table(rv$resultList[[counter]]$result %>% 
                          dplyr::arrange(desc(Control), locationSort(Location), ModificationSort(Modification)),
                        file = sampleName,
                        row.names = FALSE,
                        col.names = TRUE,
                        na = "", sep = ",")
          }
          theFiles <- list.files(path = paste0(curDir, "/"), pattern = "*.csv", full.names = TRUE)
          theFiles <- grep(theFiles, pattern = "\\.csv", value = TRUE)
          utils::zip(zipfile = paste0(curDir,"/tempzipped.zip"),
                     files = theFiles,
                     flags = "-j")
          base::file.copy(paste0(curDir,"/tempzipped.zip"), file)
        }
      )
      
      returnValue <- reactive({
        return(NA)
      })
      
      return(returnValue)
      
    })
}
    
# ---- minimal functional app ----
# In this case it cannot do anything, because no output from process module is
#  defined

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
      review2UI(insideTagsDiv = "boxMiddle"),
      hr()
    )
    
    server <- function(input, output, server){
      
      test <- review2Server()
      
    }
    
    shinyApp(ui = ui, server = server)
    
  },
  error = function(e){}
)
