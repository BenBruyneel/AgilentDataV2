#' filesTable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param sidebarClass css class for the sidebar
#' @param buttonClass css class of the button
#' @param insideTagsDiv character vector that enables embedding the element
#'  'inside' a certain css class, default is NA
#' @param width character vector, width of the UI element 
#' 
#' @noRd 
#'
filesTableUI <- function(id = "filesTable",
                         sidebarClass = "sidebarpanel",
                         buttonClass = "btn-primary",
                         insideTagsDiv = NA,
                         width = "150px"){
  ns = NS(id)
  result <- tagList(
    shiny::fluidPage(
      shiny::sidebarPanel(
        class = sidebarClass,
        width = 2,
        br(),
        shinydashboard::box(height = "16px", width = "100%"),
        sHLine,
        br(),
        tagList(
          tags$div(
            class = insideTagsDiv,
            shiny::fileInput(
              inputId = ns("uploadFiles"),
              label = "Upload Files",
              multiple = TRUE,
              width = "150px"
            )
          )
        ),
        sHLine,
        shiny::tags$div(
          class = insideTagsDiv,
          HTML("<h5><strong>Send Data</strong></h5>")
        ),
        shiny::tags$div(
          class = insideTagsDiv,
          shiny::actionButton(
            inputId = ns("sendData"),
            class = buttonClass,
            label = "To Processing",
            icon = icon("bolt"),
            width = "130px",
            disabled = TRUE
          )
        ),
        sHLine,
        shiny::tags$div(
          class = insideTagsDiv,
          filesSelectDefinitionUI(id = id,
                                  name = "fileFilter",
                                  width = width,
                                  label = "Filter",
                                  checked = FALSE,
                                  defaultPositiveValue = "",
                                  defaultNegativeValue = "",
                                  insideTagsDiv = insideTagsDiv)
        ),
        sHLine,
        tags$div(
          class = insideTagsDiv,
          shinyWidgets::pickerInput(
            inputId = ns("selectSamples"),
            label = "Manual Selection",
            choices = 1,
            selected = 1,
            multiple = TRUE,
            width = width,
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "None",
              `select-all-text` = "All",
              `selected-text-format`= "count",
              `count-selected-text` = "{0} Selected")
          )
        ),
        sHLine,
        shiny::tags$div(
          class = insideTagsDiv,
          shiny::htmlOutput(outputId = ns("completeSamples"))
        )
      ),
      shiny::mainPanel(
        width = 10,
        shiny::fluidRow(
          br(),
          shiny::column(12,
                        shiny::htmlOutput(outputId = ns("directory"))
          )
        ),
        sHLine,
        shiny::fluidRow(
          column(4,
                 shiny::h5("Files"),
                 shiny::htmlOutput(outputId = ns("files"))
          ),
          column(4,
                 h5("MS Peak Lists"),
                 htmlOutput(outputId = ns("msPeak_Files"))
          ),
          # column(3,
          #        h5("Chromatograms"),
          #        htmlOutput(outputId = ns("chrom_Files"))
          # ),
          column(4,
                 shiny::h5("Spectra"),
                 shiny::htmlOutput(outputId = ns("spectra_Files"))
          )
        )
      )
    )
  )
  return(result)
}
    
#' filesTable Server Functions
#'
#' @param uploadLimit defines uploadlimit (in Mb) for files
#'
#' @noRd 
filesTableServer <- function(id = "filesTable",
                             uploadLimit = 1024
){
  
  fileFilterServer <- filesSelectDefinitionServer(id = id)
  
  moduleServer(
    id,
    function(input, output, session){
      
      options(shiny.maxRequestSize=uploadLimit*1024^2)
      
      rv <- shiny::reactiveValues()
      
      rv$uploadedFilesDir <- NA
      
      shiny::observeEvent(input$uploadFiles,{
        theDir <- dirname(input$uploadFiles$datapath[1])
        for (counter in 1:nrow(input$uploadFiles)){
          file.rename(
            from = input$uploadFiles$datapath[counter],
            to = paste0(theDir, "/", input$uploadFiles$name[counter])
          )
        }
        rv$uploadedFilesDir <- theDir
      })
      
      uploadedFiles <- shiny::reactive({
        if (is.na(rv$uploadedFilesDir)){
          return(as.character())
        } else {
          return(list.files(path = rv$uploadedFilesDir,
                            pattern = "*.*",
                            full.names = T))
        }
      })
      
      output$directory <- shiny::renderText({
        if (!is.na(rv$uploadedFilesDir)){
          result <- "Files Loaded!"
        } else {
          result <- "No files loaded..."
        }
        result <- paste(c("<h5>", result, "</h5>"), collapse = "")
        return(result)
      })
      
      dirSelect <- shiny::reactive({
        if (!is.na(rv$uploadedFilesDir)){
          return(rv$uploadedFilesDir)
        } else {
          return(character(0))
        }
      })
      
      listOfFiles <- shiny::reactive({
        if (!is.na(rv$uploadedFilesDir)){
          result <- list.files(path = rv$uploadedFilesDir,
                               pattern = "*.*",
                               full.names = T)
          if (!identical(fileFilterServer(), NA)){
            result <- selectStrings(result,
                                    positive = fileFilterServer()$stringPositive,
                                    negative = fileFilterServer()$stringNegative)
          }
          return(result |> sort())
        } else {
          return(character(0))
        }
      })
      
      # ---- ----
      
      observeEvent(listOfFiles(),{
        if (length(listOfFiles()) != 0){
          result <- giveFilePathsFromStrings(listOfFiles())
          updatePickerInput(
            inputId = "selectSamples",
            choices = basename(result),
            selected = basename(result)
          )
          shinyjs::runjs(paste(c("$('#",id,"-selectSamples').prop('disabled', false);"), collapse = ""))
          shinyjs::runjs(paste(c("$('#",id,"-selectSamples').selectpicker('refresh');"), collapse = ""))
        } else {
          shinyjs::runjs(paste(c("$('#",id,"-selectSamples').prop('disabled', true);"), collapse = ""))
          shinyjs::runjs(paste(c("$('#",id,"-selectSamples').selectpicker('refresh');"), collapse = ""))
        }
      })
      
      listOfFilesFromSamples <- shiny::reactive({
        if (length(listOfFiles()) != 0){
          result <- listOfFiles()
          if (length(input$selectSamples) != 0){
            result <- result[grepl(result, pattern = paste(input$selectSamples, collapse = "|"))]
            return(result %>% sort())
          }
        }
        return(listOfFiles())
      })
      
      output$files <- shiny::renderText({
        if (length(listOfFilesFromSamples()) != 0){
          result <- basename(listOfFilesFromSamples())
          result <- purrr::map_chr(result, ~paste(c("<span style=\"font-family:'Courier New'; font-size:14px;\">",.x, "</span><br>"), collapse = ""))
          return(result)
        }
      })
      
      listOfMsFiles <- shiny::reactive({
        result <- listOfFilesFromSamples()
        if (length(result) != 0){
          result <- selectStrings(listOfFilesFromSamples(),
                                  positive = "_MS.csv$")
        }
        return(result |> sort())
      })
      
      output$msPeak_Files <- shiny::renderText({
        if (length(listOfMsFiles()) != 0){
          result <- basename(listOfMsFiles())
          result <- purrr::map_chr(result, ~paste(c("<span style=\"font-family:'Courier New'; font-size:14px;\">",.x, "</span><br>"), collapse = ""))
          return(result)
        }
      })
      
      listOfChromFiles <- shiny::reactive({
        result <- listOfFilesFromSamples()
        if (length(result) != 0){
          result <- listOfFilesFromSamples()
          if (!identical(chromFilesServer(), NA)){
            result <- selectStrings(result,
                                    positive = chromFilesServer()$stringPositive,
                                    negative = chromFilesServer()$stringNegative)
          }
        }
        return(result |> sort())
      })
      
      output$chrom_Files <- shiny::renderText({
        if (length(listOfChromFiles()) != 0){
          result <- basename(listOfChromFiles())
          result <- purrr::map_chr(result, ~paste(c("<span style=\"font-family:'Courier New'; font-size:14px;\">",.x, "</span><br>"), collapse = ""))
          return(result)
        }
      })
      
      listOfSpectraFiles <- shiny::reactive({
        result <- listOfFilesFromSamples()
        if (length(result) != 0){
          result <- selectStrings(listOfFilesFromSamples(),
                                  positive = ".mzdata.xml$")
        }
        return(result |> sort())
      })
      
      output$spectra_Files <- shiny::renderText({
        if (length(listOfSpectraFiles()) != 0){
          result <- basename(listOfSpectraFiles())
          result <- purrr::map_chr(result, ~paste(c("<span style=\"font-family:'Courier New'; font-size:14px;\">",.x, "</span><br>"), collapse = ""))
          return(result)
        }
      })
      
      returnValue <- shiny::reactive({
        return(list(MSPeakFiles = listOfMsFiles(),
                    # Chromatograms = listOfChromFiles() ,
                    Spectra = listOfSpectraFiles()))
      })
      
      output$completeSamples <- shiny::renderText({
        result <- testCompleteData(theFilenames = returnValue())
        completeSamples <- names(result[result])
        incompleteSamples <- names(result[!result])
        outputText <- as.character()
        if ((length(completeSamples) != 0) | (length(incompleteSamples) != 0)){
          if (length(completeSamples != 0)){
            outputText <- c(outputText, "<b>Complete Samples:</b><br><br>",
                            purrr::map_chr(completeSamples,
                                           ~paste(c(.x, "<br>"),
                                                  collapse = "")), "<br><br>")
          }
          if (length(incompleteSamples != 0)){
            outputText <- c(outputText, "<b>Incomplete Samples:</b><br><br>",
                            purrr::map_chr(incompleteSamples,
                                           ~paste(c("<span style='color:red;'>" ,
                                                    .x,
                                                    "</span><br>"  ),
                                                  collapse = "")))
          }
        }
        if (length(outputText)>0){
          return(outputText)
        }
      })
      
      rv$sendData <- FALSE
      
      completeData <- shiny::reactive({
        result <- list(MSPeakFiles = listOfMsFiles(),
                       # Chromatograms = listOfChromFiles() ,
                       Spectra = listOfSpectraFiles())
        result <- testCompleteData(gatherAllCompleteFiles(result))
        if (length(result) != 0){
          return(TRUE)
        }
        return(FALSE)
      })
      
      shiny::observeEvent(completeData(),{
        rv$sendData <- FALSE
        if (completeData()){
          shinyjs::enable("sendData")
        } else {
          shinyjs::disable("sendData")
        }
      })
      
      shiny::observeEvent(input$sendData,{
        rv$sendData <- TRUE
      })
      
      returnValue1 <- shiny::reactive({
        if (rv$sendData){
          if (completeData()){
            return(
              gatherAllCompleteFiles(theFiles = list(MSPeakFiles = listOfMsFiles(),
                                                     # Chromatograms = listOfChromFiles() ,
                                                     Spectra = listOfSpectraFiles()))
            )
          } else {
            return(
              processingFilesEmpty()
            )
          }
        }
      })
      
      return(returnValue1)
      
    }
  )
}

# ---- minimal functional app ----

tryCatch(
  {
    library(shiny)
    library(shinyjs)
    library(shinyWidgets)
    library(shinydashboard)
    
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
           .selectInputs {
            width: 1500px;
            height: 40px;
            margin: 2px;
            display: flex;
            }
            ")
        )
      ),
      filesTableUI(
        id = "filesTable",
        insideTagsDiv = "boxMiddle"),
      hr()
    )
    
    server <- function(input, output, session){
      
      test <- filesTableServer(id = "filesTable")
      
      observeEvent(test(),{
        print(test())
      })
      
    }
    
    shinyApp(ui = ui, server = server)
  },
  error = function(e){}
)

