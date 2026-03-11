#' process UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @param admin logical vector, defines whether administrator version of UI is
#'  displayed. Default is TRUE
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
processUI <- function(id = "processing",
                      admin = TRUE,
                      insideTagsDiv = NA,
                      buttonClass = "btn-primary",
                      width = "150px",
                      targetWidth = "200px"){
  ns <- NS(id)
  result <- tagList(
    fluidPage(
      sidebarPanel(
        class = "sidebarpanel",
        width = 2,
        br(),
        shinydashboard::box(height = "2px"),
        tags$div(
          class = insideTagsDiv,
          actionButton(
            inputId = ns("startProcessing"),
            class = buttonClass,
            label = "Process",
            icon = icon("route"),
            width = width,
            disabled = TRUE
          )
        ),
        br(),
        tags$div(
          class = insideTagsDiv,
          checkboxInput(
            inputId = ns("readData"),
            label = "Read Data",
            width = width,
            value = FALSE
          )
        ),
        tags$div(
          class = insideTagsDiv,
          checkboxInput(
            inputId = ns("createTables"),
            label = "Create Tables",
            width = width,
            value = FALSE
          )
        ),
        sHLine,
        tags$div(
          class = insideTagsDiv,
          HTML("<h5><strong>Send Data</strong></h5>")
        ),
        tags$div(
          class = insideTagsDiv,
          actionButton(
            inputId = ns("sendData"),
            class = buttonClass,
            label = "To Results",
            icon = icon("square-poll-horizontal"),
            width = width,
            disabled = TRUE
          )
        ),
        ifelseProper(
          admin,
          tagList(
            sHLine,
            tags$div(
              class = insideTagsDiv,
              HTML("<h5><strong>Targets</strong></h5>")
            )
          ),
          tagList()
        ),
        tags$div(
          class = insideTagsDiv,
          selectInput(
            inputId = ns("selectTargets"),
            label = NULL,
            choices = c(1,2),
            selected = 1,
            width = targetWidth
          )
        ),
        tags$div(
          class = insideTagsDiv,
          actionButton(
            inputId = ns("setTargets"),
            label = "Set",
            class = "btn-primary",
            width = width
          )
        ),
        sHLine,
        tags$div(
          class = insideTagsDiv,
          HTML("<h5><strong>Target Parameters</strong></h5>")
        ),
        tags$div(
          class = insideTagsDiv,
          checkboxInput(
            inputId = ns("useRRt"),
            label = "Use relative rt",
            value = TRUE
          )
        ),
        tabsetPanel(
          id = ns("tableOptions"),
          tabPanel(
            title = "Smooth",
            icon = icon("hammer"),
            br(),
            tags$div(
              class = insideTagsDiv,
              checkboxInput(
                inputId = ns("useSmooth"),
                label = "Use Smoothed Data",
                value = FALSE
              )
            )
          ),
          tabPanel(
            title = "Spectra",
            icon = icon("chart-column"),
            br(),
            selectWidgetsUI(
              id = ns("setSingleRt"),
              widgets = list(
                widgetsUI(
                  widget = sliderInput,
                  name = "setSingleRt",
                  label = "Rt +/- (min)",
                  value = 0.05,
                  min = 0.01,
                  max = 0.5,
                  step = 0.01,
                  insideTagsDiv = insideTagsDiv,
                  width = width
                )
              ),
              checked = TRUE,
              checkedLabel = "Single point spectra",
              insideTagsDiv = insideTagsDiv,
              hideSelect = FALSE
            )
          ),
          tabPanel(
            title = "Rt",
            icon = icon("chart-area"),
            br(),
            selectWidgetsUI(
              id = ns("rtOptions"),
              widgets = list(
                widgetsUI(
                  widget = sliderInput,
                  name = "rtTolerance",
                  label = "Tolerance rt",
                  value = c(-0.05, 0.05),
                  min = -1.5,
                  max = 1.5,
                  step = 0.01,
                  insideTagsDiv = insideTagsDiv,
                  width = width
                ),
                widgetsUI(
                  widget = checkboxInput,
                  name = "rtClosest",
                  label = "Closest rt",
                  value = TRUE,
                  insideTagsDiv = insideTagsDiv,
                  width = width
                ),
                widgetsUI(
                  widget = checkboxInput,
                  name = "rtPeak",
                  label = "Peak #",
                  value = FALSE,
                  insideTagsDiv = insideTagsDiv,
                  width = width
                )
              ),
              checked = TRUE,
              checkedLabel = NULL,
              insideTagsDiv = insideTagsDiv,
              hideSelect = TRUE
            ),
            br(),
            selectWidgetsUI(
              id = ns("refFixed"),
              widgets = list(
                widgetsUI(
                  widget = textInput,
                  name = "fixedRefRt",
                  label = "Reference Rt",
                  value = "28.69",
                  insideTagsDiv = insideTagsDiv,
                  width = width
                )
              ),
              checked = FALSE,
              checkedLabel = "Use Reference Rt",
              insideTagsDiv = insideTagsDiv,
              hideSelect = FALSE
            )
          ),
          tabPanel(
            title = "RRt",
            icon = icon("location-pin-lock"),
            br(),
            selectWidgetsUI(
              id = ns("rrtOptions"),
              widgets = list(
                widgetsUI(
                  widget = sliderInput,
                  name = "rtTolerance",
                  label = "Tolerance Rt RRT",
                  value = c(-0.5, 0.5),
                  min = -0.50,
                  max = 0.50,
                  step = 0.01,
                  insideTagsDiv = insideTagsDiv,
                  width = width
                ),
                widgetsUI(
                  widget = checkboxInput,
                  name = "rtClosest",
                  label = "Closest rt",
                  value = TRUE,
                  insideTagsDiv = insideTagsDiv,
                  width = width
                ),
                widgetsUI(
                  widget = checkboxInput,
                  name = "rtPeak",
                  label = "Peak #",
                  value = FALSE,
                  insideTagsDiv = insideTagsDiv,
                  width = width
                )
              ),
              checked = TRUE,
              checkedLabel = NULL,
              insideTagsDiv = insideTagsDiv,
              hideSelect = TRUE
            )
          ),
          tabPanel(
            title = "m/z",
            icon = icon("chart-column"),
            br(),
            selectWidgetsUI(
              id = ns("mzOptions"),
              widgets = list(
                widgetsUI(
                  widget = checkboxInput,
                  name = "useLimitRt",
                  label = "Use rt Limits",
                  value = FALSE,
                  insideTagsDiv = insideTagsDiv,
                  width = width
                ),
                widgetsUI(
                  widget = sliderInput,
                  name = "limitRt",
                  label = "rt Limits",
                  value = 0.5,
                  min = 0,
                  max = 1.5,
                  step = 0.01,
                  insideTagsDiv = insideTagsDiv,
                  width = width
                ),
                widgetsUI(
                  widget = sliderInput,
                  name = "peakMzLimits",
                  label = "m/z range (ppm)",
                  value = c(-5,5),
                  min = -25,
                  max = 25,
                  step = 0.5,
                  insideTagsDiv = insideTagsDiv,
                  width = width
                ),
                widgetsUI(
                  widget = checkboxInput,
                  name = "peakClosest",
                  label = "Closest peak",
                  value = TRUE,
                  insideTagsDiv = insideTagsDiv,
                  width = width
                ),
                widgetsUI(
                  widget = sliderInput,
                  name = "peakIntensityLimit",
                  label = "Minumum intensity",
                  value = 10,
                  min = 0,
                  max = 1000,
                  step = 10,
                  insideTagsDiv = insideTagsDiv,
                  width = width
                )
              ),
              checked = TRUE,
              checkedLabel = NULL,
              insideTagsDiv = insideTagsDiv,
              hideSelect = TRUE
            )
          )
        )
      ),
      mainPanel(
        width = 10,
        br(),
        br(),
        fluidRow(
          h4("Samples")
        ),
        fluidRow(
          reactable::reactableOutput(ns("table"))
        )
      )
    )
  )
  return(result)
}
    
#' process Server Functions
#' 
#' @param admin logical vector, defines whether administrator version of server
#'  is used. Default is TRUE. administrator version hides some UI elements in
#'  order to hide certain app/module options. These options should only be used
#'  for debugging options
#' @param toProcess reactive that returns a list of MSPeakFiles & Spectra
#'  filenames (both character vectors). See e.g. the 'processingFilesEmpty' (is
#'  also the default). Essential to pass on correctly.
#' @param theTargets reactive that returns a character vector of the available
#'  targetFiles (essential to pass on correctly)
#' @param defaultTarget reactive that returns integer that will be default
#'  selected targetFile (from the parameter theTargets)
#' @param fontFamily,fontsize,cellpadding set of character vectors that define
#'  properties of the main table in the UI
#' 
#' @noRd 
processServer <- function(id = "processing",
                          admin = TRUE,
                          toProcess = reactive({return(
                            processingFilesEmpty()
                          )}),
                          theTargets = reactive({return("targetFiles")}),
                          defaultTarget = reactive({1}),
                          fontFamily = reactive({return("Courier")}),
                          fontSize = reactive({return("13px")}),
                          cellpadding = reactive({return("3px")})){
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      if (!admin){
        shiny::hideTab(inputId = "tableOptions", target = "Smooth")
        shiny::hideTab(inputId = "tableOptions", target = "Spectra")
        shiny::hideTab(inputId = "tableOptions", target = "RRt")
        shiny::hideTab(inputId = "tableOptions", target = "m/z")
        
        shinyjs::hide(paste(c("rtOptions", "rtClosest"), collapse = "-"))
        shinyjs::hide(paste(c("rtOptions", "rtPeak"), collapse = "-"))
        
        shiny::updateTabsetPanel(
          inputId = "tableOptions",
          selected = "Rt"
        )
        
        shinyjs::hide("selectTargets")
        shinyjs::hide("setTargets")
        shinyjs::hide("useRRt")
        shinyjs::hide("useSmooth")
        
        updateCheckboxInput(inputId = "readData",
                            value = TRUE)
        shinyjs::hide("readData")
        updateCheckboxInput(inputId = "createTables",
                            value = TRUE)
        shinyjs::hide("createTables")
      }
      
      rv <- reactiveValues()
      
      rv$files <- processingFilesEmpty()
      
      observeEvent(toProcess(),{
        if (!emptyFiles(theFiles = toProcess())){
          rv$files <- toProcess()
        }
      })
      
      rv$resultList <- list()
      
      rv$theData <- list()
      
      rv$theTable <- data.frame(samplename = as.character(),
                                peaktables = as.integer(),
                                Spectra = as.integer(),
                                rtArea = as.logical(),
                                mzPpm = as.logical())
      
      observeEvent(rv$files,{
        if (countProcessingData(theFilenames = rv$files,
                                includeIncomplete = FALSE) > 0){
          updateActionButton(inputId = "startProcessing",
                             session = session,
                             disabled = FALSE)
        } else {
          updateActionButton(inputId = "startProcessing",
                             session = session,
                             disabled = TRUE)
        }
      })
      
      sampleTable <- reactive({
        if (length(rv$resultList) == 0){
          if (!emptyFiles(rv$files)){
            result <- testCompleteData(rv$files)
            result <- names(result)[result]
            return(
              data.frame(
                samplename = result,
                peaktables = rep(0, length(result)),
                Spectra = rep(0, length(result)),
                processed = rep(FALSE, length(result))
              )
            )
          } else {
            return(
              data.frame(samplename = as.character(),
                         peaktables = as.integer(),
                         Spectra = as.integer(),
                         rtArea = as.logical(),
                         mzPpm = as.logical()
              )
            )
          }
        } else {
          tempResult <- returnValue()
          result <- data.frame(
            samplename = names(tempResult$resultData),
            peaktables = unname(purrr::map_int(tempResult$resultData, ~ifelse(length(.x)>0,
                                                                       ifelse(nrow(.x$peakList) !=0, nrow(.x$peakList), 0),
                                                                       0))),
            Spectra = unname(purrr::map_int(tempResult$resultData, ~ifelse(length(.x) !=0, .x$spectra$length, 0))),
            targets = unname(purrr::map_chr(tempResult$resultData, ~ifelse(length(.x) !=0, .x$targetsName, targetNamesDefault())))
          )
          if (length(tempResult$resultList) != 0){
            result$processed = unname(purrr::map_lgl(tempResult$resultList, ~!identical(.x, list())))
          } else {
            result$processed = FALSE
          }
          return(result)
        }
      })
      
      targetNames <- reactive({
        return(theTargets()$name)
      })
      
      targetNamesDefault <- reactive({
        return(theTargets()$name[defaultTarget()])
      })
      
      
      observeEvent(sampleTable(),{
        if (!("targets" %in% colnames(sampleTable()))){
          if (nrow(sampleTable()) == 0){
            rv$theTable <- dplyr::bind_cols(sampleTable(), data.frame(targets = as.character()))
          } else {
            rv$theTable <- dplyr::bind_cols(sampleTable(), data.frame(targets = targetNamesDefault()))
          }
        } else {
          rv$theTable <- sampleTable()
        }
      })
      
      observeEvent(list(targetNames(), targetNamesDefault()),{
        updateSelectInput(
          inputId = "selectTargets",
          choices = targetNames(),
          selected = targetNamesDefault()
        )
      })
      
      selectedSamples <- reactive(reactable::getReactableState("table", "selected"))
      
      output$table <- reactable::renderReactable({
        if (nrow(rv$theTable) != 0){
          defaultToSelect <- ifelseProper(length(selectedSamples()) != 0, selectedSamples(), NULL)
          if (length(defaultToSelect) !=0){
            # in case # of rows in the table has changed
            defaultToSelect <- defaultToSelect[purrr::map_lgl(defaultToSelect, ~(.x <= nrow(rv$theTable)))]
          }
          reactable::reactable(rv$theTable %>%
                      dplyr::select(samplename, peaktables, Spectra, targets, processed),
                    pagination = FALSE,
                    highlight = TRUE,
                    defaultSelected = defaultToSelect,
                    selection = "multiple",
                    onClick = "select",
                    resizable = TRUE,
                    sortable = FALSE,
                    columns = list(
                      samplename = reactable::colDef(name = "Sample", width = 250, maxWidth = 400, align = "left"),
                      peaktables = reactable::colDef(name = "Peak Tables", width = 125, align = "center"),
                      Spectra = reactable::colDef(name = "Spectra", width = 125, align = "center"),
                      targets = reactable::colDef(name = "Targets", width = 250, align = "center"),
                      processed = reactable::colDef(name = "Processed",
                                         align = "center",
                                         width = 150,
                                         cell = function(value) {
                                           # Render as an X mark or check mark
                                           if (!value) "\u274c" else "\u2714\ufe0f"})
                    ),
                    theme = reactable::reactableTheme(
                      rowSelectedStyle = list(backgroundColor = "#5272d2", boxShadow = "inset 2px 0 0 0 #5272d2", color = "#ffffff"),
                      style = list(fontFamily = fontFamily(), fontSize = fontSize()),
                      cellPadding = cellpadding()
                    ))
        }
      })
      
      observeEvent(input$setTargets,{
        rv$theTable$targets[selectedSamples()] <- input$selectTargets
      })
      
      observeEvent(input$startProcessing,{
        if (input$readData){
          todo <- rv$theTable$samplename[selectedSamples()]
          progressor <- shiny::Progress$new(min = 0, max = 4)
          on.exit(progressor$close())
          updateProgressor <- function(message = NULL, step = NULL) {
            if (is.null(step)) {
              step <- progressor$getValue()
              step <- step + (progressor$getMax() - step) / 4
            }
            progressor$set(value = step, message = message)
          }
          for (counter in 1:nrow(rv$theTable)){
            if (counter %in% selectedSamples()){
              tempSample <- gatherRelatedFiles(
                theFiles = rv$files,
                sample = rv$theTable$samplename[counter]
              )
              tempResult <- list()
              progressor$set(message = paste0("Start Processing: ", rv$theTable$samplename[counter]), value = 0)
              tempResult <- getData.All(
                # sendMessage = shinyLog(),
                peptideTargetsFile = targetFiles$file[1],
                updateProgressor = updateProgressor,
                sampleName = rv$theTable$samplename[counter],
                listData = tempResult,
                mzDataFile = tempSample$Spectra,
                msPeakFile = tempSample$MSPeakFiles,
                useSmooth = input$useSmooth# ,
                # setSingleRt = setSingleRtValue()
              )
              rv$resultList[[counter]] <- tempResult[[1]]
              rv$resultList[[counter]]$targetsName <- rv$theTable$targets[counter]
              names(rv$resultList)[counter] <- names(tempResult)
              # empty result tables
              if (length(rv$theData) != 0){
                rv$theData[[counter]] <- list()
                names(rv$theData)[counter] <- names(tempResult)
              }
            } else {
              if (counter > length(rv$resultList)){
                rv$resultList[[counter]] <- list()
                names(rv$resultList)[counter] <- rv$theTable$samplename[counter]
              }
            }
          }
          progressor$close()
        }
        if (input$createTables){
          progressor <- shiny::Progress$new(min = 0, max = (length(rv$resultList)/2)+1)
          on.exit(progressor$close())
          updateProgressor <- function(message = NULL, step = NULL) {
            if (is.null(step)) {
              step <- progressor$getValue()
              step <- step + (progressor$getMax() - step) / ((length(rv$resultList)/2)+1)
            }
            progressor$set(value = step, message = message)
          }
          if (length(rv$theData) == 0){
            if (length(rv$resultList) != 0)
              for (counter in 1:length(rv$resultList)){
                rv$theData[[counter]] <- list()
              }
            names(rv$theData) <- names(rv$resultList)
          }
          progressor$set(message = "Start table creation: ", value = 0)
          print("Creating Tables ----")
          for (counter in 1:length(rv$resultList)){
            print(paste0(Sys.time()," : ",names(rv$resultList)[counter]))
            if (is.function(updateProgressor)){
              updateProgressor(step = NULL,
                               message = paste0("Table : ",names(rv$resultList)[counter]))
            }
            if (counter %in% selectedSamples()){
              if (length(rv$resultList) != 0){
                if (length(rv$resultList[[counter]]) != 0){
                  refRRt <- refFixed()
                  if (!identical(refRRt, NA)){
                    refRRt <- as.numeric(refRRt$fixedRefRt)
                  }
                  rtAreaSettings <- createTableSettings()
                  resultPeaks <- getRRtArea(
                    theData = rv$resultList[[counter]],
                    closeRts = rtAreaSettings$rtArea$closeRts,
                    refcloseRts = rtAreaSettings$rtArea$closeRRts,
                    fixedRrt = refRRt,
                    includeMissing = TRUE,
                    mzNr = NA
                  )
                  resultPeaks <- getMzs(
                    theData = rv$resultList[[counter]],
                    thePeaks = resultPeaks,
                    mzNr = NA,
                    setSingleRt = setSingleRtValue(),
                    limitPpm = rtAreaSettings$mzPpm$limitPpm,
                    peakIntensityLimit = rtAreaSettings$mzPpm$peakIntensityLimit
                  )
                  resultPeaks <- completeTable2Signals(resultPeaks)
                  rv$theData[[counter]]$result <- resultPeaks
                  names(rv$theData)[counter] <- names(rv$resultList)[counter]
                } 
              }
            }
            # progressor$close()
          }
        }
      })
      
      rtOptions <- selectWidgetsServer(id = "rtOptions",
                                       hideSelect = TRUE,
                                       widgets = c("rtTolerance", "rtClosest", "rtPeak"))
      
      refFixed <- selectWidgetsServer(id = "refFixed",
                                      hideSelect = FALSE,
                                      widgets = c("fixedRefRt"))
      
      rrtOptions <- selectWidgetsServer(id = "rrtOptions",
                                        hideSelect = TRUE,
                                        widgets = c("rtTolerance", "rtClosest", "rtPeak"))
      
      mzOptions <- selectWidgetsServer(id = "mzOptions",
                                       hideSelect = TRUE,
                                       widgets = c("useLimitRt", "limitRt",
                                                   "peakMzLimits", "peakClosest",
                                                   "peakIntensityLimit"))
      
      setSingleRt <- selectWidgetsServer(id = "setSingleRt",
                                         hideSelect = FALSE,
                                         widgets = c("setSingleRt"))
      
      setSingleRtValue <- reactive({
        if (identical(setSingleRt(), NA)){
          return(NA)
        } else {
          return(setSingleRt()$setSingleRt)
        }
      })
      
      createTableSettings <- reactive({     
        # note: useSmooth & setSingleRt are not included in this, this may be
        #       relevant in implementations where these options need to be saved
        mzOpt <- mzOptions()
        mzOpt$limitRt <- ifelse(mzOpt$useLimitRt, mzOpt$limitRt, as.logical(NA))
        rtOpt <- rtOptions()
        rrtOpt <- rrtOptions()
        return(
          list(
            rtArea = list(
              closeRts = list(left = -rtOpt$rtTolerance[1], right = rtOpt$rtTolerance[2],
                              closest = rtOpt$rtClosest, peak = rtOpt$rtPeak),
              closeRRts = list(left = -rrtOpt$rtTolerance[1], right = rrtOpt$rtTolerance[2],
                               closest = rrtOpt$rtClosest, peak = rrtOpt$rtPeak),
              mzNr = NA),
            mzPpm = list(limitRt = ifelse(mzOpt$useLimitRt, mzOpt$limitRt, NA),
                         limitPpm = c(-mzOpt$peakMzLimits[1], mzOpt$peakMzLimits[2]),
                         peakClosest = mzOpt$peakClosest,
                         peakIntensityLimit = mzOpt$peakIntensityLimit)
          )
        )
      })
      
      returnValue <- reactive({
        return( # swap resultList & resultData
          list(resultData = rv$resultList,
               resultList = rv$theData)
        )
      })
      
      anyComplete <- function(results){
        whichCompleteList <- which(unname(purrr::map_lgl(results$resultList, ~length(.x) != 0)))
        return(whichCompleteList)
      }
      
      completeData <- reactive({
        if ((length(rv$resultList) != 0) & (length(rv$theData) != 0)){
          return(length(anyComplete(results = returnValue())) !=0)
        }
        return(FALSE)
      })
      
      rv$sendData <- FALSE
      
      observeEvent(completeData(),{
        rv$sendData <- FALSE
        if (completeData()){
          shinyjs::enable("sendData")
        } else {
          shinyjs::disable("sendData")
        }
      })
      
      observeEvent(input$sendData,{
        rv$sendData <- TRUE
      })
      
      returnValue1 <- reactive({
        if (completeData() & rv$sendData){
          return( # swap resultList & resultData
            list(resultList = rv$theData[anyComplete(results = returnValue())])
          )
        } else {
          return( # swap resultList & resultData
            list(resultList = list())
          )
        }
      })
      
      return(returnValue1)
      
    }
  )
}

# ---- minimal functional app ----
# In this case it cannot do anything, because no files are defined


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
      processUI(insideTagsDiv = "boxMiddle"),
      hr()
    )
    
    server <- function(input, output, server){
      
      test <- processServer(
        theTargets = reactive({return(list(names = c("Test 1", "Test 2")))})
      )
      
      observeEvent(test(),{
        print(test())
      })
    }
    
    shinyApp(ui = ui, server = server)
    
  },
  error = function(e){}
)
