#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  targetFilesName <- "inst/app/data/targetsList.csv"
  if (file.exists(targetFilesName)){
    targetFiles <<- utils::read.table(targetFilesName,
                                      sep = ";",
                                      header = T)
  } else { # to fool test-golem-recommended.R during tests
    targetFiles <<- data.frame(file = "1",
                               name = "1")
  }
  titleText <- "DON-OVA Peptide Mapping V2.0"
  contactPreText <- "Questions/Comments: "
  contactName <- "Ben Bruyneel"
  contactEmail <- "ben.bruyneel@merck.com"
  contactText <- paste(c(contactPreText,
                         "<a href = mailto:",
                         contactEmail,">",
                         contactName,"</a>"), collapse ="")
  tagList(
    shinyjs::useShinyjs(),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      fluidRow(
        column(2,
               img(src='www/msd_ah_rgb_pos.png',
                   height = "75px",# width = "150px",
                   align = "left")
        ),
        column(7,
               titlePanel(titleText)
        ),
        column(3,
               style = "text-align: right; padding-top: 10px;",
               HTML(contactText)
        )
      ),
      agilentDataUI(insideTagsDiv = "boxMiddle",
                    theTargets = targetFiles,
                    admin = TRUE)
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  add_resource_path(
    "data",
    app_sys("app/data")
  )
  
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "AgilentDataV2"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    bundle_resources(
      path = app_sys("app/data"),
      app_title = "AgilentDataV2"
    ),
    
  )
}
