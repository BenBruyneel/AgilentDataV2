#' Title
#'
#' @param locations 
#'
#' @returns
#' @export
#'
#' @examples
locationSort <- function(locations){
  result <- ifelse(locations == "277/279",
                   277, 
                   ifelse(locations == "",
                          0,
                          locations))
  return(as.integer(result))
}

#' Title
#'
#' @param modifications 
#'
#' @returns
#' @export
#'
#' @examples
ModificationSort <- function(modifications){
  result <- ifelse(modifications == "",
                   "0",
                   ifelse(modifications == "DON",
                          "1",
                          ifelse(modifications == "DON82",
                                 "3","2")))
  result <- as.integer(result)
}

#' Title
#'
#' @param theTable 
#' @param useTitle 
#'
#' @returns
#' @export
#'
#' @examples
showTable <- function(theTable, useTitle = NA){
  result <- theTable |>
    dplyr::arrange(desc(Control), locationSort(Location), ModificationSort(Modification)) %>%
    gt::gt() |>
    gt::sub_missing() |>
    gt::fmt_number(
      columns = c("Reference m/z (Da)", "m/z (Da)"),
      decimals = 4,
      sep_mark = ""
    ) |>
    gt::fmt_number(
      columns = c("Reference Retention Time (min)",
                  "Retention Time (min)"),
      decimals = 2,
      sep_mark = ""
    ) |>
    gt::tab_style(style = list(gt::cell_text(weight = "bold")), locations = gt::cells_column_labels()) %>%
    gt::opt_table_font(font = "invention") %>%
    gt::tab_options(
      table.align = "left",
      table.font.size = 13,
      data_row.padding = 1
    )
  if (!identical(useTitle, NA)){
    result <- result |>
      gt::tab_header(title = useTitle)
  }
  return(result)
}