#' @title locationSort
#' 
#' @description
#'  Helper function to 'translate' a character vector of numbers to an integer
#'  vector. It handles the special case '277/279' by 'translating' it to 277L
#'  and a blank ('') to 0L
#' 
#'
#' @param locations character vector of integer numbers. If one
#'  element is '277/279' it gets transformed to 277. This to be able to use the
#'  vector for sorting purposes (the sorting is NOT done in this function).
#'
#' @returns integer vector
#' @export
#'
#' @examples
#' locationSort(locations = c("277/279", "192" ,"55"))
locationSort <- function(locations){
  result <- ifelse(locations == "277/279",
                   277, 
                   ifelse(locations == "",
                          0,
                          locations))
  return(as.integer(result))
}

#' @title ModificationSort
#'
#' @description
#'  helper function that 'translates' a character vector to a character vector
#'  of modifications to integer numbers (as character numbers) for predefined
#'  sorting order (no sorting takes place in this function). If an element is a
#'  blank ('') it gets 'translated' to 0:
#'
#' @param modifications character vector with elements '', 'DON', 'DON82' and/or
#'  'DON100'
#'
#' @returns integer vector
#' @export
#'
#' @examples
#' ModificationSort(c("DON82","","DON100", "DON"))
ModificationSort <- function(modifications){
  result <- ifelse(modifications == "",
                   "0",
                   ifelse(modifications == "DON",
                          "1",
                          ifelse(modifications == "DON82",
                                 "3","2")))
  return(as.integer(result))
}

#' @title showTable
#'
#' @description
#'  transforms the end result of the application into a (gt) html table
#' 
#'
#' @param theTable the table to be transformed into the html table. Note: must
#'  have the columns 'Control', 'Location', 'Modification', 
#'  'Reference m/z (Da)', 'm/z (Da)', 'Reference Retention Time (min)' and
#'  'Retention Time (min)'. The rest is optional
#' @param useTitle if not NA (default), then useTitle is used as the title
#'  (header) of the (gt) html table
#'
#' @returns gt table object
#' @export
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