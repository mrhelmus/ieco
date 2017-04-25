#' General functions to check data for errors
#' @name data_check
#' @description functions that check the statistics of data looking for outliers normality etc. 
#' @param x character object of the title of the sheet to load
#' @param y character vector of sheet titles to bind
#' @details bindsheet.morph assumes that sheets to bind have the same columns and is only for herp morphology datasheets
#' @return concatenated datatable
#' @author Matthew R. Helmus
# @examples None None 
# @seealso None None
# @references None None
#' @rdname sheet.data
#' @export
#' @import googlesheets
#' @import tibble
