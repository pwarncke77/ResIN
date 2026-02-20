#' @title (Depricated.) Convert ResIN networks to Gephi-readable csv tables. Use \code{as.gephi()} method instead.
#'
#' @description Deprecated/legacy function. Saves a ResIN graph as a series of csv files readable by Gephi. Now supplanted by \code{as.gephi()} method.
#'
#' @param ResIN_object The output of the ResIN function (a list with class ResIN).
#' @param file The name with .csv extension for the Gephi readable file to be output at. Defaults to "ResIN_gephi.csv".
#' @param edges_only Logical; if TRUE write/return only edges.
#' @param dont_save_csv Logical; set TRUE to disable writing.
#'
#' @return A series of csv files readable by Gephi
#'
#' @examples
#' ## Load the 12-item simulated Likert-type ResIN toy dataset
#' data(lik_data)
#'
#' ## Estimate a ResIN network
#' res <- ResIN(lik_data, generate_ggplot = FALSE)
#'
#' ## Create Gephi edge table without writing files
#' edges <- as.gephi(res, dont_save_csv = TRUE)
#' head(edges)
#'
#' \dontrun{
#' ## Write CSV file(s) for import to Gephi
#' ## (writes "ResIN_gephi.csv" by default)
#' as.gephi(res, file = "ResIN_gephi.csv")
#'
#' ## Write both edges and nodes tables
#' ## (writes "ResIN_gephi_edges.csv" and "ResIN_gephi_nodes.csv")
#' as.gephi(res, file = "ResIN_gephi.csv", edges_only = FALSE)
#' }
#'
#'
#' @export
#' @importFrom readr "write_csv"
#' @references Source code of original function (< version 2.2.0) had been adapted from: https://github.com/RMHogervorst/gephi?tab=MIT-1-ov-file#readme
#'

ResIN_to_gephi <- function(ResIN_object, file = "ResIN_gephi.csv", edges_only = TRUE, dont_save_csv = FALSE) {
  .Deprecated("as.gephi", package = "ResIN",
              msg = "ResIN_to_gephi() is deprecated; use as.gephi(x, file=...) instead.")
  as.gephi(ResIN_object, file = file, edges_only = edges_only, dont_save_csv = dont_save_csv)
}
