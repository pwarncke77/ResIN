#' @title (Deprecated.) Convert a ResIN network into an igraph object. Use \code{as.igraph()} method instead.
#'
#' @description Deprecated/legacy function. Transforms the output of the \code{ResIN} function into an [igraph](https://igraph.org/r/doc/cluster_leading_eigen.html) object. Now simply a wrapper for the \code{as.igraph()} method.
#'
#' @param ResIN_object the output of the ResIN function (a list with class ResIN).
#' @param igraph_arglist an optional argument list to be supplied to the igraph::graph_from_adjacency_matrix function. If NULL, default is: list(mode = "undirected", weighted = TRUE, diag = FALSE).
#'
#' @return A class \code{igraph} object.
#'
#' @examples
#'
#' ## Load the 12-item simulated Likert-type ResIN toy dataset
#' data(lik_data)
#'
#' ## Run the function:
#'
#' igraph_output <-  as.igraph(ResIN(lik_data, plot_ggplot = FALSE))
#'
#' class(igraph_output)
#'
#' ## Plot and/or investigate as you wish:
#' \donttest{
#' igraph::plot.igraph(igraph_output)
#' }
#'
#'
#' @export
#' @importFrom igraph "graph_from_adjacency_matrix"
#'
#' @references Csardi G, Nepusz T (2006). “The igraph software package for complex network research.” InterJournal, Complex Systems, 1695. https://igraph.org.
#' @seealso \code{\link[=as.igraph.ResIN]{as.igraph}} as the recommended interface.
ResIN_to_igraph <- function(ResIN_object, igraph_arglist = NULL) {

  .Deprecated("as.igraph", package = "ResIN",
              msg = "Note: ResIN_to_igraph() is deprecated as of Version 2.2.2; please use as.igraph(x, ...) instead.")

  if (is.null(igraph_arglist)) {
    igraph_arglist <- list(mode = "undirected", weighted = TRUE, diag = FALSE)
  }

  do.call(as.igraph, c(list(ResIN_object), igraph_arglist))
}
