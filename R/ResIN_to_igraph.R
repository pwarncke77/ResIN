#' @title ResIN_to_igraph
#'
#' @description Transforms the output of the ResIN function into an [igraph](https://igraph.org/r/doc/cluster_leading_eigen.html) object
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
#' \donttest{
#' igraph_output <-  ResIN_to_igraph(ResIN(lik_data))
#'
#'
#' ## Plot and/or investigate as you wish:
#' igraph::plot.igraph(igraph_output)
#' }
#'
#' @export
#' @importFrom igraph "graph_from_adjacency_matrix"
#'
#' @references Csardi G, Nepusz T (2006). “The igraph software package for complex network research.” InterJournal, Complex Systems, 1695. https://igraph.org.

ResIN_to_igraph <- function(ResIN_object, igraph_arglist = NULL) {
  ## Test for ResIN object
  if(class(ResIN_object)[2] !=  "ResIN"){
    stop("Please supply a ResIN type list object.")
  }
  ## Generating the igraph object
  if(is.null(igraph_arglist)) {
    igraph_arglist <- list(mode = "undirected", weighted = TRUE, diag = FALSE)
  }
  res_in_graph_igraph <- do.call(igraph::graph_from_adjacency_matrix, c(list(adjmatrix = ResIN_object$aux_objects$adj_matrix), igraph_arglist))
  return(res_in_graph_igraph)
}
