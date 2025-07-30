#' @title ResIN_to_qgraph
#'
#' @description Transforms the output of the ResIN function into an \code{qgraph} object
#'
#' @param ResIN_object the output of the ResIN function (a list with class ResIN).
#' @param qgraph_arglist an optional argument list to be supplied to the igraph::graph_from_adjacency_matrix function. If NULL, defaults are: list(layout = "spring", maximum = 1, vsize = 6, DoNotPlot = TRUE, sampleSize = nrow(df_nodes), mar = c(3,3,3,3), normalize = FALSE)
#'
#' @return A [qgraph]https://cran.r-project.org/web/packages/qgraph/index.html graph object.
#'
#' @examples
#'
#' \donttest{
#' ## Load the 12-item simulated Likert-type ResIN toy dataset
#' data(lik_data)
#'
#' ## Run the function:
#' ResIN_qgraph <-  ResIN_to_qgraph(ResIN(lik_data))
#' }
#'
#' @export
#' @importFrom qgraph "qgraph" "cor_auto" "centrality_auto" "EBICglasso" "qgraph.layout.fruchtermanreingold"
#'
#' @references Epskamp S, Cramer AOJ, Waldorp LJ, Schmittmann VD, Borsboom D (2012). “qgraph: Network Visualizations of Relationships in Psychometric Data.” Journal of Statistical Software, 48(4), 1–18.
#'

ResIN_to_qgraph <- function(ResIN_object, qgraph_arglist = NULL) {
  ## Test for ResIN object
  if(class(ResIN_object)[1] !=  "ResIN"){
    stop("Please supply a ResIN type list object.")
  }
  if(is.null(qgraph_arglist)) {
    qgraph_arglist <- list(layout = "spring", maximum = 1, vsize = 6,
                           DoNotPlot = TRUE, sampleSize = nrow(ResIN_object$aux_objects$df_dummies),
                           title = "ResIN graph in qgraph", mar = c(2,2,2,2),
                           normalize = FALSE)
    }
  res_in_graph_qgraph <- do.call(qgraph::qgraph, c(list(input = ResIN_object$aux_objects$adj_matrix), qgraph_arglist))
  return(res_in_graph_qgraph)
}

