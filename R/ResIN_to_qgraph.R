#' @title (Deprecated.) Convert a ResIN network into an qgraph object. Use \code{as.qgraph()} method instead.
#'
#' @description Deprecated/legacy function. Transforms the output of the \code{ResIN} function into an \code{qgraph} object. Use \code{as.qgraph()} method instead.
#'
#' @param ResIN_object the output of the ResIN function (a list with class ResIN).
#' @param qgraph_arglist an optional argument list to be supplied to the igraph::graph_from_adjacency_matrix function. If NULL, defaults are: list(layout = "spring", maximum = 1, vsize = 6, DoNotPlot = TRUE, sampleSize = nrow(df_nodes), mar = c(3,3,3,3), normalize = FALSE)
#'
#' @return A [qgraph]https://cran.r-project.org/web/packages/qgraph/index.html graph object.
#'
#' @examples
#' ## Load the 12-item simulated Likert-type ResIN toy dataset
#' data(lik_data)
#'
#' ## Run the function:
#' ResIN_qgraph <-  as.qgraph(ResIN(lik_data, plot_ggplot = FALSE))
#'
#' class(ResIN_qgraph)
#'
#' @export
#' @importFrom qgraph "qgraph" "cor_auto" "centrality_auto" "EBICglasso" "qgraph.layout.fruchtermanreingold"
#' @seealso \code{\link[=as.qgraph.ResIN]{as.qgraph}} as the recommended interface.
#' @references Epskamp S, Cramer AOJ, Waldorp LJ, Schmittmann VD, Borsboom D (2012). “qgraph: Network Visualizations of Relationships in Psychometric Data.” Journal of Statistical Software, 48(4), 1–18.
#'

ResIN_to_qgraph <- function(ResIN_object, qgraph_arglist = NULL) {

  .Deprecated("as.qgraph", package = "ResIN",
              msg = "ResIN_to_qgraph() is deprecated; use as.qgraph(x, ...) instead.")

  if (is.null(qgraph_arglist)) {
    qgraph_arglist <- list(
      layout = "spring",
      maximum = 1,
      vsize = 6,
      DoNotPlot = TRUE,
      sampleSize = nrow(ResIN_object$aux_objects$df_dummies),
      title = "ResIN graph in qgraph",
      mar = c(3, 8, 3, 8),
      normalize = FALSE
    )
  }

  do.call(as.qgraph, c(list(ResIN_object), qgraph_arglist))
}
