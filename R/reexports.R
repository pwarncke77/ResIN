#' Re-exported functions used by ResIN
#'
#' These functions are re-exported so users can call common coercion generics
#' (e.g., \code{as.igraph()} and \code{as.network()}) directly after loading
#' \pkg{ResIN}, with S3 dispatch to \code{ResIN} methods.
#'
#' @name ResIN-reexports
#' @keywords internal
NULL

#' @importFrom igraph as.igraph
#' @export
#' @rdname ResIN-reexports
igraph::as.igraph

#' @importFrom network as.network
#' @export
#' @rdname ResIN-reexports
network::as.network
