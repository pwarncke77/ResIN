#' Internal coercion generics
#'
#' These functions are S3 generics used for object coercion in \pkg{ResIN}.
#' End users should typically consult the class methods:
#' \code{\link{as.igraph.ResIN}}, \code{\link{as.qgraph.ResIN}}, and \code{\link{as.gephi.ResIN}}.
#'
#' @name ResIN-coercion-generics
#' @keywords internal
NULL

#' @rdname ResIN-coercion-generics
#' @export
as.qgraph <- function(x, ...) UseMethod("as.qgraph")

#' @rdname ResIN-coercion-generics
#' @export
as.gephi <- function(x, ...) UseMethod("as.gephi")
