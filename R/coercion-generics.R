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


#' S3 generics used internally by \pkg{ResIN} for graph coercion.
#' End users will usually call the class-specific methods (e.g.,
#' \code{\link{as.tidygraph.ResIN}}).
#'
#' @name ResIN-coercion-generics
#' @keywords internal
NULL

#' @rdname ResIN-coercion-generics
#' @export
as.tidygraph <- function(x, ...) UseMethod("as.tidygraph")

#' @exportS3Method as.tidygraph default
#' @noRd
as.tidygraph.default <- function(x, ...) {
  stop(
    "No as.tidygraph() method for objects of class: ",
    paste(class(x), collapse = "/"),
    call. = FALSE
  )
}
