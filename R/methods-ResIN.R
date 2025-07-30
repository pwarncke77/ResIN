#' Print a ResIN object
#'
#' @param x A ResIN object.
#' @param ... Ignored.
#' @export
print.ResIN <- function(x, ...) {
  cat("<ResIN object>\n")
  if (is.data.frame(x$ResIN_edgelist)) {
    cat(sprintf("  ResIN_edgelist : %d x %d\n",
                nrow(x$ResIN_edgelist), ncol(x$ResIN_edgelist)))
  } else cat("  ResIN_edgelist : not available\n")

  if (is.data.frame(x$ResIN_nodeframe)) {
    cat(sprintf("  ResIN_nodeframe: %d x %d\n",
                nrow(x$ResIN_nodeframe), ncol(x$ResIN_nodeframe)))
  } else cat("  ResIN_nodeframe: not available\n")

  if (is.data.frame(x$ResIN_scores)) {
    cat(sprintf("  ResIN_scores   : %d x %d\n",
                nrow(x$ResIN_scores), ncol(x$ResIN_scores)))
  } else cat("  ResIN_scores   : %s\n", as.character(x$ResIN_scores)[1])

  cat(sprintf("  ResIN_ggplot   : %s\n",
              if (inherits(x$ResIN_ggplot, "ggplot")) "ggplot object" else as.character(x$ResIN_ggplot)[1]))
  cat(sprintf("  graph_stats    : list(%s)\n", paste0(length(x$graph_stats), " elements")))
  cat(sprintf("  aux_objects    : list(%s)\n", paste(names(x$aux_objects), collapse = ", ")))
  cat(sprintf("  bipartite_output: %s\n",
              if (is.list(x$bipartite_output)) "generated" else as.character(x$bipartite_output)[1]))
  invisible(x)
}

#' Summarize a ResIN object
#'
#' @param object A ResIN object.
#' @param ... Ignored.
#' @return An object of class \code{summary.ResIN}.
#' @export
summary.ResIN <- function(object, ...) {
  out <- list(
    n_edges      = if (is.data.frame(object$ResIN_edgelist)) nrow(object$ResIN_edgelist) else NA_integer_,
    n_nodes      = if (is.data.frame(object$ResIN_nodeframe)) nrow(object$ResIN_nodeframe) else NA_integer_,
    has_scores   = is.data.frame(object$ResIN_scores),
    has_plot     = inherits(object$ResIN_ggplot, "ggplot"),
    structuration = tryCatch(object$graph_stats[[1]], error = function(e) NULL),
    centralization = tryCatch(object$graph_stats[[2]], error = function(e) NULL),
    extras       = names(object$aux_objects)
  )
  class(out) <- c("summary.ResIN", "list")
  out
}

#' @export
print.summary.ResIN <- function(x, ...) {
  cat("<Summary of ResIN>\n")
  cat(sprintf("  Nodes : %s\n", x$n_nodes))
  cat(sprintf("  Edges : %s\n", x$n_edges))
  cat(sprintf("  Scores available : %s\n", if (isTRUE(x$has_scores)) "yes" else "no"))
  cat(sprintf("  Plot available   : %s\n", if (isTRUE(x$has_plot)) "yes" else "no"))
  if (is.list(x$structuration)) {
    cat("  Structuration:\n")
    print(x$structuration)
  }
  if (is.list(x$centralization)) {
    cat("  Centralization:\n")
    print(x$centralization)
  }
  if (length(x$extras)) {
    cat("  Aux objects:", paste(x$extras, collapse = ", "), "\n")
  }
  invisible(x)
}

#' Plot a ResIN object
#'
#' @param x A ResIN object.
#' @param which Which plot to show: \code{"network"} (default) or \code{"bipartite"}.
#' @param print_plot Logical; print the plot (TRUE) or just return it (FALSE).
#' @param ... Ignored.
#' @return The plot object (invisibly if \code{print_plot = TRUE}).
#' @importFrom graphics plot
#' @export
plot.ResIN <- function(x, which = c("network", "bipartite"), print_plot = TRUE, ...) {
  which <- match.arg(which)

  p <- switch(which,
              network = {
                if (!inherits(x$ResIN_ggplot, "ggplot"))
                  stop("No ggplot stored in $ResIN_ggplot. Set generate_ggplot = TRUE in ResIN().", call. = FALSE)
                x$ResIN_ggplot
              },
              bipartite = {
                if (!is.list(x$bipartite_output) || !"bipartite_ggraph" %in% names(x$bipartite_output))
                  stop("Bipartite output not generated. Call ResIN(..., bipartite = TRUE).", call. = FALSE)
                x$bipartite_output$bipartite_ggraph
              }
  )

  if (print_plot) print(p)
  invisible(p)
}

#' Autoplot for ResIN objects
#'
#' Produces the ggplot stored in \code{$ResIN_ggplot}.
#'
#' @param object A ResIN object.
#' @param ... Passed on (currently ignored).
#' @return A \code{ggplot} object.
#' @importFrom ggplot2 autoplot
#' @export
autoplot.ResIN <- function(object, ...) {
  if (!inherits(object$ResIN_ggplot, "ggplot")) {
    stop("No ggplot stored in $ResIN_ggplot.", call. = FALSE)
  }
  object$ResIN_ggplot
}

#' @export
as.data.frame.ResIN <- function(x,
                                row.names = NULL,
                                optional = FALSE,
                                which = c("edges", "nodes", "scores"),
                                ...) {
  if (length(which) > 1L) {
    stop("Please supply a single value for 'which': 'edges', 'nodes', or 'scores'.", call. = FALSE)
  }
  which <- match.arg(which)
  out <- switch(which,
                nodes  = x$ResIN_nodeframe,
                edges  = x$ResIN_edgelist,
                scores = x$ResIN_scores
  )
  if (!is.data.frame(out)) {
    stop(sprintf("Component '%s' is not available as a data frame.", which), call. = FALSE)
  }
  out
}

#' @importFrom utils head
#' @export
head.ResIN <- function(x, n = 6L, which = c("nodes","edges","scores"), ...) {
  which <- match.arg(which)
  df <- as.data.frame(x, which = which)
  utils::head(df, n)
}

#' @importFrom utils tail
#' @export
tail.ResIN <- function(x, n = 6L, which = c("edges","nodes","scores"), ...) {
  which <- match.arg(which)
  df <- as.data.frame(x, which = which)
  utils::tail(df, n)
}
