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
  } else {
    cat(sprintf("  ResIN_scores   : %s\n", as.character(x$ResIN_scores)[1]))
  }

  cat(sprintf("  ResIN_ggplot   : %s\n",
              if (inherits(x$ResIN_ggplot, "ggplot")) "ggplot object" else as.character(x$ResIN_ggplot)[1]))
  cat(sprintf("  graph_stats    : list(%s)\n", paste0(length(x$graph_stats), " elements")))
  cat(sprintf("  aux_objects    : list(%s)\n", paste(names(x$aux_objects), collapse = ", ")))
  cat(sprintf("  bipartite_output: %s\n",
              if (is.list(x$bipartite_output)) "generated" else as.character(x$bipartite_output)[1]))

  if (is.list(x$aux_objects$meta)) {
    cat(sprintf("  df_id         : %s\n", x$aux_objects$meta$df_id))
    cat(sprintf("  ResIN_version  : %s\n", x$aux_objects$meta$ResIN_version))
  }

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
#' @noRd
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
#' @noRd
head.ResIN <- function(x, n = 6L, which = c("nodes","edges","scores"), ...) {
  which <- match.arg(which)
  df <- as.data.frame(x, which = which)
  utils::head(df, n)
}

#' @importFrom utils tail
#' @export
#' @noRd
tail.ResIN <- function(x, n = 6L, which = c("edges","nodes","scores"), ...) {
  which <- match.arg(which)
  df <- as.data.frame(x, which = which)
  utils::tail(df, n)
}

#' ResIN_boots_prepped S3 methods:
#'
#' @export
#' @noRd
print.ResIN_boots_prepped <- function(x, ...) {
  cat("<ResIN bootstrap plan>\n")
  cat("  type:         ", x$boots_type, "\n", sep = "")
  cat("  iterations:   ", x$n, "\n", sep = "")
  cat("  resample_size:", x$resample_size, "\n", sep = "")
  cat("  weights:      ", if (is.null(x$weights)) "none" else "provided", "\n", sep = "")
  cat("  save_input:   ", x$save_input, "\n", sep = "")
  cat("  seed:         ", x$seed_boots, "\n", sep = "")
  invisible(x)
}
#'
#' @export
#' @noRd
summary.ResIN_boots_prepped <- function(object, ...) {
  out <- list(
    boots_type = object$boots_type,
    n = object$n,
    resample_size = object$resample_size,
    has_weights = !is.null(object$weights),
    save_input = object$save_input,
    seed_boots = object$seed_boots
  )
  class(out) <- c("summary.ResIN_boots_prepped", "list")
  out
}
#'
#' @export
#' @noRd
print.summary.ResIN_boots_prepped <- function(x, ...) {
  cat("<Summary: ResIN bootstrap plan>\n")
  cat("  type:         ", x$boots_type, "\n", sep = "")
  cat("  iterations:   ", x$n, "\n", sep = "")
  cat("  resample_size:", x$resample_size, "\n", sep = "")
  cat("  weights:      ", if (x$has_weights) "yes" else "no", "\n", sep = "")
  cat("  save_input:   ", x$save_input, "\n", sep = "")
  cat("  seed:         ", x$seed_boots, "\n", sep = "")
  invisible(x)
}
#'
#' @export
#' @noRd
`[.ResIN_boots_prepped` <- function(x, i, ...) {
  if (missing(i)) return(x)
  x$iter_seeds <- x$iter_seeds[i]
  x$n <- length(x$iter_seeds)
  class(x) <- c("ResIN_boots_prepped", "list")
  x
}
#'
#' @export
#' @noRd
length.ResIN_boots_prepped <- function(x) x$n


#' ResIN_boots_executed S3 methods:
#'
#' @export
#' @noRd
print.ResIN_boots_executed <- function(x, ...) {
  plan <- attr(x, "plan")
  n <- length(x)

  cat("<ResIN bootstrap results>\n")
  cat("  iterations:   ", n, "\n", sep = "")

  if (inherits(plan, "ResIN_boots_prepped")) {
    cat("  type:         ", plan$boots_type, "\n", sep = "")
    cat("  resample_size:", plan$resample_size, "\n", sep = "")
    cat("  df_id:        ", if (!is.null(plan$df_id)) plan$df_id else "<not stored>", "\n", sep = "")
    cat("  ResIN_version:", if (!is.null(plan$ResIN_version)) plan$ResIN_version else "<not stored>", "\n", sep = "")
  } else {
    cat("  plan:         <not attached>\n")
  }

  has_inputs <- !is.null(attr(x, "boot_inputs"))
  cat("  saved inputs: ", if (has_inputs) "yes" else "no", "\n", sep = "")

  created <- attr(x, "created")
  if (!is.null(created)) cat("  created:      ", format(created), "\n", sep = "")

  invisible(x)
}

#' @export
#' @noRd
length.ResIN_boots_executed <- function(x) base::length(unclass(x))

#' @export
#' @noRd
`[.ResIN_boots_executed` <- function(x, i, ...) {
  if (missing(i)) return(x)

  y <- unclass(x)[i]

  class(y) <- class(x)

  attr(y, "plan") <- attr(x, "plan")
  attr(y, "created") <- attr(x, "created")

  bi <- attr(x, "boot_inputs")
  if (!is.null(bi)) attr(y, "boot_inputs") <- bi[i]

  y
}

#' @export
#' @noRd
summary.ResIN_boots_executed <- function(object, ...) {
  n <- length(object)

  ok <- attr(object, "ok", exact = TRUE)

  # Prefer the stored ok-vector (fast + consistent with execute())
  if (is.logical(ok) && length(ok) == n && !anyNA(ok)) {
    n_ok <- sum(ok)
    n_fail <- sum(!ok)
  } else {
    # Fallback for older objects (or if attributes were stripped)
    is_resin <- vapply(object, inherits, logical(1), what = "ResIN")
    n_ok <- sum(is_resin)
    n_fail <- n - n_ok
    ok <- is_resin
  }

  plan <- attr(object, "plan", exact = TRUE)

  out <- list(
    n = n,
    n_ok = n_ok,
    n_failed = n_fail,
    boots_type = if (inherits(plan, "ResIN_boots_prepped")) plan$boots_type else NA_character_,
    resample_size = if (inherits(plan, "ResIN_boots_prepped")) plan$resample_size else NA_integer_,
    saved_inputs = !is.null(attr(object, "boot_inputs", exact = TRUE)),
    df_id = if (inherits(plan, "ResIN_boots_prepped")) plan$df_id else NULL,
    ResIN_version = if (inherits(plan, "ResIN_boots_prepped")) plan$ResIN_version else NULL,
    created = attr(object, "created", exact = TRUE),
    ok = ok
  )

  class(out) <- c("summary.ResIN_boots_executed", "list")
  out
}

#' @export
#' @noRd
print.summary.ResIN_boots_executed <- function(x, ...) {
  cat("<Summary: ResIN bootstrap results>\n")
  cat("  iterations:   ", x$n, "\n", sep = "")
  cat("  succeeded:    ", x$n_ok, "\n", sep = "")
  cat("  failed:       ", x$n_failed, "\n", sep = "")
  if (!is.na(x$boots_type)) {
    cat("  type:         ", x$boots_type, "\n", sep = "")
    cat("  resample_size:", x$resample_size, "\n", sep = "")
  }
  cat("  saved inputs: ", if (x$saved_inputs) "yes" else "no", "\n", sep = "")
  if (!is.null(x$df_id)) cat("  df_id:        ", x$df_id, "\n", sep = "")
  if (!is.null(x$ResIN_version)) cat("  ResIN_version:", x$ResIN_version, "\n", sep = "")
  if (!is.null(x$created)) cat("  created:      ", format(x$created), "\n", sep = "")
  invisible(x)
}

## ResIN_boots_extract S3 methods:

#' @export
#' @noRd
print.ResIN_boots_draws <- function(x, ...) {
  what <- attr(x, "what", exact = TRUE)
  n_ok <- attr(x, "n_ok", exact = TRUE)
  n_failed <- attr(x, "n_failed", exact = TRUE)

  cat("<ResIN bootstrap draws>\n")
  cat("  what:     ", what, "\n", sep = "")
  cat("  n_ok:     ", n_ok, "\n", sep = "")
  cat("  n_failed: ", n_failed, "\n", sep = "")

  qs <- stats::quantile(x, probs = c(.025, .5, .975), na.rm = TRUE)
  cat(sprintf("  median:   %.4f\n", qs[[2]]))
  cat(sprintf("  95%% CI:   [%.4f, %.4f]\n", qs[[1]], qs[[3]]))
  invisible(x)
}

#' @export
#' @noRd
summary.ResIN_boots_draws <- function(object, probs = c(.025, .05, .25, .5, .75, .95, .975), ...) {
  qs <- stats::quantile(object, probs = probs, na.rm = TRUE)
  out <- c(
    n_ok = attr(object, "n_ok", exact = TRUE),
    n_failed = attr(object, "n_failed", exact = TRUE),
    min = min(object, na.rm = TRUE),
    mean = mean(object, na.rm = TRUE),
    sd = stats::sd(object, na.rm = TRUE),
    max = max(object, na.rm = TRUE),
    qs
  )
  out
}

#' @export
#' @noRd
confint.ResIN_boots_draws <- function(object, parm = NULL, level = 0.95, ...) {
  alpha <- (1 - level) / 2
  stats::quantile(object, probs = c(alpha, 1 - alpha), na.rm = TRUE, names = TRUE)
}

#' @export
#' @noRd
plot.ResIN_boots_draws <- function(x, level = 0.95, breaks = "Sturges", main = NULL, xlab = NULL, ...) {
  what <- attr(x, "what", exact = TRUE)
  if (is.null(main)) main <- paste0("Bootstrap draws: ", what)
  if (is.null(xlab)) xlab <- what

  graphics::hist(x, breaks = breaks, main = main, xlab = xlab, ...)

  ci <- confint.ResIN_boots_draws(x, level = level)
  graphics::abline(v = ci, lty = 2)
  invisible(ci)
}

#' @export
#' @noRd
`[.ResIN_boots_draws` <- function(x, i, ...) {
  y <- unclass(x)[i]
  class(y) <- class(x)
  attributes(y)[setdiff(names(attributes(x)), c("names", "dim", "dimnames"))] <-
    attributes(x)[setdiff(names(attributes(x)), c("names", "dim", "dimnames"))]
  attr(y, "n_ok") <- length(y)
  y
}

#' @export
#' @noRd
as.data.frame.ResIN_boots_draws <- function(x, ...) {
  data.frame(
    iter = seq_along(x),
    value = as.numeric(x),
    what = attr(x, "what", exact = TRUE),
    row.names = NULL
  )
}

#' @title Coerce a ResIN object to an igraph graph
#'
#' @description
#' Converts a \code{ResIN} object to an \code{igraph} graph using the adjacency
#' matrix stored in \code{x$aux_objects$adj_matrix}.
#'
#' @param x A \code{ResIN} object.
#' @param mode,weighted,diag Passed to \code{igraph::graph_from_adjacency_matrix()}.
#' @param ... Additional arguments passed to \code{igraph::graph_from_adjacency_matrix()}.
#'
#' @return An \code{igraph} object.
#'
#' @examples
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
#' @export
#' @importFrom igraph graph_from_adjacency_matrix
as.igraph.ResIN <- function(x, mode = "undirected", weighted = TRUE, diag = FALSE, ...) {
  if (!inherits(x, "ResIN")) {
    stop("x must be a ResIN object.", call. = FALSE)
  }

  A <- x$aux_objects$adj_matrix
  if (is.null(A) || !is.matrix(A)) {
    stop("ResIN object does not contain a valid adjacency matrix in x$aux_objects$adj_matrix.", call. = FALSE)
  }

  igraph::graph_from_adjacency_matrix(adjmatrix = A, mode = mode, weighted = weighted, diag = diag, ...)
}

#' @title Coerce a ResIN object to a qgraph object
#'
#' @description
#' Converts a \code{ResIN} object to a \code{qgraph} object using the adjacency
#' matrix stored in \code{x$aux_objects$adj_matrix}.
#'
#' @param x A \code{ResIN} object.
#' @param layout,maximum,vsize,DoNotPlot,sampleSize,title,mar,normalize Passed to \code{qgraph::qgraph()}.
#' @param ... Additional arguments passed to \code{qgraph::qgraph()}.
#'
#' @return A \code{qgraph} object.
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
#' @importFrom qgraph qgraph
as.qgraph.ResIN <- function(x,
                            layout = "spring",
                            maximum = 1,
                            vsize = 6,
                            DoNotPlot = TRUE,
                            sampleSize = NULL,
                            title = "ResIN graph in qgraph",
                            mar = c(3, 8, 3, 8),
                            normalize = FALSE,
                            ...) {

  if (!inherits(x, "ResIN")) {
    stop("x must be a ResIN object.", call. = FALSE)
  }

  A <- x$aux_objects$adj_matrix
  if (is.null(A) || !is.matrix(A)) {
    stop("ResIN object does not contain a valid adjacency matrix in x$aux_objects$adj_matrix.", call. = FALSE)
  }

  if (is.null(sampleSize)) {
    dd <- x$aux_objects$df_dummies
    sampleSize <- if (is.data.frame(dd) || is.matrix(dd)) nrow(dd) else NA_integer_
  }

  qgraph::qgraph(
    input = A,
    layout = layout,
    maximum = maximum,
    vsize = vsize,
    DoNotPlot = DoNotPlot,
    sampleSize = sampleSize,
    title = title,
    mar = mar,
    normalize = normalize,
    ...
  )
}

#' qgraph generic helpers (only back-end relevant to resolve methods conflicts)
#'
#' @param x A ResIN object to coerce.
#' @param ... Passed to methods.
#' @export
#' @noRd
as.qgraph <- function(x, ...) UseMethod("as.qgraph")

#' @exportS3Method as.qgraph default
#' @noRd
as.qgraph.default <- function(x, ...) {
  stop("No as.qgraph() method for objects of class: ", paste(class(x), collapse = "/"),
       call. = FALSE)
}

#' @title Coerce a ResIN object to Gephi CSV table(s)
#'
#' @description
#' Produces Gephi-readable edge (and optionally node) tables from a \code{ResIN}
#' object and (by default) writes them to CSV. Set \code{dont_save_csv = TRUE}
#' to return tables without writing files.
#'
#' @param x A \code{ResIN} object.
#' @param file Output file name (legacy style).
#' @param edges_only Logical; if TRUE write/return only edges.
#' @param dont_save_csv Logical; set TRUE to disable writing.
#' @param weight_col Name of the edge-weight column in \code{x$ResIN_edgelist}.
#' @param ... Ignored.
#'
#' @examples
#' ## Load the 12-item simulated Likert-type ResIN toy dataset
#' data(lik_data)
#'
#' ## Estimate a ResIN network
#' res <- ResIN(lik_data, plot_ggplot = FALSE)
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
#' @return If \code{edges_only = TRUE}, an edge table data.frame. Otherwise a list with \code{edges} and \code{nodes}. “When \code{dont_save_csv = FALSE}, the return value is returned invisibly.”
#' @export
#' @importFrom readr write_csv
as.gephi.ResIN <- function(x,
                           file = "ResIN_gephi.csv",
                           edges_only = TRUE,
                           dont_save_csv = FALSE,
                           weight_col = "weight",
                           ...) {

  if (!inherits(x, "ResIN")) {
    stop("x must be a ResIN object.", call. = FALSE)
  }
  if (!is.data.frame(x$ResIN_edgelist)) {
    stop("ResIN object does not contain a valid edge list in x$ResIN_edgelist.", call. = FALSE)
  }

  el <- x$ResIN_edgelist

  from_col <- if ("from" %in% names(el)) "from" else grep("from", names(el), value = TRUE)[1]
  to_col   <- if ("to"   %in% names(el)) "to"   else grep("to",   names(el), value = TRUE)[1]
  if (is.na(from_col) || is.na(to_col)) {
    stop("Could not identify 'from'/'to' columns in ResIN_edgelist.", call. = FALSE)
  }

  w_col <- if (weight_col %in% names(el)) weight_col else NULL
  if (is.null(w_col) && ncol(el) >= 3L) w_col <- names(el)[3]

  edges <- data.frame(
    Source = el[[from_col]],
    Target = el[[to_col]],
    stringsAsFactors = FALSE
  )
  if (!is.null(w_col)) edges$Weight <- el[[w_col]]

  nodes <- NULL
  if (!isTRUE(edges_only)) {
    if (is.data.frame(x$ResIN_nodeframe)) {
      nf <- x$ResIN_nodeframe
      if (!"Id" %in% names(nf)) {
        if ("node_names" %in% names(nf)) nf$Id <- nf$node_names
        else if ("from" %in% names(nf))  nf$Id <- nf$from
        else nf$Id <- seq_len(nrow(nf))
      }
      nodes <- nf
    } else {
      nodes <- data.frame(Id = unique(c(edges$Source, edges$Target)))
    }
  }

  out <- if (isTRUE(edges_only)) edges else list(edges = edges, nodes = nodes)

  if (!isTRUE(dont_save_csv)) {

    if (isTRUE(edges_only)) {
      readr::write_csv(edges, file = file, na = "")
      created <- normalizePath(file, winslash = "/", mustWork = FALSE)

      message("Gephi edge table created: ", basename(created),
              " (", dirname(created), ")")

    } else {
      prefix <- sub("\\.csv$", "", file, ignore.case = TRUE)
      edges_file <- paste0(prefix, "_edges.csv")
      nodes_file <- paste0(prefix, "_nodes.csv")

      readr::write_csv(edges, file = edges_file, na = "")
      readr::write_csv(nodes, file = nodes_file, na = "")

      ef <- normalizePath(edges_file, winslash = "/", mustWork = FALSE)
      nf <- normalizePath(nodes_file, winslash = "/", mustWork = FALSE)

      message("Gephi tables created: ",
              basename(ef), " and ", basename(nf),
              " (", dirname(ef), ")")
    }
    return(invisible(out))
  }
  out
}

#' Generic (default) Gephi table method
#'
#' @param x An object to coerce.
#' @param ... Passed to methods.
#' @export
#' @noRd
as.gephi <- function(x, ...) UseMethod("as.gephi")

#' @exportS3Method as.gephi default
#' @noRd
as.gephi.default <- function(x, ...) {
  stop("No as.gephi() method for objects of class: ", paste(class(x), collapse = "/"),
       call. = FALSE)
}

#' @title Coerce a ResIN object to a tidygraph graph
#'
#' @description
#' Converts a \code{ResIN} object to a \code{tidygraph::tbl_graph} object while
#' preserving as much node- and edge-level information as possible from
#' \code{x$ResIN_nodeframe} and \code{x$ResIN_edgelist}.
#'
#' Because \code{tidygraph} stores edge endpoints as integer node indices, the
#' original edge endpoint labels are preserved in additional edge columns
#' \code{from_name} and \code{to_name}.
#'
#' If \code{ResIN_nodeframe} or \code{ResIN_edgelist} are unavailable, the method
#' falls back to a simpler conversion via \code{as.igraph()} followed by
#' \code{tidygraph::as_tbl_graph()}, which may not preserve all metadata.
#'
#' @param x A \code{ResIN} object.
#' @param directed Logical; should the resulting graph be treated as directed?
#'   Defaults to \code{FALSE}.
#' @param ... Ignored.
#'
#' @return A \code{tidygraph::tbl_graph} object. Node data include (when present)
#'   all columns from \code{x$ResIN_nodeframe}; edge data include (when present)
#'   all columns from \code{x$ResIN_edgelist}, plus \code{from_name}/\code{to_name}
#'   preserving original endpoint labels.
#'
#' @examples
#' ## Load toy data and estimate ResIN
#' data(lik_data)
#' res <- ResIN(lik_data, network_stats = TRUE, detect_clusters = TRUE,
#'              plot_ggplot = FALSE)
#'
#' ## Convert to tidygraph
#' tg <- as.tidygraph(res)
#' tg
#'
#' @export
#' @importFrom tidygraph tbl_graph as_tbl_graph
as.tidygraph.ResIN <- function(x, directed = FALSE, ...) {

  if (!inherits(x, "ResIN")) {
    stop("x must be a ResIN object.", call. = FALSE)
  }

  nf <- x$ResIN_nodeframe
  ef <- x$ResIN_edgelist

  # A-route: direct conversion preserving ResIN-generated node and edge metadata
  if (is.data.frame(nf) && is.data.frame(ef)) {

    # Validate node identifiers
    node_id_col <- if ("node_names" %in% names(nf)) {
      "node_names"
    } else if ("name" %in% names(nf)) {
      "name"
    } else {
      NA_character_
    }

    if (is.na(node_id_col)) {
      stop(
        "ResIN_nodeframe must contain a node identifier column ('node_names' or 'name').",
        call. = FALSE
      )
    }

    node_names <- as.character(nf[[node_id_col]])

    if (anyNA(node_names) || any(node_names == "")) {
      stop("Node identifier column contains missing/empty values.", call. = FALSE)
    }
    if (anyDuplicated(node_names)) {
      stop("Node identifier column contains duplicates; tidygraph conversion requires unique node names.",
           call. = FALSE)
    }

    # Validate edge endpoints
    if (!all(c("from", "to") %in% names(ef))) {
      stop("ResIN_edgelist must contain 'from' and 'to' columns.", call. = FALSE)
    }

    # Copy node table and add standard tidygraph key `name` (without dropping node_names)
    nodes <- nf
    if (!"name" %in% names(nodes)) {
      nodes$name <- node_names
    } else {
      # If `name` exists, ensure consistency / uniqueness
      nm <- as.character(nodes$name)
      if (anyNA(nm) || any(nm == "") || anyDuplicated(nm)) {
        # overwrite with node_names to ensure valid key
        nodes$name <- node_names
      }
    }

    # Copy edge table and preserve original labels
    edges <- ef
    if (!"from_name" %in% names(edges)) edges$from_name <- as.character(edges$from)
    if (!"to_name" %in% names(edges))   edges$to_name   <- as.character(edges$to)

    # Convert endpoints to integer indices expected by tbl_graph()
    idx_from <- match(as.character(edges$from), as.character(nodes$name))
    idx_to   <- match(as.character(edges$to),   as.character(nodes$name))

    if (anyNA(idx_from) || anyNA(idx_to)) {
      missing_from <- unique(as.character(edges$from)[is.na(idx_from)])
      missing_to   <- unique(as.character(edges$to)[is.na(idx_to)])
      msg <- paste0(
        "Could not match all edge endpoints to node names in ResIN_nodeframe.",
        if (length(missing_from)) paste0(" Unmatched 'from': ", paste(utils::head(missing_from, 5), collapse = ", "),
                                         if (length(missing_from) > 5) ", ..."),
        if (length(missing_to)) paste0(" Unmatched 'to': ", paste(utils::head(missing_to, 5), collapse = ", "),
                                       if (length(missing_to) > 5) ", ...")
      )
      stop(msg, call. = FALSE)
    }

    edges$from <- idx_from
    edges$to   <- idx_to

    # Building tidygraph object
    tg <- tidygraph::tbl_graph(
      nodes = nodes,
      edges = edges,
      directed = directed
    )

    return(tg)
  }

  # igraph-based fallback route
  if (exists("as.igraph")) {
    warning(
      "ResIN_nodeframe and/or ResIN_edgelist not available; falling back to as.igraph() -> tidygraph::as_tbl_graph(). ",
      "Some ResIN metadata may not be preserved.",
      call. = FALSE
    )
    return(tidygraph::as_tbl_graph(as.igraph(x)))
  }

  stop(
    "Could not convert ResIN object to tidygraph: missing node/edge tables and no as.igraph() fallback available.",
    call. = FALSE
  )
}

#' @rawNamespace export(as.network.ResIN)
NULL

#' Convert a ResIN object to a statnet/network object
#'
#' @description
#' Coerces a \code{ResIN} object to a \code{network} object (from the
#' \pkg{network} package used in the \pkg{statnet} ecosystem). The method
#' preserves edge-level columns from \code{x$ResIN_edgelist} as edge attributes
#' and node-level columns from \code{x$ResIN_nodeframe} as vertex attributes
#' whenever available.
#'
#' @param x A \code{ResIN} object.
#' @param directed Logical; should the resulting network be directed?
#'   Defaults to \code{FALSE}.
#' @param loops Logical; allow self-loops? Defaults to \code{FALSE}.
#' @param multiple Logical; allow multiple edges? Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to \code{network::as.network()}.
#'
#' @return An object of class \code{network}.
#'
#' @examples
#' data(lik_data)
#' res <- ResIN(lik_data, generate_ggplot = FALSE, plot_ggplot = FALSE)
#'
#' # ResIN re-exports network::as.network()
#' net <- as.network.ResIN(res) ## alternatively: as.network(res)
#' net
#'
#' @export
#' @importFrom network set.network.attribute
as.network.ResIN <- function(x,
                             directed = FALSE,
                             loops = FALSE,
                             multiple = FALSE,
                             ...) {
  if (!inherits(x, "ResIN")) {
    stop("x must be a ResIN object.", call. = FALSE)
  }

  # Edge table
  el <- x$ResIN_edgelist
  if (!is.data.frame(el) || nrow(el) < 1L) {
    stop("ResIN object does not contain a valid edge list in x$ResIN_edgelist.", call. = FALSE)
  }

  from_col <- if ("from" %in% names(el)) "from" else grep("from", names(el), value = TRUE)[1]
  to_col   <- if ("to"   %in% names(el)) "to"   else grep("to",   names(el), value = TRUE)[1]

  if (is.na(from_col) || is.na(to_col)) {
    stop("Could not identify 'from'/'to' columns in x$ResIN_edgelist.", call. = FALSE)
  }

  edge_cols <- c(from_col, to_col, setdiff(names(el), c(from_col, to_col)))
  edges_df <- el[, edge_cols, drop = FALSE]
  edges_df[[1]] <- as.character(edges_df[[1]])
  edges_df[[2]] <- as.character(edges_df[[2]])

  # Vertex table (preserve node-level metadata if present)
  nf <- x$ResIN_nodeframe
  if (is.data.frame(nf) && nrow(nf) > 0L) {
    id_col <- if ("node_names" %in% names(nf)) {
      "node_names"
    } else if ("from" %in% names(nf)) {
      "from"
    } else {
      names(nf)[1]
    }

    vertex_cols <- c(id_col, setdiff(names(nf), id_col))
    vertices_df <- nf[, vertex_cols, drop = FALSE]
    vertices_df[[1]] <- as.character(vertices_df[[1]])
  } else {
    vertices_df <- data.frame(
      vertex_id = unique(c(edges_df[[1]], edges_df[[2]])),
      stringsAsFactors = FALSE
    )
  }

  net <- network::as.network(
    edges_df,
    directed = directed,
    vertices = vertices_df,
    loops = loops,
    multiple = multiple,
    ...
  )

  # Optional provenance metadata
  net <- network::set.network.attribute(net, "ResIN_source_class", "ResIN")

  if (is.list(x$graph_stats)) {
    net <- network::set.network.attribute(net, "ResIN_graph_stats", x$graph_stats)
  }

  net
}


#' Build edge/node export tables from a ResIN object
#'
#' @param x A ResIN object.
#' @param weight_col Preferred edge-weight column name.
#' @param endpoint_names Character vector of length 2 for endpoint column names.
#' @param node_id_col Name of node identifier column in returned node table.
#' @param integer_node_ids Logical; if TRUE, node IDs are recoded to 1..N and
#'   edge endpoints are mapped accordingly (useful for Graphs.jl).
#' @param include_name_cols Logical; if TRUE and integer_node_ids = TRUE, include
#'   original endpoint names in edge table.
#'
#' @return A list with elements \code{edges}, \code{nodes}, and \code{id_map}.
#' @importFrom stats setNames
#' @noRd
.resin_export_tables <- function(x,
                                 weight_col = "weight",
                                 endpoint_names = c("Source", "Target"),
                                 node_id_col = "Id",
                                 integer_node_ids = FALSE,
                                 include_name_cols = TRUE) {
  if (!inherits(x, "ResIN")) {
    stop("x must be a ResIN object.", call. = FALSE)
  }
  if (!is.data.frame(x$ResIN_edgelist)) {
    stop("ResIN object does not contain a valid edge list in x$ResIN_edgelist.", call. = FALSE)
  }

  el <- x$ResIN_edgelist

  from_col <- if ("from" %in% names(el)) "from" else grep("from", names(el), value = TRUE)[1]
  to_col   <- if ("to"   %in% names(el)) "to"   else grep("to",   names(el), value = TRUE)[1]
  if (is.na(from_col) || is.na(to_col)) {
    stop("Could not identify 'from'/'to' columns in ResIN_edgelist.", call. = FALSE)
  }

  # Build nodes table (preserve as much as possible)
  if (is.data.frame(x$ResIN_nodeframe)) {
    nf <- x$ResIN_nodeframe

    name_col <- if ("node_names" %in% names(nf)) {
      "node_names"
    } else if ("from" %in% names(nf)) {
      "from"
    } else {
      names(nf)[1]
    }

    nodes <- nf
    if (!(node_id_col %in% names(nodes))) {
      nodes[[node_id_col]] <- as.character(nodes[[name_col]])
    }
    # Put ID first
    nodes <- nodes[, c(node_id_col, setdiff(names(nodes), node_id_col)), drop = FALSE]

  } else {
    v_names <- unique(c(as.character(el[[from_col]]), as.character(el[[to_col]])))
    nodes <- data.frame(stringsAsFactors = FALSE)
    nodes[[node_id_col]] <- v_names
  }

  # Build edges table (preserve all edge attrs)
  edges <- el
  names(edges)[names(edges) == from_col] <- endpoint_names[1]
  names(edges)[names(edges) == to_col]   <- endpoint_names[2]

  # Standardize weight column name if present
  if (weight_col %in% names(edges) && weight_col != "weight") {
    # leave as-is
  } else if (!("weight" %in% names(edges)) && ncol(edges) >= 3L) {
    # no-op: preserve original columns; do not guess/rename aggressively
  }

  id_map <- NULL

  if (isTRUE(integer_node_ids)) {
    # Prefer node_names if present, otherwise node_id_col as source labels
    node_name_col <- if ("node_names" %in% names(nodes)) "node_names" else node_id_col
    node_names <- as.character(nodes[[node_name_col]])

    # Ensure uniqueness for Graphs.jl vertex indexing
    if (anyDuplicated(node_names)) {
      stop("Node identifiers are not unique; cannot build integer vertex mapping for Graphs.jl export.",
           call. = FALSE)
    }

    id_map <- setNames(seq_along(node_names), node_names)

    src_names <- as.character(edges[[endpoint_names[1]]])
    dst_names <- as.character(edges[[endpoint_names[2]]])

    if (!all(src_names %in% names(id_map)) || !all(dst_names %in% names(id_map))) {
      stop("Some edge endpoints are missing from the node table; cannot build Graphs.jl export.",
           call. = FALSE)
    }

    edges[[endpoint_names[1]]] <- unname(id_map[src_names])
    edges[[endpoint_names[2]]] <- unname(id_map[dst_names])

    # overwrite node_id_col with integer IDs
    nodes[[node_id_col]] <- seq_len(nrow(nodes))

    if (isTRUE(include_name_cols)) {
      edges$source_name <- src_names
      edges$target_name <- dst_names
    }
  }

  list(edges = edges, nodes = nodes, id_map = id_map)
}


#' @rawNamespace export(as.networkx.ResIN)
NULL

#' Python NetworkX coercion helpers
#'
#' @description
#' Generic for exporting objects to a lightweight NetworkX-ready representation.
#' For \code{ResIN} objects, this creates edge and node CSV tables that can be
#' read via \pkg{pandas} and converted to a \pkg{networkx} graph.
#'
#' @param x Object to coerce/export.
#' @param ... Passed to methods.
#' @export
as.networkx <- function(x, ...) UseMethod("as.networkx")

#' @export
#' @noRd
as.networkx.default <- function(x, ...) {
  stop("No as.networkx() method for objects of class: ",
       paste(class(x), collapse = "/"),
       call. = FALSE)
}

#' Export a ResIN object to NetworkX (Python) tables
#'
#' @description
#' Produces NetworkX-ready edge (and optionally node) tables
#' from a \code{ResIN} object for further manipulation in Python.
#' By default, this method writes CSV files that can be imported into Python
#' via \pkg{pandas} and \pkg{networkx}. Node and edge metadata are preserved as
#' table columns.
#'
#' @param x A \code{ResIN} object.
#' @param file Output file name (legacy style). If \code{edges_only = TRUE},
#'   the edge table is written to \code{file}. If \code{edges_only = FALSE},
#'   \code{file} is treated as a prefix and \code{"_edges.csv"} /
#'   \code{"_nodes.csv"} are appended (with any trailing \code{.csv} removed).
#' @param edges_only Logical; if TRUE (default), only write/return edge table.
#' @param dont_save_csv Logical; if FALSE (default), write CSV output. If TRUE,
#'   no files are written and the resulting table(s) are returned visibly.
#' @param weight_col Preferred edge-weight column name. Defaults to \code{"weight"}.
#' @param ... Ignored.
#'
#' @return If \code{edges_only = TRUE}, an edge table \code{data.frame}.
#'   Otherwise a list with elements \code{edges} and \code{nodes}.
#'
#' @examples
#' \dontrun{
#' data(lik_data)
#' res <- ResIN(lik_data, generate_ggplot = FALSE, plot_ggplot = FALSE)
#'
#' # Return tables only (no files written)
#' nx_tbls <- as.networkx(res, dont_save_csv = TRUE, edges_only = FALSE)
#'
#' # Default behavior writes CSV files
#' # as.networkx(res, file = "ResIN_networkx.csv", edges_only = FALSE)
#' }
#'
#' @export
#' @importFrom readr write_csv
as.networkx.ResIN <- function(x,
                              file = "ResIN_networkx.csv",
                              edges_only = TRUE,
                              dont_save_csv = FALSE,
                              weight_col = "weight",
                              ...) {
  tabs <- .resin_export_tables(
    x = x,
    weight_col = weight_col,
    endpoint_names = c("source", "target"),
    node_id_col = "node",
    integer_node_ids = FALSE
  )

  out <- if (isTRUE(edges_only)) {
    tabs$edges
  } else {
    list(edges = tabs$edges, nodes = tabs$nodes)
  }

  if (isTRUE(dont_save_csv)) {
    return(out)
  }

  if (isTRUE(edges_only)) {
    readr::write_csv(tabs$edges, file = file, na = "")
    message("NetworkX edge table created: ", normalizePath(file, winslash = "/", mustWork = FALSE))
    return(invisible(tabs$edges))
  }

  prefix <- sub("\\.csv$", "", file, ignore.case = TRUE)
  edge_file <- paste0(prefix, "_edges.csv")
  node_file <- paste0(prefix, "_nodes.csv")

  readr::write_csv(tabs$edges, file = edge_file, na = "")
  readr::write_csv(tabs$nodes, file = node_file, na = "")

  message(
    "NetworkX tables created: ",
    normalizePath(edge_file, winslash = "/", mustWork = FALSE),
    " and ",
    normalizePath(node_file, winslash = "/", mustWork = FALSE)
  )

  invisible(list(edges = tabs$edges, nodes = tabs$nodes))
}

#' @rawNamespace export(as.graphsjl.ResIN)
NULL

#' Julia Graphs.jl coercion helpers
#'
#' @description
#' Generic for exporting objects to a lightweight Graphs.jl-ready representation.
#' For \code{ResIN} objects, this creates edge and node CSV tables suitable for
#' loading with \pkg{CSV.jl}/\pkg{DataFrames.jl}, with integer vertex IDs for
#' direct use in \pkg{Graphs.jl}.
#'
#' @param x Object to coerce/export.
#' @param ... Passed to methods.
#' @export
as.graphsjl <- function(x, ...) UseMethod("as.graphsjl")

#' @export
#' @noRd
as.graphsjl.default <- function(x, ...) {
  stop("No as.graphsjl() method for objects of class: ",
       paste(class(x), collapse = "/"),
       call. = FALSE)
}

#' Export a ResIN object to Graphs.jl (Julia) tables
#'
#' @description
#' Produces Graphs.jl-style edge (and optionally node) tables from a \code{ResIN}
#' object. Edge endpoints are mapped to integer vertex IDs (\code{src}, \code{dst})
#' to align with \pkg{Graphs.jl} in Julia. Node and edge metadata are preserved as table
#' columns. The node table stores the vertex mapping and additional ResIN metadata.
#'
#' @param x A \code{ResIN} object.
#' @param file Output file name (legacy style). If \code{edges_only = TRUE},
#'   the edge table is written to \code{file}. If \code{edges_only = FALSE},
#'   \code{file} is treated as a prefix and \code{"_edges.csv"} /
#'   \code{"_nodes.csv"} are appended (with any trailing \code{.csv} removed).
#' @param edges_only Logical; if TRUE (default), only write/return edge table.
#' @param dont_save_csv Logical; if FALSE (default), write CSV output. If TRUE,
#'   no files are written and the resulting table(s) are returned visibly.
#' @param weight_col Preferred edge-weight column name. Defaults to \code{"weight"}.
#' @param ... Ignored.
#'
#' @return If \code{edges_only = TRUE}, an edge table \code{data.frame}.
#'   Otherwise a list with elements \code{edges} and \code{nodes}. The node table
#'   includes integer \code{vertex_id} values and preserved node metadata.
#'
#' @examples
#' \dontrun{
#' data(lik_data)
#' res <- ResIN(lik_data, generate_ggplot = FALSE, plot_ggplot = FALSE)
#'
#' # Return tables only (no files written)
#' jl_tbls <- as.graphsjl(res, dont_save_csv = TRUE, edges_only = FALSE)
#'
#' # Default behavior writes CSV files
#' # as.graphsjl(res, file = "ResIN_graphsjl.csv", edges_only = FALSE)
#' }
#'
#' @export
#' @importFrom readr write_csv
as.graphsjl.ResIN <- function(x,
                              file = "ResIN_graphsjl.csv",
                              edges_only = TRUE,
                              dont_save_csv = FALSE,
                              weight_col = "weight",
                              ...) {
  tabs <- .resin_export_tables(
    x = x,
    weight_col = weight_col,
    endpoint_names = c("src", "dst"),
    node_id_col = "vertex_id",
    integer_node_ids = TRUE,
    include_name_cols = TRUE
  )

  out <- if (isTRUE(edges_only)) {
    tabs$edges
  } else {
    list(edges = tabs$edges, nodes = tabs$nodes)
  }

  if (isTRUE(dont_save_csv)) {
    return(out)
  }

  if (isTRUE(edges_only)) {
    readr::write_csv(tabs$edges, file = file, na = "")
    message("Graphs.jl edge table created: ", normalizePath(file, winslash = "/", mustWork = FALSE))
    return(invisible(tabs$edges))
  }

  prefix <- sub("\\.csv$", "", file, ignore.case = TRUE)
  edge_file <- paste0(prefix, "_edges.csv")
  node_file <- paste0(prefix, "_nodes.csv")

  readr::write_csv(tabs$edges, file = edge_file, na = "")
  readr::write_csv(tabs$nodes, file = node_file, na = "")

  message(
    "Graphs.jl tables created: ",
    normalizePath(edge_file, winslash = "/", mustWork = FALSE),
    " and ",
    normalizePath(node_file, winslash = "/", mustWork = FALSE)
  )

  invisible(list(edges = tabs$edges, nodes = tabs$nodes))
}

