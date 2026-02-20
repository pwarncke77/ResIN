#' @title Extract and summarize the results of a bootstrap simulation undertaken on a ResIN object
#'
#' @description
#' Extract bootstrap draws from \code{"ResIN_boots_executed"} results. Failed iterations
#' (stored as \code{NULL}) are skipped automatically.
#'
#' @param ResIN_boots_executed An object of class \code{"ResIN_boots_executed"} (output of \code{\link{ResIN_boots_execute}}).
#' @param what Character scalar naming the quantity to extract (e.g., \code{"global_clustering"}).
#' @param summarize_results Logical; if TRUE return a small summary table, otherwise return draws.
#' @param allow_missing Logical; if FALSE (default) the function errors if \code{what} is missing in any successful fit.
#'   If TRUE, missing iterations are kept as \code{NULL} and summaries are computed from available values.
#'
#' @return
#' If the extracted quantity is scalar per iteration, returns an object of class
#' \code{"ResIN_boots_draws"} (a numeric vector with attributes). If \code{summarize_results = TRUE},
#' returns a one-row data.frame of summary statistics.
#'
#' @examples
#' ## Load the 12-item simulated Likert-type toy dataset
#' data(lik_data)
#'
#' # Apply the ResIN function to toy Likert data:
#' ResIN_obj <- ResIN(lik_data, cor_method = "spearman", network_stats = TRUE,
#'                       generate_ggplot = FALSE, plot_ggplot = FALSE)
#'
#'\donttest{
#' # Prepare for bootstrapping
#' prepped_boots <- ResIN_boots_prepare(ResIN_obj, n=100, boots_type="resample")
#'
#' # Execute the prepared bootstrap list
#' executed_boots <-  ResIN_boots_execute(prepped_boots, parallel = TRUE, detect_cores = TRUE)
#'
#' # Extract results - here for example, the network (global)-clustering coefficient
#' ResIN_boots_extract(executed_boots, what = "global_clustering", summarize_results = TRUE)
#'}
#'
#'
#' @export
ResIN_boots_extract <- function(ResIN_boots_executed,
                                what,
                                summarize_results = FALSE,
                                allow_missing = FALSE) {

  if (!inherits(ResIN_boots_executed, "ResIN_boots_executed")) {
    stop("Please supply a 'ResIN_boots_executed' object.", call. = FALSE)
  }
  if (!is.character(what) || length(what) < 1L || anyNA(what)) {
    stop("'what' must be a non-empty character vector.", call. = FALSE)
  }
  what <- what[!duplicated(what)]

  # Successful fits
  ok <- attr(ResIN_boots_executed, "ok", exact = TRUE)
  if (is.logical(ok) && length(ok) == length(ResIN_boots_executed)) {
    idx_ok <- which(ok)
  } else {
    idx_ok <- which(vapply(ResIN_boots_executed, inherits, logical(1), what = "ResIN"))
  }
  fits <- ResIN_boots_executed[idx_ok]
  if (!length(fits)) stop("No successful ResIN fits found.", call. = FALSE)

  # Small alias support (only used if key is not found directly)
  resolve_alias <- function(key) {
    switch(key,
           "scores_x" = "raw_x",
           "x_scores" = "raw_x",
           "scores_y" = "raw_y",
           "y_scores" = "raw_y",
           key
    )
  }

  # Deterministic extractor (returns value as stored: scalar, vector, etc.)
  extract_any <- function(fit, key) {

    # graph_stats named vectors
    if (is.list(fit$graph_stats) && length(fit$graph_stats) >= 1) {
      s1 <- fit$graph_stats[[1]]
      if (is.numeric(s1) && !is.null(names(s1)) && key %in% names(s1)) return(unname(s1[key]))
    }
    if (is.list(fit$graph_stats) && length(fit$graph_stats) >= 2) {
      s2 <- fit$graph_stats[[2]]
      if (is.numeric(s2) && !is.null(names(s2)) && key %in% names(s2)) return(unname(s2[key]))
    }

    # scores
    if (is.data.frame(fit$ResIN_scores) && key %in% names(fit$ResIN_scores)) {
      return(fit$ResIN_scores[[key]])
    }

    # nodeframe
    if (is.data.frame(fit$ResIN_nodeframe) && key %in% names(fit$ResIN_nodeframe)) {
      return(fit$ResIN_nodeframe[[key]])
    }

    # edgelist
    if (is.data.frame(fit$ResIN_edgelist) && key %in% names(fit$ResIN_edgelist)) {
      return(fit$ResIN_edgelist[[key]])
    }

    # aux_objects
    if (is.list(fit$aux_objects) && !is.null(names(fit$aux_objects)) && key %in% names(fit$aux_objects)) {
      return(fit$aux_objects[[key]])
    }

    NULL
  }

  # Metadata
  plan <- attr(ResIN_boots_executed, "plan", exact = TRUE)
  n_total <- length(ResIN_boots_executed)
  n_failed <- attr(ResIN_boots_executed, "n_failed", exact = TRUE)
  if (!is.numeric(n_failed) || length(n_failed) != 1L) n_failed <- n_total - length(fits)

  # First-level summary of a numeric vector (within one bootstrap iteration)
  summarize_vector_once <- function(v) {
    v <- as.numeric(v)
    v <- v[is.finite(v)]
    if (!length(v)) return(rep(NA_real_, 11))

    qs <- stats::quantile(v, probs = c(.025, .05, .25, .5, .75, .95, .975), na.rm = TRUE)
    out <- c(
      min = min(v, na.rm = TRUE),
      q2.5 = unname(qs[["2.5%"]]),
      q5 = unname(qs[["5%"]]),
      q25 = unname(qs[["25%"]]),
      median = unname(qs[["50%"]]),
      mean = mean(v, na.rm = TRUE),
      q75 = unname(qs[["75%"]]),
      q95 = unname(qs[["95%"]]),
      q97.5 = unname(qs[["97.5%"]]),
      max = max(v, na.rm = TRUE),
      sd = stats::sd(v, na.rm = TRUE)
    )
    out
  }

  # Second-level summary “diagonal” (apply same stats to first-level stats across iterations)
  summarize_summaries_diagonal <- function(mat) {
    # mat: n_iter x 11, with colnames = c(min,q2.5,...,sd)
    out <- numeric(ncol(mat))
    names(out) <- colnames(mat)

    out["min"]    <- min(mat[, "min"], na.rm = TRUE)
    out["q2.5"]   <- unname(stats::quantile(mat[, "q2.5"],  probs = 0.025, na.rm = TRUE))
    out["q5"]     <- unname(stats::quantile(mat[, "q5"],    probs = 0.05,  na.rm = TRUE))
    out["q25"]    <- unname(stats::quantile(mat[, "q25"],   probs = 0.25,  na.rm = TRUE))
    out["median"] <- stats::median(mat[, "median"], na.rm = TRUE)
    out["mean"]   <- mean(mat[, "mean"], na.rm = TRUE)
    out["q75"]    <- unname(stats::quantile(mat[, "q75"],   probs = 0.75,  na.rm = TRUE))
    out["q95"]    <- unname(stats::quantile(mat[, "q95"],   probs = 0.95,  na.rm = TRUE))
    out["q97.5"]  <- unname(stats::quantile(mat[, "q97.5"], probs = 0.975, na.rm = TRUE))
    out["max"]    <- max(mat[, "max"], na.rm = TRUE)
    out["sd"]     <- stats::sd(mat[, "sd"], na.rm = TRUE)

    out
  }

  # Build extracted objects for each `what`
  make_one <- function(key) {
    key0 <- key
    vals <- lapply(fits, extract_any, key = key0)

    # If not found, try alias
    if (all(vapply(vals, is.null, logical(1)))) {
      key1 <- resolve_alias(key0)
      if (!identical(key1, key0)) {
        vals <- lapply(fits, extract_any, key = key1)
      }
    }

    missing <- vapply(vals, is.null, logical(1))
    if (any(missing)) {
      msg <- sprintf("'%s' missing in %d of %d successful fits.", key, sum(missing), length(vals))
      if (!allow_missing) stop(msg, call. = FALSE)
    }

    names(vals) <- paste0("iter_", idx_ok)

    # If everything is scalar numeric => numeric draws
    is_scalar_num <- vapply(vals, function(z) is.numeric(z) && length(z) == 1L, logical(1))
    if (all(is_scalar_num | missing)) {
      draws <- vapply(vals, function(z) if (is.null(z)) NA_real_ else as.numeric(z), numeric(1))
      class(draws) <- c("ResIN_boots_draws", "numeric")
      attr(draws, "what") <- key
      attr(draws, "n_total") <- n_total
      attr(draws, "n_ok") <- sum(!is.na(draws))
      attr(draws, "n_failed") <- n_failed
      attr(draws, "plan") <- plan
      attr(draws, "created") <- attr(ResIN_boots_executed, "created", exact = TRUE)
      return(draws)
    }

    # Otherwise return list as extracted (vector-valued etc.)
    vals
  }

  extracted <- stats::setNames(lapply(what, make_one), what)

  if (!summarize_results) {
    if (length(extracted) == 1L) return(extracted[[1L]])
    return(extracted)
  }

  # Summarize to one-row-per-what data.frame
  summarize_one <- function(obj, key) {

    # Case 1: scalar draws (numeric vector)
    if (is.numeric(obj) && !is.list(obj)) {
      qs <- stats::quantile(obj, probs = c(.025, .05, .25, .5, .75, .95, .975), na.rm = TRUE)
      return(data.frame(
        what = key,
        n_total = attr(obj, "n_total", exact = TRUE),
        n_ok = attr(obj, "n_ok", exact = TRUE),
        n_failed = attr(obj, "n_failed", exact = TRUE),
        min = min(obj, na.rm = TRUE),
        q2.5 = unname(qs[["2.5%"]]),
        q5 = unname(qs[["5%"]]),
        q25 = unname(qs[["25%"]]),
        median = unname(qs[["50%"]]),
        mean = mean(obj, na.rm = TRUE),
        q75 = unname(qs[["75%"]]),
        q95 = unname(qs[["95%"]]),
        q97.5 = unname(qs[["97.5%"]]),
        max = max(obj, na.rm = TRUE),
        sd = stats::sd(obj, na.rm = TRUE),
        row.names = NULL
      ))
    }

    # Case 2: vector/list-valued per iteration => two-stage summarization
    # Keep only numeric vectors
    vecs <- obj[!vapply(obj, is.null, logical(1))]
    vecs <- vecs[vapply(vecs, is.numeric, logical(1))]

    if (!length(vecs)) {
      stop(sprintf("'%s' is not numeric (or not found) across successful fits.", key), call. = FALSE)
    }

    message(sprintf(
      "ResIN_boots_extract(): '%s' is vector-valued; summarize_results=TRUE applies a two-stage summary (within-iteration then across-iteration).",
      key
    ))

    first <- t(vapply(vecs, summarize_vector_once, numeric(11)))
    colnames(first) <- c("min","q2.5","q5","q25","median","mean","q75","q95","q97.5","max","sd")

    second <- summarize_summaries_diagonal(first)

    data.frame(
      what = key,
      n_total = n_total,
      n_ok = nrow(first),
      n_failed = n_failed,
      min = unname(second["min"]),
      q2.5 = unname(second["q2.5"]),
      q5 = unname(second["q5"]),
      q25 = unname(second["q25"]),
      median = unname(second["median"]),
      mean = unname(second["mean"]),
      q75 = unname(second["q75"]),
      q95 = unname(second["q95"]),
      q97.5 = unname(second["q97.5"]),
      max = unname(second["max"]),
      sd = unname(second["sd"]),
      row.names = NULL
    )
  }

  rows <- lapply(names(extracted), function(k) summarize_one(extracted[[k]], k))
  out_df <- do.call(rbind, rows)
  rownames(out_df) <- NULL
  out_df
}
