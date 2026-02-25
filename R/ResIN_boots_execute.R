#' @title Carry out prepared bootstrap analyses on ResIN networks
#'
#' @description
#' Executes a bootstrap plan created by \code{\link{ResIN_boots_prepare}} by repeatedly
#' re-estimating ResIN on resampled or permuted versions of the original data.
#' Can optionally leverage CPU parallelism.
#'
#' @param ResIN_boots_prepped A \code{"ResIN_boots_prepped"} bootstrap plan (output of \code{\link{ResIN_boots_prepare}}).
#' @param parallel Should execution use parallelism via \code{foreach} + a PSOCK cluster? Defaults to FALSE.
#' @param detect_cores Should available CPU cores be detected automatically? Defaults to TRUE (ignored when \code{parallel = FALSE}).
#' @param core_offset Integer offset subtracted from the number of detected cores. Defaults to 0L.
#' @param n_cores Manually specify number of cores (ignored if \code{detect_cores = TRUE} or \code{parallel = FALSE}).
#' @param inorder Should parallel execution preserve sequential ordering? Defaults to FALSE.
#'
#' @return
#' An object of class \code{"ResIN_boots_executed"} containing \code{n} bootstrapped
#' \code{ResIN} fits. Use \code{print()}, \code{summary()}, \code{length()}, and \code{[}
#' to inspect or subset results. See \code{\link{ResIN_boots_executed}} for details.
#'
#' @examples
#' ## Load the 12-item simulated Likert-type toy dataset
#' data(lik_data)
#'
#' # Apply the ResIN function to toy Likert data:
#' ResIN_obj <- ResIN(lik_data, network_stats = TRUE,
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
#' @export
#' @importFrom foreach foreach %dopar%
#' @importFrom parallelly availableCores
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
ResIN_boots_execute <- function(ResIN_boots_prepped,
                                parallel = FALSE,
                                detect_cores = TRUE,
                                core_offset = 0L,
                                n_cores = 2L,
                                inorder = FALSE) {

  if (!inherits(ResIN_boots_prepped, "ResIN_boots_prepped")) {
    stop("Please supply a 'ResIN_boots_prepped' bootstrap plan.", call. = FALSE)
  }
  if (!is.list(ResIN_boots_prepped$arglist) || is.null(ResIN_boots_prepped$arglist$df)) {
    stop("Bootstrap plan must contain a valid $arglist with $df.", call. = FALSE)
  }

  n <- ResIN_boots_prepped$n
  df0        <- ResIN_boots_prepped$arglist$df
  boots_type <- ResIN_boots_prepped$boots_type
  resample_n <- ResIN_boots_prepped$resample_size
  wts        <- ResIN_boots_prepped$weights
  seeds      <- ResIN_boots_prepped$iter_seeds
  save_input <- isTRUE(ResIN_boots_prepped$save_input)

  if (length(seeds) != n) stop("Bootstrap plan contains inconsistent iter_seeds length.", call. = FALSE)

  make_boot_df <- function(i) {
    set.seed(seeds[i])

    if (boots_type == "resample") {
      idx <- sample.int(nrow(df0), size = resample_n, replace = TRUE, prob = wts)
      out <- df0[idx, , drop = FALSE]
      rownames(out) <- NULL
      return(out)
    }

    # permute
    out <- df0
    for (j in seq_along(out)) {
      out[[j]] <- sample(out[[j]], size = nrow(out), replace = FALSE)
    }
    rownames(out) <- NULL
    out
  }

  boot_inputs <- if (save_input) vector("list", n) else NULL

  if (isTRUE(parallel)) {

    if (isTRUE(detect_cores)) {
      n_cores <- as.integer(parallelly::availableCores()[1] - core_offset)
    } else {
      n_cores <- as.integer(n_cores)
    }
    if (is.na(n_cores) || n_cores < 1L) n_cores <- 1L

    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    doSNOW::registerDoSNOW(cl)

    pb <- utils::txtProgressBar(max = n, style = 3)
    on.exit(close(pb), add = TRUE)

    progress <- function(k) utils::setTxtProgressBar(pb, k)
    opts <- list(progress = progress)

    res_raw <- foreach::foreach(
      i = seq_len(n),
      .inorder = inorder,
      .options.snow = opts,
      .packages = "ResIN"
    ) %dopar% {

      boot_df <- make_boot_df(i)
      args_i <- ResIN_boots_prepped$arglist
      args_i$df <- boot_df

      fit <- tryCatch(
        do.call(ResIN::ResIN, args_i),
        error = function(e) NULL
      )

      ok <- !is.null(fit) && inherits(fit, "ResIN")

      list(
        fit = fit,
        ok = ok,
        boot_df = if (save_input && ok) boot_df else NULL
      )
    }

    utils::setTxtProgressBar(pb, n)
    close(pb)
    cat("\n")

    ok <- vapply(res_raw, `[[`, logical(1), "ok")
    res_list <- lapply(res_raw, `[[`, "fit")

    if (save_input) {
      boot_inputs <- lapply(res_raw, `[[`, "boot_df")
    }

  } else {

    res_list <- vector("list", n)
    ok <- logical(n)

    for (i in seq_len(n)) {
      boot_df <- make_boot_df(i)
      args_i <- ResIN_boots_prepped$arglist
      args_i$df <- boot_df

      fit <- tryCatch(
        do.call(ResIN, args_i),
        error = function(e) NULL
      )

      ok[i] <- !is.null(fit) && inherits(fit, "ResIN")
      res_list[[i]] <- fit

      if (save_input && ok[i]) boot_inputs[[i]] <- boot_df
      if (save_input && !ok[i]) boot_inputs[[i]] <- NULL
    }
  }

  class(res_list) <- c("ResIN_boots_executed", "list")
  attr(res_list, "plan") <- ResIN_boots_prepped
  attr(res_list, "created") <- Sys.time()
  attr(res_list, "ok") <- ok
  attr(res_list, "n_failed") <- sum(!ok)

  if (save_input) attr(res_list, "boot_inputs") <- boot_inputs

  res_list
}


