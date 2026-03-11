#' @title Carry out prepared bootstrap analyses on ResIN networks
#'
#' @description
#' Executes a bootstrap plan created by \code{\link{ResIN_boots_prepare}} by repeatedly
#' re-estimating ResIN on resampled or permuted versions of the original data.
#' Can optionally leverage CPU parallelism.
#'
#' @param ResIN_boots_prepped A \code{"ResIN_boots_prepped"} bootstrap plan (output of \code{\link{ResIN_boots_prepare}}).
#' @param parallel Logical; should execution use parallelism via \code{foreach} + a PSOCK cluster? Defaults to FALSE.
#' @param detect_cores Logical; should available CPU cores be detected automatically? Defaults to TRUE (ignored when \code{parallel = FALSE}).
#' @param core_offset Integer offset subtracted from the number of detected cores. Defaults to 1L. Change to 0L on low-overhead systems or if sure that system won't stall.
#' @param n_cores Manually specify number of cores (ignored if \code{detect_cores = TRUE} or \code{parallel = FALSE}).
#' @param inorder Logical; should parallel execution preserve sequential ordering? Defaults to FALSE.
#' @param verbose Logical; should the type of computational execution (parallel or sequential), the parallel engine (if any) and the number of cores be returned to the dashboard while the function is running?
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
#' prepped_boots <- ResIN_boots_prepare(ResIN_obj, n=50, boots_type="resample")
#'
#' # Execute the prepared bootstrap list
#' executed_boots <-  ResIN_boots_execute(prepped_boots, parallel = TRUE,
#'                       detect_cores = TRUE, verbose = FALSE)
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
                                core_offset = 1L,
                                n_cores = 2L,
                                inorder = FALSE,
                                verbose = TRUE) {

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

  if (length(seeds) != n) {
    stop("Bootstrap plan contains inconsistent iter_seeds length.", call. = FALSE)
  }

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
  errors <- vector("list", n)

  arglist0 <- ResIN_boots_prepped$arglist
  arglist0$df <- NULL

  used_parallel <- FALSE
  workers_used <- 1L
  backend_used <- "sequential"

  if (isTRUE(parallel)) {

    if (isTRUE(detect_cores)) {
      detected <- tryCatch(
        as.integer(parallelly::availableCores()),
        error = function(e) NA_integer_
      )

      if (is.na(detected)) detected <- 1L
      n_cores <- detected - as.integer(core_offset)
    } else {
      n_cores <- as.integer(n_cores)
    }

    if (is.na(n_cores) || n_cores < 1L) n_cores <- 1L

    if (n_cores >= 2L) {
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)

      doSNOW::registerDoSNOW(cl)

      backend_used <- foreach::getDoParName()
      workers_used <- foreach::getDoParWorkers()
      used_parallel <- isTRUE(foreach::getDoParRegistered()) && workers_used >= 2L

      if (!used_parallel) {
        if (verbose) {
          warning(
            sprintf(
              "Parallel backend registration did not yield multiple workers (backend=%s, workers=%d). Falling back to sequential execution.",
              backend_used, workers_used
            ),
            call. = FALSE
          )
        }
      } else {
        if (verbose) {
          message(sprintf("Running bootstrap in parallel with %d workers (%s).", workers_used, backend_used))
        }

        pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
        on.exit(close(pb), add = TRUE)

        progress <- function(k) utils::setTxtProgressBar(pb, k)
        opts <- list(progress = progress)

        res_raw <- withCallingHandlers(
          foreach::foreach(
            i = seq_len(n),
            .inorder = inorder,
            .options.snow = opts,
            .packages = "ResIN",
            .export = c("make_boot_df", "arglist0", "save_input")
          ) %dopar% {
            boot_df <- make_boot_df(i)

            args_i <- arglist0
            args_i$df <- boot_df

            res <- tryCatch(
              {
                fit <- do.call(ResIN::ResIN, args_i)
                list(fit = fit, error = NULL)
              },
              error = function(e) {
                list(fit = NULL, error = conditionMessage(e))
              }
            )

            ok <- !is.null(res$fit) && inherits(res$fit, "ResIN")

            list(
              fit = res$fit,
              ok = ok,
              error = res$error,
              pid = Sys.getpid(),
              boot_df = if (save_input && ok) boot_df else NULL
            )
          },
          warning = function(w) {
            msg <- conditionMessage(w)
            if (grepl("^already exporting variable\\(s\\):", msg)) {
              invokeRestart("muffleWarning")
            }
          }
        )

        ok <- vapply(res_raw, `[[`, logical(1), "ok")
        res_list <- lapply(res_raw, `[[`, "fit")
        errors <- lapply(res_raw, `[[`, "error")

        if (save_input) {
          boot_inputs <- lapply(res_raw, `[[`, "boot_df")
        }

        class(res_list) <- c("ResIN_boots_executed", "list")
        attr(res_list, "plan") <- ResIN_boots_prepped
        attr(res_list, "created") <- Sys.time()
        attr(res_list, "ok") <- ok
        attr(res_list, "n_failed") <- sum(!ok)
        attr(res_list, "errors") <- errors
        attr(res_list, "parallel") <- TRUE
        attr(res_list, "workers") <- workers_used
        attr(res_list, "backend") <- backend_used

        if (save_input) attr(res_list, "boot_inputs") <- boot_inputs

        if (all(!ok)) {
          warning("All bootstrap iterations failed. Inspect attr(result, 'errors').", call. = FALSE)
        }

        return(res_list)
      }
    } else {
      if (verbose) {
        warning(
          sprintf(
            "parallel = TRUE was requested, but only %d worker was available after core_offset. Falling back to sequential execution.",
            n_cores
          ),
          call. = FALSE
        )
      }
    }
  }

  # Sequential fallback or explicit sequential mode
  if (verbose) {
    message("Running bootstrap sequentially.")
  }

  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  on.exit(close(pb), add = TRUE)

  res_list <- vector("list", n)
  ok <- logical(n)

  for (i in seq_len(n)) {
    boot_df <- make_boot_df(i)
    args_i <- ResIN_boots_prepped$arglist
    args_i$df <- boot_df

    res <- tryCatch(
      {
        fit <- do.call(ResIN, args_i)
        list(fit = fit, error = NULL)
      },
      error = function(e) {
        list(fit = NULL, error = conditionMessage(e))
      }
    )

    ok[i] <- !is.null(res$fit) && inherits(res$fit, "ResIN")
    res_list[[i]] <- res$fit
    errors[[i]] <- res$error

    if (save_input) {
      boot_inputs[[i]] <- if (ok[i]) boot_df else NULL
    }

    utils::setTxtProgressBar(pb, i)
  }

  class(res_list) <- c("ResIN_boots_executed", "list")
  attr(res_list, "plan") <- ResIN_boots_prepped
  attr(res_list, "created") <- Sys.time()
  attr(res_list, "ok") <- ok
  attr(res_list, "n_failed") <- sum(!ok)
  attr(res_list, "errors") <- errors
  attr(res_list, "parallel") <- FALSE
  attr(res_list, "workers") <- 1L
  attr(res_list, "backend") <- "sequential"

  if (save_input) attr(res_list, "boot_inputs") <- boot_inputs

  if (all(!ok)) {
    warning("All bootstrap iterations failed. Inspect attr(result, 'errors').", call. = FALSE)
  }

  res_list
}

