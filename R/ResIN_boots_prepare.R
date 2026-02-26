#' @title Create a bootstrap plan for re-estimating ResIN objects to derive statistical uncertainty estimates
#'
#' @description Provides instructions for how to bootstrap a ResIN network to derive uncertainty estimates around core quantities of interest. Requires output of \code{ResIN} function.
#'
#' @param ResIN_object A ResIN object to prepare bootstrapping workflow.
#' @param n Bootstrapping sample size. Defaults to 10.000.
#' @param weights An optional weights vector that can be used to adjust the re-sampling of observations. Should either be NULL (default) or a positive numeric vector of the same length as the original data.
#' @param boots_type What kind of bootstrapping should be performed? If set to "resample", function performs row-wise re-sampling of raw data (useful for e.g., sensitivity or power analysis). If set to "permute", function will randomly reshuffle raw item responses (useful e.g., for simulating null-hypothesis distributions). Defaults to "resample".
#' @param resample_size Optional parameter determining sample size when \code{boots_type} is set to "resample". Defaults of to number of rows in raw data.
#' @param save_input Should all input information for each bootstrap iteration (including re-sampled/permuted data) be stored. Set to FALSE by default to save a lot of memory and disk storage.
#' @param seed_boots Random seed for bootstrap samples
#'
#' @return
#' An object of class \code{"ResIN_boots_prepped"} containing a bootstrap plan
#' (specification) used by \code{\link{ResIN_boots_execute}}.
#' Use \code{print()}, \code{summary()}, \code{length()}, and \code{[}
#' to inspect or subset the plan. See \code{\link{ResIN_boots_prepped}} for details.
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
#' @importFrom dplyr "sample_n" "select" "all_of"
#'

ResIN_boots_prepare <- function(ResIN_object,
                                n = 10000,
                                boots_type = "resample",
                                resample_size = NULL,
                                weights = NULL,
                                save_input = FALSE,
                                seed_boots = 42) {

  # Validation
  if (!inherits(ResIN_object, "ResIN")) {
    stop("Please supply a ResIN object.")
  }
  if (!boots_type %in% c("resample", "permute")) {
    stop("boots_type must be either 'resample' or 'permute'.")
  }

  df <- ResIN_object$aux_objects$ResIN_arglist$df
  if (is.null(resample_size)) resample_size <- nrow(df)

  if (!is.null(weights)) {
    if (!is.numeric(weights) || any(weights < 0) || length(weights) != nrow(df)) {
      stop("weights must be NULL or a non-negative numeric vector of length nrow(df).")
    }
    if (all(weights == 0)) stop("weights cannot be all zero.")
  }

  # Prepare ResIN arglist for repeated fitting
  arglist <- ResIN_object$aux_objects$ResIN_arglist
  arglist$plot_ggplot <- FALSE
  if (isFALSE(save_input)) arglist$save_input <- FALSE

  ## Reproducibility metadata
  # Stable ID for the underlying data used to build the plan
  tmp <- tempfile("ResIN_df_", fileext = ".rds")
  saveRDS(df, tmp, compress = FALSE)
  df_id <- unname(tools::md5sum(tmp))
  unlink(tmp)

  # Keep track of package version used to create the plan
  resin_version <- as.character(utils::packageVersion("ResIN"))

  # Create per-iteration seeds (parallel-friendly)
  old_seed <- .Random.seed
  on.exit({ if (!is.null(old_seed)) .Random.seed <<- old_seed }, add = TRUE)

  set.seed(seed_boots)
  iter_seeds <- sample.int(.Machine$integer.max, n)

  out <- list(
    call = match.call(),
    boots_type = boots_type,
    n = n,
    resample_size = resample_size,
    weights = weights,
    save_input = save_input,
    seed_boots = seed_boots,
    iter_seeds = iter_seeds,
    arglist = arglist,
    base_n = nrow(df),
    base_p = ncol(df),

    # Reproducibility fields
    df_id = df_id,
    ResIN_version = resin_version
  )

  class(out) <- c("ResIN_boots_prepped", "list")
  out
}
