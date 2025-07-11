#' @title ResIN_boots_extract
#'
#' @description Extract and summarize quantities from bootstrapped ResIN objects
#'
#' @param ResIN_boots_executed A list of prepared ResIN objects for bootstrapping (outcome of the \code{ResIN_boots_executed} function)
#' @param what A character vector of length one specifying the target quantity of interest. This should be a one-to-one match with the corresponding output vector (or scalar) among the bootstrapped result list (see \code{ResIN_boots_execute}). For participants' position on the x-axis of the ResIN latent space, for example, specify "scores_x".
#' @param summarize_results Should the extracted quantities be summarized through a series of descriptive statistics? If set to true, the minimum, maximum, mean, selected quantiles, and the standard deviation are reported. If set to FALSE (default), extracted quantities are instead returned as a list.
#'
#' @return A list object containing n (bootstrapped) \code{ResIN} list objects.
#'
#' @examples
#' ## Load the 12-item simulated Likert-type toy dataset
#' data(lik_data)
#'
#' # Apply the ResIN function to toy Likert data:
#' ResIN_obj <- ResIN(lik_data, cor_method = "spearman", network_stats = TRUE,
#'                       generate_ggplot = FALSE)
#'
#'\dontrun{
#' # Prepare for bootstrapping
#' prepped_boots <- ResIN_boots_prepare(ResIN_obj, n=5000, boots_type="permute")
#'
#' # Execute the prepared bootstrap list
#' executed_boots <-  ResIN_boots_execute(prepped_boots, parallel = TRUE, detect_cores = TRUE)
#'
#' # Extract results - here for example, the network (global)-clustering coefficient
#' ResIN_boots_extract(executed_boots, what = "global_clustering", summarize_results = TRUE)
#'}
#'
#' @export
#' @importFrom stats "median" "quantile" "rnorm" "var"
#'

ResIN_boots_extract <- function(ResIN_boots_executed, what, summarize_results = FALSE) {

  search_list <- function(current_list, what) {
    result <- list()
    # Initialize a counter in the environment if it doesn't exist
    if (!exists("match_counter", envir = .GlobalEnv)) {
      assign("match_counter", 0, envir = .GlobalEnv)
    }
    for (i in seq_along(current_list)) {
      item <- current_list[[i]]
      # Get the name if it exists; otherwise, set to index
      name <- if (!is.null(names(current_list))) names(current_list)[i] else NULL

      if (is.list(item)) {
        # Recursively search the sublist and accumulate results
        res <- search_list(item, what)
        if (length(res) > 0) {
          result <- c(result, res)
        }
      } else if (is.data.frame(item)) {
        if (what %in% colnames(item)) {
          # Increment the counter
          match_counter <<- get("match_counter", envir = .GlobalEnv) + 1
          # Add the matching column as a separate list element
          result[[length(result) + 1]] <- item[[what]]
          # Set a name for this element with the index
          names(result)[length(result)] <- paste0(what, "_", match_counter)
        }
      } else if (!is.null(name) && name == what) {
        # Increment the counter
        match_counter <<- get("match_counter", envir = .GlobalEnv) + 1
        # Add the matching item as a separate list element
        result[[length(result) + 1]] <- item
        # Set the name of this element with the index
        names(result)[length(result)] <- paste0(what, "_", match_counter)
      }
    }
    # Remove the counter from the environment when the top-level call finishes
    if (sys.nframe() == 1) {
      rm("match_counter", envir = .GlobalEnv)
    }
    return(result)
  }

  result <- search_list(ResIN_boots_executed, what = what)
  rm("match_counter", envir = .GlobalEnv)

  ### Optional summarize function
  sum_res <- function(result) {
    res_sum <- c(min(result, na.rm = TRUE),
                 quantile(result, 0.05, na.rm = TRUE),
                 quantile(result, 0.25, na.rm = TRUE),
                 median(result, na.rm = TRUE),
                 mean(result, na.rm = TRUE),
                 quantile(result, 0.75, na.rm = TRUE),
                 quantile(result, 0.95, na.rm = TRUE),
                 max(result, na.rm = TRUE),
                 sqrt(var(result, na.rm = TRUE)))

    names(res_sum) <- c("min", "5th perct.", "25th perct.", "median", "mean",
                        "75th perct.", "95th perct.", "max", "stdn. dev.")
    return(res_sum)
  }

  ### Return the extracted quantities
  if(summarize_results==FALSE) {
    return(result)} else {
      res_sum <-  sum_res(result)
      return(res_sum)
    }
}




