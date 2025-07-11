#' @title ResIN_boots_execute
#'
#' @description Execute prepared ResIN bootstrap analysis
#'
#' @param ResIN_boots_prepped A list of prepared ResIN objects for bootstrapping (outcome of the \code{ResIN_boots_prepare} function)
#' @param parallel Should the function be executed in parallel using the \code{foreach} package? Defaults to FALSE. If FALSE, function will execute sequentially in a simple for loop.
#' @param detect_cores Should the number of available CPU cores be automatically detected? Defaults to TRUE and is ignored when parallel is set to FALSE.
#' @param core_offset Optionally, specify a positive integer offset that is subtracted from the number of automatically detected cores. Defaults to 0L.
#' @param n_cores Manually specify the number of available CPU cores. Defaults to 2L and is ignored if detect_cores is set to TRUE or if parallel is set to FALSE.
#' @param inorder Should parallel execution be done in sequential order of the \code{ResIN_boots_prepped} object?
#'
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
#' @importFrom foreach "foreach" "%dopar%"
#' @importFrom parallelly "availableCores"
#' @importFrom parallel "makeCluster"
#' @importFrom doSNOW "registerDoSNOW"
#' @importFrom utils "setTxtProgressBar" "txtProgressBar"
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


