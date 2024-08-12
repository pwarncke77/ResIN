#' @title ResIN_boots_prepare
#'
#' @description Prepare a ResIN-based bootstrap analysis
#'
#' @param ResIN_object A ResIN object to prepare bootstrapping workflow.
#' @param n Bootstrapping sample size. Defaults to 10.000.
#' @param boots_type What kind of bootstrapping should be performed? If set to "resample", function performs row-wise re-sampling of raw data (useful for e.g., sensitivity or power analysis). If set to "permute", function will randomly reshuffle raw item responses (useful e.g., for simulating null-hypothesis distributions). Defaults to "resample".
#' @param resample_size Optional parameter determining sample size when \code{boots_type} is set to "resample". Defaults of to number of rows in raw data.
#' @param save_input Should all input information for each bootstrap iteration (including re-sampled/permuted data) be stored. Set to FALSE by default to save a lot of memory and disk storage.
#' @param seed Random seed for bootstrap samples
#'
#' @return A list object containing n re-sampled or permuted copies of the raw data, along with a list of instructions for how to perform the ResIN analysis and what outputs to generate.
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
#' @importFrom dplyr "sample_n" "select" "all_of"
#'

ResIN_boots_prepare <- function(ResIN_object, n=10000, boots_type="resample", resample_size=NULL, save_input=FALSE, seed = 42){
  ## Validation tests
  if(class(ResIN_object)[2] !=  "ResIN"){
    stop("Please supply a ResIN type list object.")
  }
  if(! boots_type %in% c("resample", "permute")){
    stop("Please speficy either 'resample' or 'permute' for boots_type.")
  }
  if(is.null(resample_size)){
    resample_size <- nrow(ResIN_object$aux_objects$df_dummies)
  }
  if(save_input==FALSE){
    ResIN_object$aux_objects$ResIN_arglist$save_input <- FALSE
  }
  ## Assuming no-one wants to print 10k plots
  ResIN_object$aux_objects$ResIN_arglist$plot_ggplot <- FALSE

  ## Setting up base data frame and outcome list
  set.seed(seed)
  df <- ResIN_object$aux_objects$ResIN_arglist$df
  mega_list <- vector("list", length = n)

  ## Re-sampling type bootstrap
  if(boots_type == "resample"){
    for(i in 1:n){
      mega_list[[i]] <- ResIN_object$aux_objects$ResIN_arglist
      mega_list[[i]]$df <- dplyr::sample_n(df, resample_size)
      }
    }

  ## Permutation bootstrap
  if(boots_type == "permute"){
    for(i in 1:n){
      mega_list[[i]] <- ResIN_object$aux_objects$ResIN_arglist
      for(j in 1:ncol(df)){
        mega_list[[i]]$df[, j] <- sample(df[, j])
      }
    }
  }

  class(mega_list) <- c("list", "ResIN_boots_prepped")
  return(mega_list)
}



