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

ResIN_boots_execute <- function(ResIN_boots_prepped, parallel = FALSE, detect_cores = TRUE, core_offset = 0L, n_cores = 2L, inorder = FALSE) {
  if(class(ResIN_boots_prepped)[2] !=  "ResIN_boots_prepped"){
    stop("Please supply a 'ResIN_boots_prepped' type list object.")
  }

  if(parallel==TRUE){
    if(detect_cores==TRUE){
      n_cores <- parallelly::availableCores()
    }

   cl <- parallel::makeCluster(n_cores[1])
   doSNOW::registerDoSNOW(cl)

   pb <- txtProgressBar(max = length(ResIN_boots_prepped), style = 3)
   progress <- function(n) setTxtProgressBar(pb, n)
   opts <- list(progress = progress)

   res_list <- foreach::foreach(i=1:length(ResIN_boots_prepped), .inorder = inorder, .options.snow = opts) %dopar% {
     do.call(ResIN, ResIN_boots_prepped[[i]])
   }

   parallel::stopCluster(cl)

  } else {
   res_list <- vector("list", length(ResIN_boots_prepped))
   for(i in 1:length(res_list)){
     res_list[[i]] <- do.call(ResIN, ResIN_boots_prepped[[i]])
   }
  }


  class(res_list) <- c("list", "ResIN_boots_executed")
  return(res_list)
}


