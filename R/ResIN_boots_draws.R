#' Extract bootrstrap draws from ResIN objects
#'
#' \code{"ResIN_boots_draws"} objects are typically returned by \code{\link{ResIN_boots_extract}}
#' when the requested quantity is scalar per bootstrap iteration (e.g., \code{"global_clustering"}).
#' The object is a numeric vector with attributes describing the bootstrap context.
#'
#' @details
#' Common attributes include:
#' \describe{
#'   \item{\code{what}}{Name of the extracted quantity.}
#'   \item{\code{n_total}, \code{n_ok}, \code{n_failed}}{Counts of total, successful, and failed iterations.}
#'   \item{\code{plan}}{The \code{"ResIN_boots_prepped"} plan used to generate the results (if attached).}
#'   \item{\code{created}}{Timestamp of execution (if attached).}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{print(x)}}{Compact display including median and 95\% CI.}
#'   \item{\code{summary(object)}}{Return descriptive statistics and quantiles.}
#'   \item{\code{confint(object)}}{Quantile-based confidence intervals.}
#'   \item{\code{plot(x)}}{Histogram with CI markers.}
#' }
#'
#' @name ResIN_boots_draws
#' @aliases ResIN_boots_draws
NULL
