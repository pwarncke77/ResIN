#' @title ResIN_utils
#'
#' @description Generates auxiliary utensils useful for Response-Item Networks analysis.
#'
#' @param df A data-frame object containing the raw data.
#' @param node_vars An optional character string detailing the attitude item columns to be selected for ResIN analysis (i.e. the subset of attitude variables in df).
#' @param cor_method Which correlation method should be used? Defaults to "auto" which applies the \code{cor_auto} function from the \code{qgraph} package. Possible arguments are \code{"auto"}, \code{"pearson"}, \code{"kendall"}, and \code{"spearman"}.
#' @param weights An optional continuous vector of survey weights. Should have the same length as number of observations in df. If weights are provided, weighted correlation matrix will be estimated with the \code{weightedCorr} function from the \code{wCorr} package.
#' @param method_wCorr If weights are supplied, which method for weighted correlations should be used? Defaults to \code{"Polychoric"}. See \code{wCorr::weightedCorr} for all correlation options.
#' @param remove_negative Should all negative correlations be removed? Defaults to TRUE (highly recommended). Setting to FALSE makes it impossible to estimate a force-directed network layout. Function will use igraph::layout_nicely instead.
#' @param EBICglasso Should a sparse, Gaussian-LASSO ResIN network be estimated? Defaults to FALSE. If set to TRUE, \code{EBICglasso} function from the \code{qgraph} packages performs regularization on (nearest positive-semi-definite) ResIN correlation matrix.
#' @param EBICglasso_arglist An argument list feeding additional instructions to the \code{EBICglasso} function if \code{EBICglasso} is set to TRUE.
#'
#' @return A list object containing the original dataframe, (\code{resin_df}), the dummy-coded dataframe (\code{resin_dummies}), the ResIN correlation and covariance matrices (\code{resin_cor} & \code{resin_vcov}), and a numeric vector detailing which item responses belong to which item (\code{same_items}).
#' @examples
#'
#' ## Load the 12-item simulated Likert-type ResIN toy dataset
#' data(lik_data)
#'
#' ## Extract the utilities
#' output <- ResIN_utils(lik_data)
#'
#' @export
#' @importFrom dplyr "%>%" "select" "left_join" "all_of"
#' @importFrom stats "complete.cases" "cor" "sd" "prcomp" "cov" "princomp"
#' @importFrom fastDummies "dummy_cols"
#' @importFrom qgraph "qgraph" "cor_auto" "centrality_auto" "EBICglasso"
#' @importFrom igraph "graph_from_adjacency_matrix" "cluster_leading_eigen" "layout_nicely" "layout_with_fr" "membership" "plot.igraph"
#' @importFrom wCorr "weightedCorr"
#' @importFrom Matrix "nearPD"
#' @importFrom DirectedClustering "ClustF"
#'
#' @references Epskamp S, Cramer AOJ, Waldorp LJ, Schmittmann VD, Borsboom D (2012). “qgraph: Network Visualizations of Relationships in Psychometric Data.” Journal of Statistical Software, 48(4), 1–18.



ResIN_utils <- function(df, node_vars = NULL, cor_method = "auto", weights = NULL,
                  method_wCorr = "Polychoric", remove_negative = TRUE,
                  EBICglasso = FALSE, EBICglasso_arglist = NULL) {

  ## Select response node_vars
  if(is.null(node_vars)) {
    df_nodes <- df
  } else {
    df_nodes <- df %>% dplyr::select(dplyr::all_of(node_vars))
  }

  ## Make the dummy frame
  df_nodes <- as.data.frame(apply(df_nodes, 2, factor))
  df_nodes[df_nodes=="NA"] <- NA ## Re-setting NA's
  df_dummies <- fastDummies::dummy_cols(df_nodes, ignore_na=TRUE,
                                        remove_selected_columns=TRUE)

  ## Generating correlation matrices
  if(is.null(weights)) {

    if(cor_method=="auto") {
      res_in_cor <- qgraph::cor_auto(df_dummies)
      res_in_vcov <- cov(df_dummies)
    }

    if(cor_method %in% c("pearson", "kendall", "spearman")) {
      res_in_cor <- cor(df_dummies, method = cor_method, use = "pairwise.complete.obs")
      res_in_vcov <- cov(df_dummies)
    }

  } else {

    res_in_cor <- matrix(NA, ncol(df_dummies), ncol(df_dummies))

    for(i in 1:ncol(df_dummies))  {
      for(j in 1:ncol(df_dummies))  {

        temp <- as.data.frame(cbind(df_dummies[, i], df_dummies[, j], df[, weights]))
        temp <- temp[complete.cases(temp), ]

        res_in_cor[i, j]  <- wCorr::weightedCorr(temp[, 1], temp[, 2], weights=temp[, 3],
                                                 method = method_wCorr)
      }
    }

    colnames(res_in_cor) <- colnames(df_dummies)
    rownames(res_in_cor) <- colnames(df_dummies)
  }

  ## Perform regularization (optional)
  if(EBICglasso==TRUE) {

    diag(res_in_cor) <- 1
    res_in_cor <- as.matrix(Matrix::nearPD(res_in_cor)$mat)

    if(is.null(EBICglasso_arglist)) {
      EBICglasso_arglist <- list(n = nrow(df), gamma = 0.5, penalize.diagonal = FALSE,
                                 nlambda = 100,
                                 returnAllResults = FALSE, checkPD = FALSE,
                                 countDiagonal = FALSE, refit = FALSE,
                                 threshold = FALSE, verbose = TRUE)
    }

    res_in_cor <- do.call(qgraph::EBICglasso, c(list(S = as.matrix(res_in_cor)),
                                                EBICglasso_arglist))

    j <- 1 ; i <- 1
    while(i <= ncol(df_nodes)) {
      res_in_cor[j:((j+length(levels(factor(df_nodes[, i]))))-1),
                 j:((j+length(levels(factor(df_nodes[, i]))))-1)] <- 0
      j <- j+length(levels(factor(df_nodes[, i]))); i <- i+1

    }
  }

  ## Set all inner-variable correlations to 0
  j <- 1 ; i <- 1
  while(i <= ncol(df_nodes)) {
    res_in_cor[j:((j+length(levels(factor(df_nodes[, i]))))-1),
               j:((j+length(levels(factor(df_nodes[, i]))))-1)] <- 0

    res_in_vcov[j:((j+length(levels(factor(df_nodes[, i]))))-1),
                j:((j+length(levels(factor(df_nodes[, i]))))-1)] <- 0

    j <- j+length(levels(factor(df_nodes[, i]))); i <- i+1
  }

  ## Removing NA's and negatives
  if(remove_negative==TRUE) {
    res_in_cor[res_in_cor<0] <- 0
    res_in_vcov[res_in_vcov<0] <- 0
  }

  res_in_cor[is.na(res_in_cor)] <- 0
  res_in_vcov[is.na(res_in_vcov)] <- 0

  ## Creating the same-items list
  same_items <- rep(NA, ncol(res_in_cor))
  j <- 1 ; i <- 1
  while(i <= ncol(df_nodes)) {
    same_items[j:((j+length(levels(factor(df_nodes[, i]))))-1)] <- i
    j <- j+length(levels(factor(df_nodes[, i]))); i <- i+1
  }

  same_items <- as.factor(same_items)
  levels(same_items) <- colnames(df_nodes)

  ### END FUNCTION
  output <- list(df_nodes, df_dummies, res_in_cor, res_in_vcov, same_items)
  names(output) <- c("resin_df", "resin_dummies", "resin_cor", "resin_vcov", "same_items")

  return(output)
}

