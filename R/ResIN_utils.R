ResIN_utils <- function(df, node_vars = NULL, cor_method = "auto", weights = NULL,
                  method_wCorr = "Polychoric", remove_negative = TRUE,
                  EBICglasso = FALSE, EBICglasso_arglist = NULL) {

  set.seed(seed)

  ## Select response node_vars
  if(is.null(node_vars)) {
    df_nodes <- df
  } else {
    df_nodes <- df %>% dplyr::select(all_of(node_vars))
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

