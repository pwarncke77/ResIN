
ResIN <- function(df, node_vars = NULL, cor_method = "auto", weights = NULL,
                      method_wCorr = "Polychoric", remove_negative = TRUE,
                      EBICglasso = FALSE, EBICglasso_arglist = NULL,
                      node_covars = NULL, node_costats = NULL,
                      edge_stats = NULL, network_stats = FALSE,
                      cluster = TRUE, seed = 42) {

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

  ## Generating the qgraph and igraph objects
  ResIN_igraph <- igraph::graph_from_adjacency_matrix(res_in_cor, mode = "undirected", weighted = TRUE, diag = FALSE)

  ResIN_qgraph <- qgraph::qgraph(res_in_cor, DoNotPlot = TRUE, layout = "spring")

  ## Network statistics (common structuration and centralization metrics)
  if(network_stats==TRUE) {
    node_net_stats <- qgraph::centrality_auto(ResIN_qgraph, weighted = TRUE)
    structuration <- apply(node_net_stats$node.centrality, 2, FUN = mean)
    structuration[5] <- mean(node_net_stats$ShortestPathLengths)
    structuration[6] <- DirectedClustering::ClustF(res_in_cor)$GlobalCC
    names(structuration) <- c("Betweenness", "Closeness", "Strength", "Expected Influence", "Average Path Length", "Global Clustering")

    centralization <- apply(node_net_stats$node.centrality, 2, FUN = sd)
  } else {
    structuration <- c("not estimated")
    centralization <- c("not estimated")
  }

  ## Calculating summary statistics based on co-variates
  if(!(is.null(node_covars)) & !(is.null(node_costats))) {

    if(length(node_covars) != length(node_covars)) {
      stop("Covariate selection and summary statistics vectors must be of equal length.")
    }

    covars_frame <- dplyr::select(df, node_covars)
    cov_stats <- as.data.frame(matrix(NA, length(same_items), length(node_covars)))

    for(i in 1:length(same_items)) {
      for(j in 1:length(node_covars)) {

        cov_stats[i, j] <- do.call(node_costats[j], c(list(x = covars_frame[, j][df_dummies[, i] == 1], na.rm = TRUE)))

      }
    }

    colnames(cov_stats) <- paste(node_covars, node_costats, sep = "_")
    cov_stats$node_label <- colnames(res_in_cor)
  }

  ## Generating and merging the basic plotting dataframe with network and covariate stats
  if(remove_negative==FALSE) {
    graph_layout <- as.data.frame(prcomp(igraph::layout_nicely(ResIN_igraph))$x)
  } else {
    graph_layout <- as.data.frame(prcomp(igraph::layout_with_fr(ResIN_igraph))$x)
  }

  graph_layout$node_names <- colnames(res_in_cor)
  colnames(graph_layout) <- c("x", "y", "node_names")

  plotting_frame <- graph_layout
  plotting_frame$from <- plotting_frame$node_names

  if(network_stats==TRUE) {
    plotting_frame <- cbind(plotting_frame, node_net_stats$node.centrality)}

  if(!(is.null(node_covars)) & !(is.null(node_costats))) {
    plotting_frame <- cbind(plotting_frame, cov_stats)
  }

  ## Perform clustering analysis
  if(cluster==TRUE) {
    cluster <- cluster_leading_eigen(ResIN_igraph)
    communities <- membership(cluster)
    nodes <- names(communities)
    outcome <- as.data.frame(cbind(as.numeric(communities), nodes))
    colnames(outcome) <- c("dimension", "from")
    outcome$dimension <- as.numeric(outcome$dimension)

    x <- length(unique(outcome$dimension))
    i <- 1

    while(i <= x) {
      if(length(outcome$dimension[outcome$dimension==i]) < 3) {
        outcome$dimension[outcome$dimension==i] <- rep("NA", length(outcome$dimension[outcome$dimension==i]))
      }
      i <- i+1}

    outcome$dimension[outcome$dimension=="NA"] <- NA
    outcome$dimension <- as.numeric(outcome$dimension)

    plotting_frame <- left_join(plotting_frame, outcome, by = "from")}

  ## Preparing plotting data for ggplot graph format
  g <- igraph::as_data_frame(ResIN_igraph)
  g$from.x <- plotting_frame$x[match(g$from, plotting_frame$node_names)]
  g$from.y <- plotting_frame$y[match(g$from, plotting_frame$node_names)]
  g$to.x <- plotting_frame$x[match(g$to, plotting_frame$node_names)]
  g$to.y <- plotting_frame$y[match(g$to, plotting_frame$node_names)]

  g_plus <- left_join(g, plotting_frame, by = "from")

  ### END FUNCTION
  output <- list(res_in_cor, same_items, g_plus, plotting_frame, structuration, centralization, df_dummies, res_in_cor_neg, res_in_cor_orig, res_in_vcov)
  names(output) <- c("adj_matrix", "same_items", "ggplot_frame", "outcome_frame", "graph_structuration", "graph_centralization", "df_dummies", "neg_cor_matrix", "res_in_cor_orig", "res_in_vcov")

  return(output)
}
