#' @title ResIN
#'
#' @description Performs Response Item-Network analysis (ResIN)
#'
#' @param df A data-frame object containing the raw data.
#' @param node_vars An optional character string detailing the attitude item columns to be selected for ResIN analysis (i.e. the subset of attitude variables in df).
#' @param cor_method Which correlation method should be used? Defaults to "auto" which applies the \code{cor_auto} function from the \code{qgraph} package. Possible arguments are \code{"auto"}, \code{"pearson"}, \code{"kendall"}, and \code{"spearman"}.
#' @param weights An optional continuous vector of survey weights. Should have the same length as number of observations in df. If weights are provided, weighted correlation matrix will be estimated with the \code{weightedCorr} function from the \code{wCorr} package.
#' @param method_wCorr If weights are supplied, which method for weighted correlations should be used? Defaults to \code{"Polychoric"}. See \code{wCorr::weightedCorr} for all correlation options.
#' @param poly_ncor How many CPU cores should be used to estimate polychoric correlation matrix? Only used if \code{cor_method = "polychoric"}.
#' @param remove_negative Should all negative correlations be removed? Defaults to TRUE (highly recommended). Setting to FALSE makes it impossible to estimate a force-directed network layout. Function will use igraph::layout_nicely instead.
#' @param EBICglasso Should a sparse, Gaussian-LASSO ResIN network be estimated? Defaults to FALSE. If set to TRUE, \code{EBICglasso} function from the \code{qgraph} packages performs regularization on (nearest positive-semi-definite) ResIN correlation matrix.
#' @param EBICglasso_arglist An argument list feeding additional instructions to the \code{EBICglasso} function if \code{EBICglasso} is set to TRUE.
#' @param node_covars An optional character string selecting quantitative covariates that can be used to enhance ResIN analysis. Typically, these covariates provide grouped summary statistics for item response nodes. (E.g.: What is the average age or income level of respondents who selected a particular item response?) Variable names specified here should match existing columns in \code{df}.
#' @param node_costats If any \code{node_covars} are selected, what summary statistics should be estimated from them? Argument should be a character vector of the same length of \code{node_covars}and call a base-R function. (E.g. \code{"mean"}, \code{"median"}, \code{"sd"}). The first element in \code{node_costats} specifies the summary statistic extracted from the first element in \code{node_covars}, and so on.
#' @param network_stats Should common network structuration and centralization metrics be extracted? Calls \code{qgraph::centrality_auto} and \code{DirectedClustering::ClustF} to the ResIN graph object to extract network average betweenness, closeness, strength centrality (mean) and centralization scores (sd). Also estimates network expected influence, average path length, and global clustering coefficients.
#' @param cluster Optional, should community detection be performed on item response network? Defaults to FALSE. If set to TRUE, performs "cluster_leading_eigen" function from the igraph package and stores results in node_frame.
#' @param seed Random seed for force-directed algorithm.
#'
#' @return A list object containing the ResIN adjacency matrix (\code{adj_matrix}), a numeric vector detailing which item responses belong to which item (\code{same_items}), a ggplot-ready edge-list type dataframe (\code{edgelist_frame}), a node-level dataframe (\code{node_frame}), a vector with the optional graph structuration (\code{graph_structuration}) and centralization (\code{graph_centralization}) statistics, as well as the dummy-coded item-response dataframe (\code{df_dummies}).
#'
#' @examples
#'
#' ## Load the 12-item simulated Likert-type ResIN toy dataset
#' data(lik_data)
#' library(ggplot2)
#'
#' # Apply the ResIN function to toy Likert data:
#' output <- ResIN(lik_data, cor_method = "spearman", network_stats = TRUE)
#'
#' # Create a basic outcome plot with ggplot
#' output$edgelist_frame <- output$edgelist_frame[order(output$edgelist_frame$Strength,
#'                                                  decreasing = FALSE), ]
#' ResIN_plot <- ggplot2::ggplot(output$edgelist_frame)+
#'   geom_curve(data = output$edgelist_frame, aes(x = from.x, xend = to.x, y = from.y,
#'                                              yend = to.y, linewidth = weight,
#'                                              color = Strength), curvature = 0.2)+
#'   geom_point(aes(x = from.x, y = from.y, shape = as.factor(cluster)), size = 8)+
#'   geom_point(aes(x = to.x, y = to.y), size = 8)+
#'   geom_text(data = output$edgelist_frame, aes(x = from.x, y = from.y, label = from),
#'             size = 3, color = "white")+
#'   geom_text(data = output$edgelist_frame, aes(x = to.x, y = to.y, label = to),
#'             size = 3, color = "white")+
#'   ggtitle("ResIN example  plot")+
#'   theme_dark()+
#'   theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
#'         axis.text.y = element_blank(), axis.title.y = element_blank(),
#'         axis.ticks = element_blank(), panel.grid.major = element_blank(),
#'         panel.grid.minor = element_blank(), legend.position = "none",
#'         legend.text = element_blank(), plot.title = element_text(hjust = 0.5))
#'
#' ResIN_plot
#'
#' @export
#' @importFrom ggplot2 "ggplot" "geom_curve"
#' @importFrom dplyr "%>%" "select" "left_join" "all_of"
#' @importFrom stats "complete.cases" "cor" "sd" "prcomp" "cov" "princomp"
#' @importFrom fastDummies "dummy_cols"
#' @importFrom qgraph "qgraph" "cor_auto" "centrality_auto" "EBICglasso" "qgraph.layout.fruchtermanreingold"
#' @importFrom igraph "graph_from_adjacency_matrix" "cluster_leading_eigen" "layout_nicely" "layout_with_fr" "membership"
#' @importFrom wCorr "weightedCorr"
#' @importFrom Matrix "nearPD"
#' @importFrom DirectedClustering "ClustF"
#'


ResIN <- function(df, node_vars = NULL, cor_method = "auto", weights = NULL,
                      method_wCorr = "Polychoric", poly_ncor = 2,
                      remove_negative = TRUE,
                      EBICglasso = FALSE, EBICglasso_arglist = NULL,
                      node_covars = NULL, node_costats = NULL,
                      network_stats = FALSE,
                      cluster = FALSE, seed = 42) {

  set.seed(seed)

  ## Select response node_vars
  if(is.null(node_vars)) {
    df_nodes <- df
  } else {
    df_nodes <- df %>% dplyr::select(dplyr::all_of(node_vars))
  }

  ## Make the dummy frame
  df_nodes <- as.data.frame(apply(df_nodes, 2, factor))
  df_nodes[df_nodes == "NA"] <- NA ## Re-setting NA's
  df_dummies <- fastDummies::dummy_cols(df_nodes, ignore_na=TRUE,
                                        remove_selected_columns=TRUE)

  ## Generating correlation matrices
  if(is.null(weights)) {

    if(cor_method == "auto") {
      res_in_cor <- qgraph::cor_auto(df_dummies)
    }

    if(cor_method %in% c("pearson", "kendall", "spearman")) {
      res_in_cor <- cor(df_dummies, method = cor_method, use = "pairwise.complete.obs")

    }

    ### Weighted correlations:
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

    j <- j+length(levels(factor(df_nodes[, i]))); i <- i+1
  }

  ## Removing NA's and negatives
  if(remove_negative==TRUE) {
    res_in_cor[res_in_cor<0] <- 0
  }

  res_in_cor[is.na(res_in_cor)] <- 0

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
    names(structuration) <- c("betweenness", "closeness", "strength", "expected_influence", "average_path_length", "global_clustering")

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

  node_frame <- graph_layout
  node_frame$from <- node_frame$node_names

  if(network_stats==TRUE) {
    node_frame <- cbind(node_frame, node_net_stats$node.centrality)}

  if(!(is.null(node_covars)) & !(is.null(node_costats))) {
    node_frame <- cbind(node_frame, cov_stats)
  }

  ## Perform clustering analysis
  if(cluster==TRUE) {
    cluster <- igraph::cluster_leading_eigen(ResIN_igraph)
    communities <- igraph::membership(cluster)
    nodes <- names(communities)
    outcome <- as.data.frame(cbind(as.numeric(communities), nodes))
    colnames(outcome) <- c("cluster", "from")
    outcome$cluster <- as.numeric(outcome$cluster)

    x <- length(unique(outcome$cluster))
    i <- 1

    while(i <= x) {
      if(length(outcome$cluster[outcome$cluster==i]) < 3) {
        outcome$cluster[outcome$cluster==i] <- rep("NA", length(outcome$cluster[outcome$cluster==i]))
      }
      i <- i+1}

    outcome$cluster[outcome$cluster=="NA"] <- NA
    outcome$cluster <- as.numeric(outcome$cluster)

    node_frame <- dplyr::left_join(node_frame, outcome, by = "from")}

  ## Preparing plotting data for ggplot graph format
  g <- igraph::as_data_frame(ResIN_igraph)
  g$from.x <- node_frame$x[match(g$from, node_frame$node_names)]
  g$from.y <- node_frame$y[match(g$from, node_frame$node_names)]
  g$to.x <- node_frame$x[match(g$to, node_frame$node_names)]
  g$to.y <- node_frame$y[match(g$to, node_frame$node_names)]

  edgelist_frame <- dplyr::left_join(g, node_frame, by = "from")

  ### END FUNCTION
  output <- list(res_in_cor, same_items, edgelist_frame, node_frame, structuration, centralization, df_dummies)
  names(output) <- c("adj_matrix", "same_items", "edgelist_frame", "node_frame", "graph_structuration", "graph_centralization", "df_dummies")

  return(output)
}



