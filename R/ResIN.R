#' @title ResIN
#'
#' @description Performs Response Item-Network (ResIN) analysis
#'
#' @param df A data-frame object containing the raw data.
#' @param node_vars An optional character vector detailing the attitude item columns to be selected for ResIN analysis (i.e. the subset of attitude variables in df).
#' @param left_anchor An optional character scalar indicating a particular response node which determines the spatial orientation of the ResIN latent space. If this response node does not appear on the left-hand side, the x-plane will be inverted. This ensures consistent interpretation of the latent space across multiple iterations (e.g. in bootstrapping analysis). Defaults to NULL (no adjustment to orientation is taken.)
#' @param cor_method Which correlation method should be used? Defaults to "auto" which applies the \code{cor_auto} function from the \code{qgraph} package. Possible arguments are \code{"auto"}, \code{"pearson"}, \code{"kendall"}, and \code{"spearman"}.
#' @param weights An optional continuous vector of survey weights. Should have the same length as number of observations in df. If weights are provided, weighted correlation matrix will be estimated with the \code{weightedCorr} function from the \code{wCorr} package.
#' @param method_wCorr If weights are supplied, which method for weighted correlations should be used? Defaults to \code{"Polychoric"}. See \code{wCorr::weightedCorr} for all correlation options.
#' @param poly_ncor How many CPU cores should be used to estimate polychoric correlation matrix? Only used if \code{cor_method = "polychoric"}.
#' @param neg_offset Should negative correlations be offset to avoid small correlation pairs disappearing? Defaults to \code{0}. Any positive number between 0 and 1 may be supplied instead.
#' @param ResIN_scores Should spatial scores be calculated for every individual. Defaults to TRUE. Function obtains the mean positional score on the major (x-axis) and minor (y-axis). Further versions of this package will include more sophisticated scoring techniques.
#' @param remove_negative Should all negative correlations be removed? Defaults to TRUE (highly recommended). Setting to FALSE makes it impossible to estimate a force-directed network layout. Function will use igraph::layout_nicely instead.
#' @param EBICglasso Should a sparse, Gaussian-LASSO ResIN network be estimated? Defaults to FALSE. If set to TRUE, \code{EBICglasso} function from the \code{qgraph} packages performs regularization on (nearest positive-semi-definite) ResIN correlation matrix.
#' @param EBICglasso_arglist An argument list feeding additional instructions to the \code{EBICglasso} function if \code{EBICglasso} is set to TRUE.
#' @param remove_nonsignificant Optionally, should non-significant edges be removed from the ResIN network? Defaults to FALSE. Note that this option is incompatible with EBICglasso and weighted correlations.
#' @param sign_threshold At what p-value threshold should non-significant edges be removed? Defaults to 0.05.
#' @param node_covars An optional character string selecting quantitative covariates that can be used to enhance ResIN analysis. Typically, these covariates provide grouped summary statistics for item response nodes. (E.g.: What is the average age or income level of respondents who selected a particular item response?) Variable names specified here should match existing columns in \code{df}.
#' @param node_costats If any \code{node_covars} are selected, what summary statistics should be estimated from them? Argument should be a character vector and call a base-R function. (E.g. \code{"mean"}, \code{"median"}, \code{"sd"}). Each element specified in \code{node_costats} is applied to each element in \code{node_covars} and the out-put is stored as a node-level summary statistic in the \code{ResIN_nodeframe}. The extra columns in \code{ResIN_nodeframe} are labeled according to the following template: "covariate name"_"statistic". So for the respondents mean age, the corresponding column in \code{ResIN_nodeframe} would be labeled as "age_mean".
#' @param network_stats Should common node- and graph level network statistics be extracted? Calls \code{qgraph::centrality_auto} and \code{DirectedClustering::ClustF} to the ResIN graph object to extract node-level betweenness, closeness, strength centrality, as well as the mean and standard deviation of these scores at the network level. Also estimates network expected influence, average path length, and global clustering coefficients. Defaults to TRUE. Set to FALSE if estimation takes a long time.
#' @param detect_clusters Optional, should community detection be performed on item response network? Defaults to FALSE. If set to TRUE, performs a clustering method from the [igraph](https://igraph.org/r/doc/cluster_leading_eigen.html) library and stores the results in the \code{ResIN_nodeframe} output.
#' @param cluster_method A character scalar specifying the [igraph-based](https://igraph.org/r/doc/communities.html) community detection function.
#' @param cluster_arglist An optional list specifying additional arguments to the selected [igraph](https://igraph.org/r/doc/communities.html) clustering method.
#' @param cluster_assignment Should individual (survey) respondents be assigned to different clusters? If set to TRUE, function will generate an n*c matrix of probabilities for each respondent to be assigned to one of c clusters. Furthermore, a vector of length n is generated displaying the most likely cluster respondents belong to. In case of a tie between one or more clusters, a very small amount of random noise determines assignment. Both matrix and vectors are added to the \code{aux_objects} list. Defaults to FALSE and will be ignored if \code{detect_clusters} is set to FALSE.
#' @param generate_ggplot Should a ggplot-based visualization of the ResIN network be generated? Defaults to TRUE.
#' @param plot_ggplot Should a basic ggplot of the ResIN network be plotted? Defaults to TRUE. If set to FALSE, the ggplot object will not be directly returned to the console. (However, if generate_ggplot=TRUE, the plot will still be generated and stored alongside the other output objects.)
#' @param plot_whichstat Should a particular node-level metric be color-visualized in the ggplot output? For node cluster, specify "cluster". For the same Likert response choices or options, specify "choices". For a particular node-level co-variate please specify the name of the particular element in \code{node_covars} followed by a "_" and the specific \code{node_costats} you would like to visualize. For instance if you want the visualize average age at the node-level, you should specify "age_mean". To colorize by node centrality statistics, possible choices are "Strength", "Betweenness", "Closeness", and "ExpectedInfluence". Defaults to NULL. Make sure to supply appropriate choices to \code{node_covars}, \code{node_costats}, \code{detect_clusters}, and/or \code{network_stats} prior to setting this argument.
#' @param plot_edgestat Should the thickness of the edges be adjusted according to a particular co-statistic? Defaults to NULL. Possible choices are "weight" for the bi-variate correlation strength, and "edgebetweenness"
#' @param color_palette Optionally, you may specify the ggplot2 color palette to be applied to the plot. All options contained in [\code{RColorBrewer}](https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf) (for discrete colors such as cluster assignments) and [\code{ggplot2::scale_colour_distiller}](https://ggplot2.tidyverse.org/reference/scale_brewer.html) are supported. Defaults to "RdBu".
#' @param plot_responselabels Should response labels be plotted via \code{geom_text}? Defaults to TRUE. It is recommended to set to FALSE if the network possesses a lot of nodes and/or long response choice names.
#' @param response_levels An optional character vector specifying the correct order of global response levels. Only useful if all node-items follow the same convention (e.g. ranging from "strong disagreement" to "strong agreement"). The supplied vector should have the same length as the total number of response options and supply these (matching exactly) in the correct order. E.g. c("Strongly Agree", "Somewhat Agree", "Neutral", "Somewhat Disagree", "Strongly Disagree"). Defaults to NULL.
#' @param plot_title Optionally, a character scalar specifying the title of the ggplot output. Defaults to "ResIN plot".
#' @param save_input Optionally, should input data and function arguments be saved (this is necessary for running ResIN_boots_prepare function). Defaults to TRUE.
#' @param seed Random seed for force-directed algorithm. Defaults to NULL (no seed is set.) If scalar integer is supplied, that seed will be set prior to analysis.
#'
#' @return An edge-list type data-frame, \code{ResIN_edgelist}, a node-level data-frame, \code{ResIN_nodeframe}, an n*2 data-frame of individual-level spatial scores along the major (x) and minor(y) axis, \code{ResIN_scores} a list of graph-level statistics \code{graph_stats} including (\code{graph_structuration}) and centralization (\code{graph_centralization}), as well as a list of auxiliary objects, \code{aux_objects}, including the ResIN adjacency matrix (\code{adj_matrix}), a numeric vector detailing which item responses belong to which item (\code{same_items}), and the dummy-coded item-response data-frame (\code{df_dummies}).
#'
#' @examples
#'
#' ## Load the 12-item simulated Likert-type toy dataset
#' data(lik_data)
#'
#' # Apply the ResIN function to toy Likert data:
#' ResIN_obj <- ResIN(lik_data, cor_method = "spearman", network_stats = TRUE, detect_clusters = TRUE)
#'
#'
#' @export
#' @importFrom ggplot2 "ggplot" "geom_curve" "geom_point" "geom_text" "ggtitle" "scale_color_continuous" "scale_color_discrete" "aes" "element_blank" "element_text" "theme" "theme_classic" "coord_fixed"
#' @importFrom dplyr "select" "left_join" "all_of" "mutate"
#' @importFrom stats "complete.cases" "cor" "sd" "prcomp" "cov" "princomp"
#' @importFrom fastDummies "dummy_cols"
#' @importFrom qgraph "qgraph" "cor_auto" "centrality_auto" "EBICglasso" "qgraph.layout.fruchtermanreingold"
#' @importFrom igraph "graph_from_adjacency_matrix" "cluster_leading_eigen" "layout_nicely" "layout_with_fr" "membership"
#' @importFrom wCorr "weightedCorr"
#' @importFrom Matrix "nearPD"
#' @importFrom DirectedClustering "ClustF"
#' @importFrom psych "corr.test"
#' @importFrom shadowtext "geom_shadowtext"
#'

ResIN <- function(df, node_vars = NULL, left_anchor = NULL, cor_method = "auto", weights = NULL,
                      method_wCorr = "Polychoric", poly_ncor = 2, neg_offset = 0,
                      ResIN_scores = TRUE, remove_negative = TRUE,
                      EBICglasso = FALSE, EBICglasso_arglist = NULL,
                      remove_nonsignificant = FALSE, sign_threshold = 0.05,
                      node_covars = NULL, node_costats = NULL,
                      network_stats = TRUE,
                      detect_clusters = FALSE, cluster_method = NULL, cluster_arglist = NULL,
                      cluster_assignment = TRUE,
                      seed = NULL, generate_ggplot = TRUE, plot_ggplot = TRUE,
                      plot_whichstat = NULL, plot_edgestat = NULL, color_palette = "RdBu", plot_responselabels = TRUE,
                      response_levels = NULL, plot_title = NULL, save_input = TRUE) {

  if(save_input==TRUE){
  ResIN_arglist <- list(df = df, node_vars = node_vars, cor_method = cor_method,
                        weights = weights, method_wCorr = method_wCorr, poly_ncor = poly_ncor, neg_offset = neg_offset,
                        ResIN_scores = ResIN_scores, remove_negative = remove_negative,
                        EBICglasso = EBICglasso, EBICglasso_arglist = EBICglasso_arglist,
                        remove_nonsignificant = remove_nonsignificant, sign_threshold = sign_threshold,
                        node_covars = node_covars, node_costats = node_costats,
                        network_stats = network_stats, detect_clusters = detect_clusters,
                        cluster_method = cluster_method, cluster_arglist = cluster_arglist,
                        cluster_assignment = cluster_assignment, seed = seed, generate_ggplot = generate_ggplot,
                        plot_ggplot = plot_ggplot, plot_whichstat = plot_whichstat, plot_edgestat = plot_edgestat,
                        color_palette = color_palette, plot_responselabels = plot_responselabels,
                        response_levels = response_levels, plot_title = plot_title, save_input = save_input)

  } else {
    ResIN_arglist <- "not stored"
  }

  if(!is.null(seed)){
  set.seed(seed)
    }

  ## Select response node_vars
  if(is.null(node_vars)) {
    df_nodes <- df
  } else {
    df_nodes <- dplyr::select(df, dplyr::all_of(node_vars))
  }

  ## Make the dummy frame
  df_nodes <- as.data.frame(apply(df_nodes, 2, factor))
  df_nodes[df_nodes == "NA"] <- NA ## Re-setting NA's
  df_dummies <- fastDummies::dummy_cols(df_nodes, ignore_na=TRUE,
                                        remove_selected_columns=TRUE)

  ## Obtaining the choice level vector for plotting purposes
  choices <- sub(".*_", "", colnames(df_dummies))

  if(!is.null(response_levels)) {
    choices <- factor(choices, levels = response_levels)
  }

  ## Generating correlation matrices
  if(remove_nonsignificant==FALSE){

  if(is.null(weights)) {
    if(cor_method == "auto") {
      res_in_cor <- qgraph::cor_auto(df_dummies, verbose = FALSE)
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
        res_in_cor[i, j]  <- wCorr::weightedCorr(temp[, 1], temp[, 2], weights=temp[, 3], method = method_wCorr)
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
                                 threshold = FALSE, verbose = FALSE)
    }
    res_in_cor <- do.call(qgraph::EBICglasso, c(list(S = as.matrix(res_in_cor)),
                                                EBICglasso_arglist))
  }
    }else{
  ## Remove non-significant edges
  if(remove_nonsignificant==TRUE){
    if(EBICglasso==TRUE){
      stop("Removal of non-significant edges based on p-values cannot be combined with EBIC-glasso regularization.")
    }
    if(!(is.null(weights))){
      stop("Removal of non-significant edges is not compatible with weighted correlation estimation.")
    }
    if(cor_method=="auto"){
      cor_method <- "pearson"
    }
    if(cor_method %in% c("pearson", "kendall", "spearman")) {
      res_corrtest <- psych::corr.test(df_dummies, method = cor_method, use = "pairwise.complete.obs", alpha = sign_threshold, adjust = "none")
      res_corrtest$r[res_corrtest$p>sign_threshold] <- 0
      res_in_cor <- res_corrtest$r
    }
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
    if(neg_offset>0 & neg_offset<1) {
      res_in_cor[res_in_cor<0] <- res_in_cor[res_in_cor<0]+neg_offset
    }
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
  ResIN_qgraph <- qgraph::qgraph(res_in_cor, DoNotPlot = TRUE, layout = "spring", labels = rownames(res_in_cor))

  ## Network statistics (common structuration and centralization metrics)
  if(network_stats==TRUE) {
    node_net_stats <- qgraph::centrality_auto(ResIN_qgraph, weighted = TRUE)
    structuration <- apply(node_net_stats$node.centrality, 2, FUN = mean)
    structuration[5] <- mean(node_net_stats$ShortestPathLengths)
    structuration[6] <- DirectedClustering::ClustF(res_in_cor)$GlobalCC
    names(structuration) <- c("average_betweenness", "average_closeness", "average_strength", "average_expected_influence", "average_path_length", "global_clustering")
    centralization <- apply(node_net_stats$node.centrality, 2, FUN = sd)
    names(centralization) <- c("betweenness_centralization", "closeness_centralization", "strength_centralization", "expected_influence_centralization")
  } else {
    structuration <- c("not estimated")
    centralization <- c("not estimated")
  }

  ## Force directed algorithm and principle component rotation
  if(remove_negative==FALSE) {
    graph_layout <- as.data.frame(prcomp(igraph::layout_nicely(ResIN_igraph))$x)
  } else {
    graph_layout <- as.data.frame(prcomp(igraph::layout_with_fr(ResIN_igraph))$x)
    rownames(graph_layout) <- rownames(res_in_cor)
    if(!is.null(left_anchor)) {
      if(graph_layout[left_anchor,][1]>0) {
      graph_layout[,1] <- -1*graph_layout[,1]
      }
    }
  }

  graph_layout$node_names <- colnames(res_in_cor)
  colnames(graph_layout) <- c("x", "y", "node_names")

  node_frame <- graph_layout
  node_frame$from <- node_frame$node_names

  ## Generating the edge-list data-frame
  g <- igraph::as_data_frame(ResIN_igraph)
  g$from.x <- node_frame$x[match(g$from, node_frame$node_names)]
  g$from.y <- node_frame$y[match(g$from, node_frame$node_names)]
  g$to.x <- node_frame$x[match(g$to, node_frame$node_names)]
  g$to.y <- node_frame$y[match(g$to, node_frame$node_names)]

  edgelist_frame <- dplyr::left_join(g, node_frame, by = "from")

  ### Adding edge-betweenness if desired
  if(network_stats==TRUE) {
    edgelist_frame$from_to <- paste(edgelist_frame$from, edgelist_frame$to, sep = "_")
    node_net_stats$edge.betweenness.centrality$from_to <- paste(node_net_stats$edge.betweenness.centrality$from,
                                    node_net_stats$edge.betweenness.centrality$to, sep = "_")

    node_net_stats$edge.betweenness.centrality$from <- NULL
    node_net_stats$edge.betweenness.centrality$to <- NULL

    edgelist_frame <- left_join(edgelist_frame, node_net_stats$edge.betweenness.centrality, by = "from_to")
  }

  ## Integrating node-level network stats only into node-frame
  if(network_stats==TRUE) {
    node_frame <- cbind(node_frame, node_net_stats$node.centrality)
  }

  ## Perform clustering analysis
  if(detect_clusters==TRUE) {
    if(is.null(cluster_method)) {
      cluster <- do.call(igraph::cluster_leading_eigen, c(list(graph = ResIN_igraph), cluster_arglist))
    } else {
      if(cluster_method=="cluster_leading_eigen") {
      }
      if(cluster_method=="cluster_fast_greedy") {
        cluster <- do.call(igraph::cluster_fast_greedy, c(list(graph = ResIN_igraph), cluster_arglist))
      }
      if(cluster_method=="cluster_spinglass") {
        cluster <- do.call(igraph::cluster_spinglass, c(list(graph = ResIN_igraph), cluster_arglist))
      }
      if(cluster_method=="cluster_edge_betweenness") {
        cluster <- do.call(igraph::cluster_edge_betweenness, c(list(graph = ResIN_igraph), cluster_arglist))
      }
      if(cluster_method=="cluster_louvain") {
        cluster <- do.call(igraph::cluster_louvain, c(list(graph = ResIN_igraph), cluster_arglist))
      }
      if(cluster_method=="cluster_leiden") {
        cluster <- do.call(igraph::cluster_leiden, c(list(graph = ResIN_igraph), cluster_arglist))
      }
      if(cluster_method=="cluster_walktrap") {
        cluster <- do.call(igraph::cluster_walktrap, c(list(graph = ResIN_igraph), cluster_arglist))
      }
    }

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

    node_frame <- dplyr::left_join(node_frame, outcome, by = "from")
  }

  ## Cluster assignment matrix and vectors
  if(detect_clusters==TRUE & cluster_assignment==TRUE){

    cluster_dummies <- as.matrix(df_dummies)

    for(i in 1:ncol(cluster_dummies)) {
      cluster_dummies[, i][cluster_dummies[, i]==1] <- node_frame$cluster[node_frame$node_names==node_frame$node_names[i]]
    }

    cluster_dummies[cluster_dummies==0] <- NA
    cluster_probs <- as.data.frame(matrix(NA, nrow(df_dummies), max(node_frame$cluster, na.rm = TRUE)))

    for(j in 1:max(node_frame$cluster, na.rm=TRUE)){
      for(i in 1:nrow(df_dummies)){
        cluster_probs[i, j] <- sum(cluster_dummies[i, ][cluster_dummies[i, ]==j]/j, na.rm = TRUE)/sum(!(is.na(cluster_dummies[i, ])))
      }
    }

    colnames(cluster_probs) <- paste0("cluster_", names(table(node_frame$cluster)))

    ### Maximum probability assignment
    temp_probs <- cluster_probs + rnorm(nrow(cluster_probs)*ncol(cluster_probs), 0, 0.001)
    temp_probs <- dplyr::mutate(temp_probs, max_ind = max.col(temp_probs))
    max_cluster <- temp_probs$max_ind
  } else {
    cluster_probs <- "not estimated"
    max_cluster <- "not estimated"
  }

  ## Calculating summary statistics based on co-variates
  if(!(is.null(node_covars)) & !(is.null(node_costats))) {
    if(length(node_covars) != length(node_covars)) {
      stop("Covariate selection and summary statistics vectors must be of equal length.")
    }

    covars_frame <- as.matrix(dplyr::select(df, all_of(node_covars)))
    cov_stats <- as.data.frame(matrix(NA, length(same_items), length(node_costats)))
    for(i in 1:length(same_items)) {
      for(j in 1:length(node_costats)) {
        cov_stats[i, j] <- do.call(node_costats[j], c(list(x = covars_frame[, j][df_dummies[, i] == 1], na.rm = TRUE)))
      }
    }
    colnames(cov_stats) <- paste(node_covars, node_costats, sep = "_")
    cov_stats$node_label <- colnames(res_in_cor)

    node_frame <- cbind(node_frame, cov_stats)
  }

  ## Add choices to node_frame
  node_frame$choices <- as.factor(choices)

  ## Scoring
  if(ResIN_scores==TRUE & remove_negative==TRUE){
    score_dummies_x <- as.matrix(df_dummies)
    score_dummies_y <- as.matrix(df_dummies)
    for(i in 1:ncol(score_dummies_x)){
      score_dummies_x[, node_frame$node_names[i]][score_dummies_x[, node_frame$node_names[i]]==1] <- node_frame$x[node_frame$node_names==node_frame$node_names[i]]
      score_dummies_y[, node_frame$node_names[i]][score_dummies_y[, node_frame$node_names[i]]==1] <- node_frame$y[node_frame$node_names==node_frame$node_names[i]]
    }
    score_dummies_x[score_dummies_x==0] <- NA
    score_dummies_y[score_dummies_y==0] <- NA

    scores_x <- apply(score_dummies_x, 1, FUN = function(x) {mean(x, na.rm=TRUE)})
    scores_y <- apply(score_dummies_y, 1, FUN = function(x) {mean(x, na.rm=TRUE)})

    scores <- as.data.frame(cbind(scores_x, scores_y))
  } else {
    scores <- "not estimated"
  }

  # ggplot visualization
  if(generate_ggplot==FALSE){
    ResIN_ggplot <- "not generated"
  } else {

  if(is.null(plot_title)){
    plot_title <- paste("ResIN plot")
  }

  ## generating base graph
  ResIN_ggplot <- ggplot2::ggplot()+
    ggplot2::coord_fixed(ratio=1, x = c(min(edgelist_frame$x-1.5), max(edgelist_frame$x+1.5)),
                y = c(min(edgelist_frame$y-1.5), max(edgelist_frame$y+1.5)))

  ### Adding edge-weights if desired
  if(is.null(plot_edgestat)){
  ResIN_ggplot <- ResIN_ggplot + ggplot2::geom_curve(ggplot2::aes(x = edgelist_frame$from.x, xend = edgelist_frame$to.x, y = edgelist_frame$from.y, yend = edgelist_frame$to.y), curvature = 0.2, color = "black", alpha = 0.25)
  }else{
    edgelist_frame <- edgelist_frame[order(edgelist_frame[, plot_edgestat], decreasing = FALSE), ]
    ResIN_ggplot <- ResIN_ggplot + ggplot2::geom_curve(ggplot2::aes(x = edgelist_frame$from.x, xend = edgelist_frame$to.x, y = edgelist_frame$from.y, yend = edgelist_frame$to.y, linewidth = edgelist_frame[, plot_edgestat]), curvature = 0.2, color = "black", alpha = 0.25)+
      ggplot2::scale_linewidth(range = c(0, 4))+
      ggplot2::labs(linewidth = paste(plot_edgestat))
  }

  if(plot_responselabels==FALSE){
  ResIN_ggplot <- ResIN_ggplot + ggplot2::geom_point(ggplot2::aes(x = node_frame$x, y = node_frame$y))
  }else{
  ResIN_ggplot <- ResIN_ggplot + ggplot2::geom_text(ggplot2::aes(x = node_frame$x, y = node_frame$y, label = node_frame$node_names), size = 3.8)
  }

  ResIN_ggplot <- ResIN_ggplot+
    ggplot2::ggtitle(plot_title)+
    ggplot2::theme_classic()+
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom", legend.box = "vertical",
          legend.text = ggplot2::element_blank(), plot.title = ggplot2::element_text(hjust = 0.5, size=15))

  ## function to remove layers
  remove_layer <- function(ggplot_obj, layer_index) {
    ggplot_obj$layers <- ggplot_obj$layers[-layer_index]
    return(ggplot_obj)
  }

  ## Different coloring options:
  if(!is.null(plot_whichstat)){

  ### Color by clusters:
  if(plot_whichstat=="cluster") {
    if(detect_clusters==FALSE) {
      stop("You must set detect_clusters to TRUE in order to visualize node clusters")
      }
    ResIN_ggplot <- remove_layer(ResIN_ggplot, c(2,3))
    if(plot_responselabels==FALSE){
      ResIN_ggplot <- ResIN_ggplot+
        ggplot2::geom_point(ggplot2::aes(x = node_frame$x, y = node_frame$y, fill = as.factor(node_frame$cluster)),
                            shape = 21, size = 3)}else{
      ResIN_ggplot <- ResIN_ggplot +
        shadowtext::geom_shadowtext(ggplot2::aes(x = node_frame$x, y = node_frame$y, label = node_frame$node_names,
                                      color = as.factor(node_frame$cluster)), size = 3.8, bg.r = 0.1)
      }
    ResIN_ggplot <- ResIN_ggplot + ggplot2::scale_colour_brewer(palette = color_palette) +
      ggplot2::scale_fill_brewer(palette = color_palette) +
      ggplot2::labs(color = "Cluster membership", fill = "Cluster membership")+
      ggplot2::theme(legend.text = ggplot2::element_text(size=10))
  }

  if(plot_whichstat=="choices"){
  ResIN_ggplot <- remove_layer(ResIN_ggplot, c(2,3))
  if(plot_responselabels==FALSE){
    ResIN_ggplot <- ResIN_ggplot+
    ggplot2::geom_point(ggplot2::aes(x = node_frame$x, y = node_frame$y, fill = node_frame$choices),
                        shape = 21, size = 3)}else{
    ResIN_ggplot <- ResIN_ggplot +
        shadowtext::geom_shadowtext(ggplot2::aes(x = node_frame$x, y = node_frame$y, label = node_frame$node_names,
                                      color = node_frame$choices), size = 3.8, bg.r = 0.1)
      }
  ResIN_ggplot <- ResIN_ggplot + ggplot2::scale_colour_brewer(palette = color_palette) +
    ggplot2::scale_fill_brewer(palette = color_palette) +
    ggplot2::labs(color = "Response choices", fill = "Response choices")+
    ggplot2::theme(legend.text = ggplot2::element_text(size=10))
  }

  ### Coloring by node-level centrality stats:
  if(plot_whichstat %in% c("Strength", "Betweenness", "Closeness", "ExpectedInfluence")){
    if(network_stats==FALSE) {
      stop("You must set network_stats to TRUE in order to visualize a particular, node-level centrality metric.")
    }

    node_frame <- node_frame[order(node_frame[, plot_whichstat], decreasing = FALSE), ]

    ResIN_ggplot <- remove_layer(ResIN_ggplot, c(2,3))
    if(plot_responselabels==FALSE){
      ResIN_ggplot <- ResIN_ggplot+
      ggplot2::geom_point(ggplot2::aes(x = node_frame$x, y = node_frame$y, fill = node_frame[, plot_whichstat]),
                          shape = 21, size = 3)}else{
      ResIN_ggplot <- ResIN_ggplot + ggplot2::geom_text(ggplot2::aes(x = node_frame$x, y = node_frame$y,
                                              label = node_frame$node_names, color = node_frame[, plot_whichstat]), size = 3.8)
        }
    ResIN_ggplot <- ResIN_ggplot + ggplot2::scale_colour_distiller(palette = color_palette, direction = 1) +
      ggplot2::scale_fill_distiller(palette = color_palette, direction = 1)+
      ggplot2::labs(color = plot_whichstat, fill = plot_whichstat)+
      ggplot2::theme(legend.text = ggplot2::element_text(size=10))
    }

    ### Coloring by co-stats:
    if(!(plot_whichstat %in% c("cluster", "choices", "Strength", "Betweenness", "Closeness", "ExpectedInfluence"))){
      if(is.null(node_covars) | is.null(node_costats)) {
        stop("You must select at least one node level covariate (node_covars) and specify at least one summary statistic (node_costats) to be able to visualize the latter.")
      }
      ResIN_ggplot <- remove_layer(ResIN_ggplot, c(2,3))
      if(plot_responselabels==FALSE){
      ResIN_ggplot <- ResIN_ggplot+
        ggplot2::geom_point(ggplot2::aes(x = node_frame$x, y = node_frame$y, fill = node_frame[, plot_whichstat]),
                            shape = 21, size = 3)}else{
      ResIN_ggplot <- ResIN_ggplot + ggplot2::geom_text(ggplot2::aes(x = node_frame$x, y = node_frame$y, label = node_frame$node_names,
                                                                       color = node_frame[, plot_whichstat]), size = 3.8)
      }
      ResIN_ggplot <- ResIN_ggplot + ggplot2::scale_colour_distiller(palette = color_palette, direction = 1) +
        ggplot2::scale_fill_distiller(palette = color_palette, direction = 1)+
        ggplot2::labs(color = plot_whichstat, fill = plot_whichstat)+
        ggplot2::theme(legend.text = ggplot2::element_text(size=10))
    }
   }
  }

  ## Plotting ggplot graph:
  if(plot_ggplot==TRUE) {
      print(ResIN_ggplot)
    }

  ## Final bit of housekeeping
  node_frame$from <- NULL
  edgelist_frame$x <- NULL
  edgelist_frame$y <- NULL
  edgelist_frame$node_names <- NULL

  # Exporting features:
  graph_stats <- list(structuration, centralization)
  aux_objects <- list(res_in_cor, same_items, df_dummies, cluster_probs, max_cluster, ResIN_arglist)
  names(aux_objects) <- c("adj_matrix", "same_items", "df_dummies", "cluster_probabilities", "max_clusterprob", "ResIN_arglist")
  output <- list(edgelist_frame, node_frame, ResIN_ggplot, scores, graph_stats, aux_objects)
  names(output) <- c("ResIN_edgelist", "ResIN_nodeframe", "ResIN_ggplot", "ResIN_scores", "graph_stats", "aux_objects")
  class(output) <- c("list", "ResIN")

  return(output)
}





