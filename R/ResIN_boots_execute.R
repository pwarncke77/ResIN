#' @title ResIN_boots_execute
#'
#' @description Execute prepared ResIN bootstrap analysis
#'
#' @param ResIN_bootsprepped A list of prepared ResIN objects for bootstrapping (outcome of the \code{ResIN_boots_prepare} function)
#' @param parallel Should the function be executed in parallel using the \code{foreach} package? Defaults to FALSE. If FALSE, function will execute sequentially using a simple for loop.
#' @param detect_cores Should the number of available CPU cores be automatically detected? Defaults to TRUE and is ignored when parallel is set to FALSE.
#' @param core_offset Optionally, specify a positive integer offset that is subtracted from the number of automatically detected cores. Defaults to 0L.
#' @param n_cores Manually specify the number of available CPU cores. Defaults to 2L and is ignored if detect_cores is set to TRUE or if parallel is set to FALSE.
#'
#' @return A list object containing n (bootstrapped) \code{ResIN} list objects.
#'
#' @examples
#' ## Load the 12-item simulated Likert-type ResIN toy dataset
#' data(lik_data)
#'
#' # Apply the ResIN function to toy Likert data:
#' ResIN_obj <- ResIN(lik_data, cor_method = "spearman", network_stats = TRUE, cluster = TRUE)
#'
#'\dontrun{
#' # Prepare for bootstrapping
#' prepped_boots <- ResIN_boots_prepare(ResIN_obj, n=5000, boots_type="permute")
#'
#' # Execute the prepared bootstrap list
#'
#' ResIN_boots_execute(prepped_boots, parallel = TRUE, detect_cores = TRUE)
#'}
#'
#' @export
#' @importFrom foreach "foreach" "%dopar%"
#' @importFrom parallelly "availableCores"
#' @importFrom parallel "makeCluster"
#' @importFrom doSNOW "registerDoSNOW"
#' @importFrom ggplot2 "ggplot" "geom_curve" "geom_point" "geom_text" "ggtitle" "scale_color_continuous" "scale_color_discrete" "aes" "element_blank" "element_text" "theme" "theme_classic" "coord_fixed"
#' @importFrom dplyr "select" "left_join" "all_of"
#' @importFrom stats "complete.cases" "cor" "sd" "prcomp" "cov" "princomp"
#' @importFrom fastDummies "dummy_cols"
#' @importFrom qgraph "qgraph" "cor_auto" "centrality_auto" "EBICglasso" "qgraph.layout.fruchtermanreingold"
#' @importFrom igraph "graph_from_adjacency_matrix" "cluster_leading_eigen" "layout_nicely" "layout_with_fr" "membership"
#' @importFrom wCorr "weightedCorr"
#' @importFrom Matrix "nearPD"
#' @importFrom DirectedClustering "ClustF"
#' @importFrom psych "corr.test"
#'

ResIN_boots_execute <- function(ResIN_bootsprepped, parallel = FALSE, detect_cores = TRUE, core_offset = 0L, n_cores = 2L) {
  if(class(ResIN_bootsprepped)[2] !=  "ResIN_bootsprepped"){
    stop("Please supply a 'ResIN_bootsprepped' type list object.")
  }

  if(parallel==TRUE){
    if(detect_cores==TRUE){
      n_cores <- parallelly::availableCores()
    }

  #### COPY OF MAIN FUNCTION
    ResIN <- function(df, node_vars = NULL, cor_method = "auto", weights = NULL,
                      method_wCorr = "Polychoric", poly_ncor = 2, ResIN_scores = TRUE,
                      remove_negative = TRUE,
                      EBICglasso = FALSE, EBICglasso_arglist = NULL,
                      remove_nonsignificant = FALSE, sign_threshold = 0.05,
                      node_covars = NULL, node_costats = NULL,
                      network_stats = FALSE,
                      detect_clusters = FALSE, cluster_method = NULL, cluster_arglist = NULL,
                      cluster_assignment = FALSE,
                      seed = 42, generate_ggplot = FALSE, plot_ggplot = TRUE,
                      plot_whichstat = NULL, plot_title = NULL, save_input = TRUE) {

      if(save_input==TRUE){
        ResIN_arglist <- list(df = df, node_vars = node_vars, cor_method = cor_method,
                              weights = weights, method_wCorr = method_wCorr, poly_ncor = poly_ncor,
                              ResIN_scores = ResIN_scores, remove_negative = remove_negative,
                              EBICglasso = EBICglasso, EBICglasso_arglist = EBICglasso_arglist,
                              remove_nonsignificant = remove_nonsignificant, sign_threshold = sign_threshold,
                              node_covars = node_covars, node_costats = node_costats,
                              network_stats = network_stats, detect_clusters = detect_clusters,
                              cluster_method = cluster_method, cluster_arglist = cluster_arglist,
                              cluster_assignment = cluster_assignment, seed = seed, generate_ggplot = generate_ggplot,
                              plot_ggplot = plot_ggplot, plot_whichstat = plot_whichstat, plot_title = plot_title, save_input = save_input)
      } else {
        ResIN_arglist <- "not stored"
      }


      set.seed(seed)

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
        covars_frame <- dplyr::select(df, all_of(node_covars))
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
        node_frame <- cbind(node_frame, node_net_stats$node.centrality)
      }

      if(!(is.null(node_covars)) & !(is.null(node_costats))) {
        node_frame <- cbind(node_frame, cov_stats)
      }

      ## Scoring ;)
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

      ## Perform clustering analysis
      if(detect_clusters==TRUE) {
        if(is.null(cluster_method)) {
          cluster <- do.call(igraph::cluster_leading_eigen, c(list(graph = ResIN_igraph), cluster_arglist))
        } else {
          if(cluster_method=="cluster_leading_eigen") {
            cluster <- do.call(igraph::cluster_leading_eigen, c(list(graph = ResIN_igraph), cluster_arglist))
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
      if(detect_clusters==TRUE & detect_clusters==TRUE){

        cluster_dummies <- as.matrix(df_dummies)

        for(i in 1:ncol(cluster_dummies)) {
          cluster_dummies[, i][cluster_dummies[, i]==1] <- node_frame$cluster[node_frame$node_names==node_frame$node_names[i]]
        }

        cluster_dummies[cluster_dummies==0] <- NA
        cluster_probs <- as.data.frame(matrix(NA, nrow(df_dummies), max(node_frame$cluster)))

        for(j in 1:max(node_frame$cluster)){
          for(i in 1:nrow(df_dummies)){
            cluster_probs[i, j] <- sum(cluster_dummies[i, ][cluster_dummies[i, ]==j]/j, na.rm = TRUE)/sum(!(is.na(cluster_dummies[i, ])))
          }
        }

        colnames(cluster_probs) <- paste0("cluster_", names(table(node_frame$cluster)))

        ### Maximum probability assignment
        temp_probs <- cluster_probs + rnorm(nrow(cluster_probs)*ncol(cluster_probs), 0, 0.001)
        temp_probs <- mutate(temp_probs, max_ind = max.col(temp_probs))
        max_cluster <- temp_probs$max_ind
      } else {
        cluster_probs <- "not estimated"
        max_cluster <- "not estimated"
      }

      ## Preparing plotting data for ggplot graph format
      g <- igraph::as_data_frame(ResIN_igraph)
      g$from.x <- node_frame$x[match(g$from, node_frame$node_names)]
      g$from.y <- node_frame$y[match(g$from, node_frame$node_names)]
      g$to.x <- node_frame$x[match(g$to, node_frame$node_names)]
      g$to.y <- node_frame$y[match(g$to, node_frame$node_names)]

      edgelist_frame <- dplyr::left_join(g, node_frame, by = "from")

      # ggplot visualization
      generate_ggplot = FALSE
      if(generate_ggplot==FALSE){
        ResIN_ggplot <- "no ggplot of ResIN network generated"
      } else {

        if(is.null(plot_title)){
          plot_title <- paste("ResIN plot")
        }

        ## generating base graph
        ResIN_ggplot <- ggplot2::ggplot()+
          ggplot2::coord_fixed(ratio=1, x = c(min(edgelist_frame$x-1.5), max(edgelist_frame$x+1.5)),
                               y = c(min(edgelist_frame$y-1.5), max(edgelist_frame$y+1.5)))+
          ggplot2::geom_curve(ggplot2::aes(x = edgelist_frame$from.x, xend = edgelist_frame$to.x, y = edgelist_frame$from.y, yend = edgelist_frame$to.y), curvature = 0.3, color = "darkgrey", alpha = 0.5)+
          ggplot2::geom_point(ggplot2::aes(x = node_frame$x, y = node_frame$y), size = 1)+
          ggplot2::geom_text(ggplot2::aes(x = node_frame$x, y = node_frame$y, label = node_frame$node_names), size = 4.5)+
          ggplot2::ggtitle(plot_title)+
          ggplot2::theme_classic()+
          ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom",
                         legend.text = ggplot2::element_blank(), plot.title = ggplot2::element_text(hjust = 0.5, size=16))

        ## function to remove layers
        remove_layer <- function(ggplot_obj, layer_index) {
          ggplot_obj$layers <- ggplot_obj$layers[-layer_index]
          return(ggplot_obj)
        }

        ### Coloring by clusters:
        if(plot_whichstat=="cluster") {
          if(detect_clusters==FALSE) {
            stop("You must set detect_clusters to TRUE in order to visualize node clusters")
          }
          ResIN_ggplot <- remove_layer(ResIN_ggplot, c(2,3))
          ResIN_ggplot <- ResIN_ggplot+
            ggplot2::geom_point(ggplot2::aes(x = node_frame$x, y = node_frame$y, color = as.factor(node_frame$cluster)), size = 1)+
            ggplot2::geom_text(ggplot2::aes(x = node_frame$x, y = node_frame$y, label = node_frame$node_names,
                                            color = as.factor(node_frame$cluster)), size = 4.5)+
            ggplot2::scale_color_discrete(name = "Network communities")
        }

        ### Coloring by co-stats:
        if(!(is.null(plot_whichstat)) & !(plot_whichstat %in% c("cluster", "Strength", "Betweenness", "Closeness", "ExpectedInfluence"))){
          if(is.null(node_covars) | is.null(node_costats)) {
            stop("You must select at least one node level covariate (node_covars) and specify at least one summary statistic (node_costats) to be able to visualize the latter.")
          }
          ResIN_ggplot <- remove_layer(ResIN_ggplot, c(2,3))
          ResIN_ggplot <- ResIN_ggplot+
            ggplot2::geom_point(ggplot2::aes(x = node_frame$x, y = node_frame$y, color = node_frame[, plot_whichstat]), size = 1)+
            ggplot2::geom_text(ggplot2::aes(x = node_frame$x, y = node_frame$y, label = node_frame$node_names,
                                            color = node_frame[, plot_whichstat]), size = 4.5)+
            ggplot2::scale_color_continuous(name = plot_whichstat)
        }

        ### Coloring by node-level centrality stats:
        if(plot_whichstat %in% c("Strength", "Betweenness", "Closeness", "ExpectedInfluence")){
          if(network_stats==FALSE) {
            stop("You must set network_stats to TRUE in order to visualize a particular, node-level centrality metric.")
          }
          ResIN_ggplot <- remove_layer(ResIN_ggplot, c(2,3))
          ResIN_ggplot <- ResIN_ggplot+
            ggplot2::geom_point(ggplot2::aes(x = node_frame$x, y = node_frame$y, color = node_frame[, plot_whichstat]), size = 1)+
            ggplot2::geom_text(ggplot2::aes(x = node_frame$x, y = node_frame$y, label = node_frame$node_names,
                                            color = node_frame[, plot_whichstat]), size = 4.5)+
            scale_color_continuous(name = plot_whichstat)
        }

      }

      # Exporting features:
      graph_stats <- list(structuration, centralization)
      aux_objects <- list(res_in_cor, same_items, df_dummies, cluster_probs, max_cluster, ResIN_arglist)
      names(aux_objects) <- c("adj_matrix", "same_items", "df_dummies", "cluster_probabilities", "max_clusterprob", "ResIN_arglist")
      output <- list(edgelist_frame, node_frame, ResIN_ggplot, scores, graph_stats, aux_objects)
      names(output) <- c("ResIN_edgelist", "ResIN_nodeframe", "ResIN_ggplot", "ResIN_scores", "graph_stats", "aux_objects")
      class(output) <- c("list", "ResIN")

      return(output)
    }
  #### END OF COPY

   cl <- parallel::makeCluster(n_cores[1])
   doSNOW::registerDoSNOW(cl)
   boots_list <- foreach::foreach(i=1:length(ResIN_bootsprepped), .inorder = FALSE) %dopar% {
     do.call(ResIN, ResIN_bootsprepped[[i]])
   }

   parallel::stopCluster(cl)

  } else {
   boots_list <- vector("list", length(ResIN_bootsprepped))
   for(i in 1:length(boots_list)){
     boots_list[[i]] <- do.call(ResIN, ResIN_bootsprepped[[i]])
   }
  }

  return(boots_list)
}


