#' @title Flagship function that implements Response Item Network (ResIN) analysis
#'
#' @description Performs Response Item-Network (ResIN) analysis in one go. Users minimally need to supply a dataframe or matrix of discrete response data. If needed for step-wise analysis, all intermediate outputs can still be accessed as part of the aux_objects output list.
#'
#' @param df A data-frame object containing the raw data.
#' @param node_vars An optional character vector detailing the attitude item columns to be selected for ResIN analysis (i.e. the subset of attitude variables in df).
#' @param left_anchor An optional character scalar indicating a particular response node which determines the spatial orientation of the ResIN latent space. If this response node does not appear on the left-hand side, the x-plane will be inverted. This ensures consistent interpretation of the latent space across multiple iterations (e.g. in bootstrapping analysis). Defaults to NULL (no adjustment to orientation is taken.)
#' @param cor_method Which correlation method should be used? Current implementation supports "pearson" (default) and "polychoric". Please note that polychoric correlations are currently unsupported for weighted analysis.
#' @param missing_cor Character scalar controlling missing-data handling for correlation estimation. Either \code{"pairwise"} (default) or  \code{"listwise"}.
#' @param weights Optional survey weights. Can be either \code{NULL} (default), a numeric vector of length \code{nrow(df)}, or a character scalar naming a weights column in \code{df}. If a column name is supplied and \code{node_vars = NULL}, the weights column is automatically excluded from the response-node variables used for ResIN estimation.
#' @param offset Optional off-set to correlation edges to manually adjust for over- or under-fitting the network. Defaults to \code{0}. Supplying a value between -1 and 0 globally reduces edge values by that amount, leading to the elimination of all positive  edges below that value, resulting in a more sparse network. (However, we strongly recommend setting \code{remove_nonsignificant=TRUE} instead for a more principled approach to ensuring optimal network sparsity as global thresholds have heuristic value at best). Alternatively, a value between 0 and 1 enforces a positive offset, resulting in more dense (but potentially over-fitted) networks.
#' @param ResIN_scores Logical; should spatial scores be calculated for every individual. Defaults to TRUE. Function obtains the mean positional score on the major (x-axis) and minor (y-axis). Current package implementation also provides empirical Bayesian scores via James-Stein shrinkage (\code{eb_x}) and heuristic shrinkage (\code{heur_x}) scores. Please refer to the package [vignette]https://pwarncke77.github.io/ResIN/articles/ResIN-VIGNETTE.html#spatial-interpretation-and-individual-latent-space-scores for further details.
#' @param remove_nonsignificant Logical; should non-significant edges be removed from the ResIN network? Defaults to FALSE. For weighted Pearson correlations, p-values are approximated using a weighted effective sample size. For currently unsupported polychoric configurations, ResIN falls back to Pearson and issues a warning.
#' @param remove_nonsignificant_method Character scalar specifying how p-values are thresholded when \code{remove_nonsignificant = TRUE}. Defaults to \code{"default"}, which prunes edges with raw p-values greater than \code{sign_threshold} (i.e., retains edges with \code{p <= sign_threshold}). If set to \code{"bh"}, p-values are adjusted using the Benjamini--Hochberg procedure; edges are retained only if the adjusted p-value is less than or equal to \code{sign_threshold}, interpreted as the target false discovery rate \eqn{q}. This provides multiplicity control across all tested edges and is typically more principled than using unadjusted p-values, but may be slightly slower. See \code{\link[stats]{p.adjust}} for details.
#' @param sign_threshold Numeric scalar controlling the pruning threshold used when \code{remove_nonsignificant = TRUE}. For \code{remove_nonsignificant_method = "default"}, this is the raw p-value cutoff (e.g., \code{0.05}). For \code{remove_nonsignificant_method = "bh"}, this is the target false discovery rate level \eqn{q} (e.g., \code{0.05}), applied to Benjamini--Hochberg adjusted p-values.
#' @param node_covars An optional character string selecting quantitative co-variates that can be used to enhance ResIN analysis. Typically, these covariates provide grouped summary statistics for item response nodes. (E.g.: What is the average age or income level of respondents who selected a particular item response?) Variable names specified here should match existing columns in \code{df}.
#' @param node_costats If any \code{node_covars} are selected, what summary statistics should be estimated from them? Argument should be a character vector and call a base-R function. (E.g. \code{"mean"}, \code{"median"}, \code{"sd"}). Each element specified in \code{node_costats} is applied to each element in \code{node_covars} and the out-put is stored as a node-level summary statistic in the \code{ResIN_nodeframe}. The extra columns in \code{ResIN_nodeframe} are labeled according to the following template: "covariate name"_"statistic". So for the respondents mean age, the corresponding column in \code{ResIN_nodeframe} would be labeled as "age_mean".
#' @param network_stats Should common node- and graph level network statistics be extracted? Calls \code{qgraph::centrality_auto} and \code{DirectedClustering::ClustF} to the ResIN graph object to extract node-level betweenness, closeness, strength centrality, as well as the mean and standard deviation of these scores at the network level. Also estimates network expected influence, average path length, and global clustering coefficients. Defaults to TRUE. Set to FALSE if estimation takes a long time.
#' @param detect_clusters Optional, should community detection be performed on item response network? Defaults to FALSE. If set to TRUE, performs a clustering method from the [igraph](https://igraph.org/r/doc/cluster_leading_eigen.html) library and stores the results in the \code{ResIN_nodeframe} output.
#' @param cluster_method A character scalar specifying the [igraph-based](https://igraph.org/r/doc/communities.html) community detection function. Current ResIN (v. 2.3.1) implementation \code{"cluster_leading_eigen"}, \code{"cluster_fast_greedy"}, \code{"cluster_spinglass"}, \code{"cluster_edge_betweenness"}, \code{"cluster_louvain"}, \code{"cluster_walktrap"}, \code{"cluster_infomap"}, \code{"cluster_optimal"} and \code{"cluster_fluid_communities"}. Note that \code{"cluster_fluid_communities"} additionally requires a pre-supplied \code{"no.of.communities"} argument which can be supplied via the \code{"cluster_arglist"} argument (defaults to 2). Please consult the [igraph](https://igraph.org/r/doc/cluster_leading_eigen.html) community detection algorithm library for more information on each algorithm and their requirements.
#' @param cluster_arglist An optional list specifying additional arguments to the selected [igraph](https://igraph.org/r/doc/communities.html) clustering method.
#' @param cluster_assignment Should individual (survey) respondents be assigned to different clusters? If set to TRUE, function will generate an n*c matrix of probabilities for each respondent to be assigned to one of c clusters. Furthermore, a vector of length n is generated displaying the most likely cluster respondents belong to. In case of a tie between one or more clusters, a very small amount of random noise determines assignment. Both matrix and vectors are added to the \code{aux_objects} list. Defaults to FALSE and will be ignored if \code{detect_clusters} is set to FALSE.
#' @param generate_ggplot Logical; should a ggplot-based visualization of the ResIN network be generated? Defaults to TRUE.
#' @param plot_ggplot Logical; should the ggplot of the ResIN network be plotted? Defaults to TRUE. If set to FALSE, the ggplot object will not be directly returned to the console. (However, if generate_ggplot=TRUE, the plot will still be generated and stored alongside the other output objects.)
#' @param plot_whichstat Should a particular node-level metric be color-visualized in the ggplot output? For node cluster, specify "cluster". For the same Likert response choices or options, specify "choices". For a particular node-level co-variate please specify the name of the particular element in \code{node_covars} followed by a "_" and the specific \code{node_costats} you would like to visualize. For instance if you want the visualize average age at the node-level, you should specify "age_mean". To colorize by node centrality statistics, possible choices are "Strength", "Betweenness", "Closeness", and "ExpectedInfluence". Defaults to NULL. Make sure to supply appropriate choices to \code{node_covars}, \code{node_costats}, \code{detect_clusters}, and/or \code{network_stats} prior to setting this argument.
#' @param plot_edgestat Should the thickness of the edges be adjusted according to a particular co-statistic? Defaults to NULL. Possible choices are "weight" for the bi-variate correlation strength, and "edgebetweenness"
#' @param color_palette Optionally, you may specify the ggplot2 color palette to be applied to the plot. All options contained in [\code{RColorBrewer}](https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf) (for discrete colors such as cluster assignments) and [\code{ggplot2::scale_colour_distiller}](https://ggplot2.tidyverse.org/reference/scale_brewer.html) are supported. Defaults to "RdBu".
#' @param direction Which direction should the color palette be applied in? Defaults to 1. Set to -1 if the palette should appear in reverse order.
#' @param plot_responselabels Should response labels be plotted via \code{geom_text}? Defaults to TRUE. It is recommended to set to FALSE if the network possesses a lot of nodes and/or long response choice names.
#' @param response_levels An optional character vector specifying the correct order of global response levels. Only useful if all node-items follow the same convention (e.g. ranging from "strong disagreement" to "strong agreement"). The supplied vector should have the same length as the total number of response options and supply these (matching exactly) in the correct order. E.g. c("Strongly Agree", "Somewhat Agree", "Neutral", "Somewhat Disagree", "Strongly Disagree"). Defaults to NULL.
#' @param plot_title Optionally, a character scalar specifying the title of the ggplot output. Defaults to "ResIN plot".
#' @param multimodal Logical; should a multimodal graph which jointly incorporates respondents/ data rows and response choices be produced in addition to classic ResIN graph? Defaults to FALSE. If set to TRUE, an  [igraph](https://igraph.org/r/doc/) multimodal graph with response options as node type 1 and participants as node type 2 will be generated and included in the output list. Further, an object called \code{coordinate_df} with spatial coordinates of respondents and a plot-able \code{ggraph}-object called \code{multimodal_ggraph} are generated if set to TRUE.
#' @param multimodal_edge_overlay Character scalar controlling which edges are drawn in the multimodal plot when \code{multimodal = TRUE}. One of \code{"none"} (no edges), \code{"ResIN"} (only the original ResIN edges among response nodes, styled via \code{plot_edgestat} when supplied), \code{"multimodal"} (only participant--response edges), or \code{"both"} (ResIN edges as a base layer plus multimodal edges on top).
#' @param remove_negative Logical; should all negative correlations be removed? Defaults to TRUE (highly recommended). Setting to FALSE makes it impossible to estimate a force-directed network layout. Function will use igraph::layout_nicely instead.
#' @param save_input Logical; should input data and function arguments be saved (this is necessary for running ResIN_boots_prepare function). Defaults to TRUE.
#' @param seed Random seed for force-directed algorithm. Defaults to NULL (no seed is set.) If scalar integer is supplied, that seed will be set prior to analysis.
#' @param EBICglasso Retired as of ResIN 2.3.0 and ignored.
#' @param EBICglasso_arglist Retired as of ResIN 2.3.0 and ignored.
#' @param bipartite Retired as of ResIN 2.3.1 and ignored. Please set \code{multimodal} argument instead.
#'
#' @return An edge-list type data-frame, \code{ResIN_edgelist}, a node-level data-frame, \code{ResIN_nodeframe}, an n*2 data-frame of individual-level spatial scores along the major (x) and minor(y) axis, \code{ResIN_scores} a list of graph-level statistics \code{graph_stats} including (\code{graph_structuration}), and centralization (\code{graph_centralization}). Further, a \code{multimodal_output} list which includes an \code{igraph} class multimodal graph (\code{multimodal_igraph}), a data frame, \code{coordinate_df}, with spatial coordinates of respondents, and a plot-able \code{ggraph}-object called \code{multimodal_ggraph} is optionally generated. Lastly, the output includes a list of auxiliary objects, \code{aux_objects}, including the ResIN adjacency matrix (\code{adj_matrix}), an alternate version of the adjacency matrix with all negative edges retained, a numeric vector detailing which item responses belong to which item (\code{same_items}), and the dummy-coded item-response data-frame (\code{df_dummies}). For reproducibility, (\code{aux_objects$meta} stores a numeric dataframe identifier (\code{df_id}, the random seed, call, and the (\code{ResIN} package version used to create the object.”
#'
#' @examples
#'
#' ## Load the 12-item simulated Likert-type toy dataset
#' data(lik_data)
#'
#' # Apply the ResIN function to toy Likert data:
#' ResIN_obj <- ResIN(lik_data, network_stats = TRUE, remove_nonsignificant = TRUE)
#'
#' @export
#' @importFrom ggplot2 "ggplot" "geom_curve" "geom_point" "geom_text" "ggtitle" "scale_color_continuous" "scale_color_discrete" "scale_colour_manual" "aes" "element_blank" "element_text" "theme" "theme_classic" "theme_void" "coord_fixed"
#' @importFrom dplyr "select" "left_join" "all_of" "mutate" "filter" "%>%" "row_number"
#' @importFrom tidyr "pivot_longer"
#' @importFrom stats "complete.cases" "cor" "sd" "prcomp" "cov" "princomp" "pt"
#' @importFrom fastDummies "dummy_cols"
#' @importFrom utils "packageVersion" "getFromNamespace" "capture.output"
#' @importFrom qgraph "qgraph" "centrality_auto" "qgraph.layout.fruchtermanreingold"
#' @importFrom igraph "graph_from_adjacency_matrix" "graph_from_data_frame" "V" "vcount" "layout_nicely" "cluster_leading_eigen" "cluster_fast_greedy" "cluster_spinglass" "cluster_edge_betweenness" "cluster_louvain" "cluster_walktrap" "cluster_infomap" "cluster_optimal" "cluster_fluid_communities" "layout_with_fr" "membership"
#' @importFrom DirectedClustering "ClustF"
#' @importFrom psych "corr.test" "tetrachoric"
#' @importFrom shadowtext "geom_shadowtext"
#' @importFrom ggraph "ggraph" "geom_edge_link" "geom_node_point"
#'

ResIN <- function(
    df,
    node_vars = NULL,
    left_anchor = NULL,
    cor_method = "pearson",
    weights = NULL,
    missing_cor = "pairwise",
    offset = 0,
    ResIN_scores = TRUE,
    remove_nonsignificant = FALSE,
    remove_nonsignificant_method = "default",
    sign_threshold = 0.05,
    node_covars = NULL,
    node_costats = NULL,
    network_stats = TRUE,
    detect_clusters = FALSE,
    cluster_method = NULL,
    cluster_arglist = NULL,
    cluster_assignment = TRUE,
    generate_ggplot = TRUE,
    plot_ggplot = TRUE,
    plot_whichstat = NULL,
    plot_edgestat = NULL,
    color_palette = "RdBu",
    direction = 1,
    plot_responselabels = TRUE,
    response_levels = NULL,
    plot_title = NULL,
    multimodal = FALSE,
    multimodal_edge_overlay = c("multimodal", "none", "ResIN", "both"),
    save_input = TRUE,
    remove_negative = TRUE,
    EBICglasso = FALSE,
    EBICglasso_arglist = NULL,
    bipartite = FALSE,
    seed = NULL
) {

  # Reproducibility housekeeping
  ## Storing input parameters
  if (save_input == TRUE) {
    ResIN_arglist <- list(
      df = df,
      node_vars = node_vars,
      left_anchor = left_anchor,
      cor_method = cor_method,
      weights = weights,
      missing_cor = missing_cor,
      offset = offset,
      ResIN_scores = ResIN_scores,
      remove_negative = remove_negative,
      EBICglasso = EBICglasso,
      EBICglasso_arglist = EBICglasso_arglist,
      bipartite = bipartite,
      remove_nonsignificant = remove_nonsignificant,
      sign_threshold = sign_threshold,
      node_covars = node_covars,
      node_costats = node_costats,
      network_stats = network_stats,
      detect_clusters = detect_clusters,
      cluster_method = cluster_method,
      cluster_arglist = cluster_arglist,
      cluster_assignment = cluster_assignment,
      generate_ggplot = generate_ggplot,
      plot_ggplot = plot_ggplot,
      plot_whichstat = plot_whichstat,
      plot_edgestat = plot_edgestat,
      color_palette = color_palette,
      direction = direction,
      plot_responselabels = plot_responselabels,
      response_levels = response_levels,
      plot_title = plot_title,
      multimodal = multimodal,
      multimodal_edge_overlay = multimodal_edge_overlay,
      save_input = save_input,
      seed = seed
    )
  } else {
    ResIN_arglist <- "not stored"
  }

  if(!is.null(seed)){
    set.seed(seed)
  }

  ## Versioning
  resin_version <- as.character(utils::packageVersion("ResIN"))
  r_version     <- paste0(R.version$major, ".", R.version$minor)

  ## Adding stable input identifier
  tmp <- tempfile("ResIN_df_", fileext = ".rds")
  saveRDS(df, tmp, compress = FALSE)
  df_id <- unname(tools::md5sum(tmp))
  unlink(tmp)

  created <- Sys.time()

  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(seed)
  }

  ## Retired and removed arguments: EBICglasso & bipartite
  if (!missing(EBICglasso) && isTRUE(EBICglasso)) {
    .Deprecated(
      msg = paste0(
        "EBICglasso is retired as of ResIN 2.3.0 and has no effect."
      )
    )
  }

  if (!missing(EBICglasso_arglist) && !is.null(EBICglasso_arglist)) {
    .Deprecated(
      msg = "EBICglasso_arglist is retired as of ResIN 2.3.0 and has no effect."
    )
  }

  invisible(EBICglasso)
  invisible(EBICglasso_arglist)

  if (!missing(bipartite) && isTRUE(bipartite)) {
    .Deprecated(
      msg = "Setting the 'bipartite' argument is retired as of ResIN 2.3.1 and has no effect.
      Please use the 'multimodal' argument instead."
    )
  }

  invisible(bipartite)

  # Actual begin of ResIN algorithm:
  weights_name <- NULL
  weights_from_df <- FALSE

  if (!is.null(weights)) {

    # Case 1: weights supplied as column name in df
    if (is.character(weights)) {
      if (length(weights) != 1L || is.na(weights) || !nzchar(weights)) {
        stop("If supplied as character, weights must be a single non-empty column name in df.",
             call. = FALSE)
      }

      weights_name <- weights

      if (!weights_name %in% colnames(df)) {
        stop("weights='", weights_name, "' was not found in colnames(df).", call. = FALSE)
      }

      weights <- df[[weights_name]]
      weights_from_df <- TRUE

      # If node_vars is not explicitly supplied, automatically exclude the weights column
      if (is.null(node_vars)) {
        node_vars <- setdiff(colnames(df), weights_name)
      } else {
        # If user explicitly included the weights column among node_vars, remove it
        if (weights_name %in% node_vars) {
          node_vars <- setdiff(node_vars, weights_name)
          warning(
            "The weights column ('", weights_name,
            "') was removed from node_vars so it is not treated as a ResIN node.",
            call. = FALSE
          )
        }
      }
    }

    # Case 2: weights supplied as external numeric vector
    if (!is.character(weights)) {
      if (!is.numeric(weights) || length(weights) != nrow(df)) {
        stop("weights must be NULL, a numeric vector of length nrow(df), or a single character naming a weights column in df.",
             call. = FALSE)
      }
    }

    # Common validation (applies after resolving character -> numeric vector)
    if (!is.numeric(weights)) {
      stop("Resolved weights must be numeric.", call. = FALSE)
    }
    if (any(!is.finite(weights))) {
      stop("weights must contain only finite values.", call. = FALSE)
    }
    if (any(weights < 0)) {
      stop("weights must be non-negative.", call. = FALSE)
    }
    if (all(weights == 0)) {
      stop("weights cannot be all zero.", call. = FALSE)
    }
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


  ## ResIN dummy frame correlations depending on user input:

  # validate correlation arguments
  if (!cor_method %in% c("pearson", "polychoric")) {
    stop("cor_method must be either 'pearson' or 'polychoric'.", call. = FALSE)
  }
  if (!missing_cor %in% c("pairwise", "listwise")) {
    stop("missing_cor must be either 'pairwise' or 'listwise'.", call. = FALSE)
  }

  # declare item id per dummy column
  item_id_per_dummycol <- rep(NA_integer_, ncol(df_dummies))
  j <- 1L
  for (i in seq_len(ncol(df_nodes))) {
    k <- length(levels(factor(df_nodes[, i])))
    idx <- j:(j + k - 1L)
    item_id_per_dummycol[idx] <- i
    j <- j + k
  }

  # split indices by original item
  item_blocks <- split(seq_along(item_id_per_dummycol), item_id_per_dummycol)

  # helper: weighted Pearson correlation for one pair + approximate p-value
  weighted_cor_test_pair <- function(x, y, w, need_p = FALSE) {
    ok <- is.finite(x) & is.finite(y) & is.finite(w) & (w > 0)
    x <- x[ok]; y <- y[ok]; w <- w[ok]

    n_pair <- length(x)
    if (n_pair < 2L) {
      return(list(r = NA_real_, p = NA_real_, n_pair = n_pair, n_eff = NA_real_))
    }

    sw <- sum(w)
    if (!is.finite(sw) || sw <= 0) {
      return(list(r = NA_real_, p = NA_real_, n_pair = n_pair, n_eff = NA_real_))
    }

    # Correlation is scale-invariant in weights; normalized weights for effective n
    wn <- w / sw

    mx <- sum(wn * x)
    my <- sum(wn * y)

    vx <- sum(wn * (x - mx)^2)
    vy <- sum(wn * (y - my)^2)

    if (!is.finite(vx) || !is.finite(vy) || vx <= 0 || vy <= 0) {
      return(list(r = NA_real_, p = NA_real_, n_pair = n_pair, n_eff = NA_real_))
    }

    cov_xy <- sum(wn * (x - mx) * (y - my))
    r <- cov_xy / sqrt(vx * vy)

    # numerical clamp
    r <- max(min(r, 1), -1)

    p <- NA_real_
    n_eff <- NA_real_

    if (isTRUE(need_p)) {

      # Effective sample size for weighted tests
      denom <- sum(wn^2)
      if (is.finite(denom) && denom > 0) {
        n_eff <- 1 / denom
      }

      if (is.finite(n_eff) && n_eff > 2 && is.finite(r)) {
        if (abs(r) < 1) {
          tval <- r * sqrt((n_eff - 2) / (1 - r^2))
          p <- 2 * stats::pt(-abs(tval), df = n_eff - 2)
        } else {
          p <- 0
        }
      }
    }

    list(r = r, p = p, n_pair = n_pair, n_eff = n_eff)
  }

  # helper: weighted Pearson matrix (upper-triangle only + skip same-item pairs)
  weighted_pearson_matrix <- function(X, w, missing = c("pairwise", "listwise"),
                                      need_p = FALSE, item_id = NULL) {
    missing <- match.arg(missing)
    X <- as.data.frame(X)

    p <- ncol(X)
    R <- matrix(0, p, p)
    diag(R) <- 1
    colnames(R) <- colnames(X)
    rownames(R) <- colnames(X)

    P <- NULL
    N_pair <- NULL
    N_eff <- NULL
    if (isTRUE(need_p)) {
      P <- matrix(NA_real_, p, p)
      diag(P) <- 0
      colnames(P) <- colnames(X); rownames(P) <- colnames(X)

      N_pair <- matrix(NA_real_, p, p)
      N_eff  <- matrix(NA_real_, p, p)
      diag(N_pair) <- nrow(X)
      diag(N_eff)  <- nrow(X)
      colnames(N_pair) <- rownames(N_pair) <- colnames(X)
      colnames(N_eff)  <- rownames(N_eff)  <- colnames(X)
    }

    # listwise pre-filter once if requested
    if (missing == "listwise") {
      cc <- stats::complete.cases(X) & is.finite(w)
      X <- X[cc, , drop = FALSE]
      w <- w[cc]
    }

    for (i in seq_len(p)) {
      xi <- X[[i]]

      for (j in i:p) {
        # skip same-item correlations
        if (!is.null(item_id) && item_id[i] == item_id[j]) {
          if (isTRUE(need_p)) {
            P[i, j] <- P[j, i] <- 0
          }
          if (i == j) R[i, j] <- 1
          next
        }

        if (i == j) {
          R[i, j] <- 1
          if (isTRUE(need_p)) {
            P[i, j] <- 0
            N_pair[i, j] <- if (missing == "listwise") nrow(X) else sum(is.finite(xi) & is.finite(w) & w > 0)
            ww <- w[is.finite(xi) & is.finite(w) & w > 0]
            if (length(ww) > 0) {
              wwn <- ww / sum(ww)
              N_eff[i, j] <- 1 / sum(wwn^2)
            }
          }
          next
        }

        out <- weighted_cor_test_pair(xi, X[[j]], w, need_p = need_p)

        R[i, j] <- R[j, i] <- out$r

        if (isTRUE(need_p)) {
          P[i, j] <- P[j, i] <- out$p
          N_pair[i, j] <- N_pair[j, i] <- out$n_pair
          N_eff[i, j]  <- N_eff[j, i]  <- out$n_eff
        }
      }
    }

    list(r = R, p = P, n_pair = N_pair, n_eff = N_eff)
  }

  # Helper: apply significance threshold pruning
  apply_p_threshold <- function(R, P, alpha, method = remove_nonsignificant_method) {
    if (is.null(P)) return(R)

    method <- match.arg(method)

    # work on off-diagonal p-values only
    off <- upper.tri(P) | lower.tri(P)
    pvec <- P[off]

    # treat NA/non-finite p-values as "not significant"
    bad <- !is.finite(pvec)
    pvec2 <- pvec
    pvec2[bad] <- 1

    if (method == "bh") {
      pvec2 <- stats::p.adjust(pvec2, method = "BH")
    }

    keep_vec <- pvec2 <= alpha

    # Build keep matrix
    keep <- matrix(FALSE, nrow(R), ncol(R))
    keep[off] <- keep_vec
    diag(keep) <- TRUE

    R2 <- R
    R2[!keep] <- 0
    R2
  }

  # helper: polychoric/tetrachoric on dummy-coded data
  tetrachoric_matrix <- function(X,
                                 missing = c("pairwise", "listwise"),
                                 item_id = NULL,
                                 correct = 0.5) {
    missing <- match.arg(missing)
    X <- as.data.frame(X)

    if (missing == "listwise") {
      X <- X[stats::complete.cases(X), , drop = FALSE]
    }

    p <- ncol(X)
    R <- matrix(0, p, p)
    diag(R) <- 1
    colnames(R) <- colnames(X)
    rownames(R) <- colnames(X)

    if (p < 2L) return(R)

    # define once (not inside loop)
    quiet_tetrachoric_pair <- function(xij, correct = 0.5) {
      old_mc <- getOption("mc.cores")
      options(mc.cores = 1L)
      on.exit(options(mc.cores = old_mc), add = TRUE)

      tc_ij <- NULL

      invisible(
        utils::capture.output(
          utils::capture.output(
            tc_ij <- suppressWarnings(
              tryCatch(
                psych::tetrachoric(xij, correct = correct),
                error = function(e) NULL
              )
            ),
            type = "message"
          ),
          type = "output"
        )
      )

      tc_ij
    }

    # upper triangle only
    for (j in 2:p) {
      for (i in 1:(j - 1)) {

        if (!is.null(item_id) && item_id[i] == item_id[j]) next

        if (missing == "pairwise") {
          ok <- stats::complete.cases(X[[i]], X[[j]])
          xij <- X[ok, c(i, j), drop = FALSE]
        } else {
          xij <- X[, c(i, j), drop = FALSE]
        }

        if (nrow(xij) < 2L) {
          R[i, j] <- NA_real_
          next
        }

        if (length(unique(xij[[1]])) < 2L || length(unique(xij[[2]])) < 2L) {
          R[i, j] <- NA_real_
          next
        }

        tc_ij <- quiet_tetrachoric_pair(xij, correct = correct)

        if (is.null(tc_ij) || is.null(tc_ij$rho) || !is.matrix(tc_ij$rho)) {
          R[i, j] <- NA_real_
          next
        }

        R[i, j] <- tc_ij$rho[1, 2]
      }
    }

    # mirror upper -> lower
    R[lower.tri(R)] <- t(R)[lower.tri(R)]

    R[is.na(R)] <- 0
    diag(R) <- 1

    R
  }

  # Scenario metadata
  has_weights <- !is.null(weights)
  need_pvals  <- isTRUE(remove_nonsignificant)

  cor_method_requested <- cor_method
  cor_method_used <- cor_method
  cor_backend <- NA_character_
  cor_fallback_note <- NULL
  p_backend <- NA_character_

  # Polychoric support currently only for C1/C2:
  ## no weights + no significance filtering + pairwise/listwise missing
  if (cor_method == "polychoric" && (has_weights || need_pvals)) {
    cor_method_used <- "pearson"
    cor_fallback_note <- paste0(
      "Requested cor_method='polychoric' with ",
      if (has_weights) "weights" else "no weights",
      " and ",
      if (need_pvals) "remove_nonsignificant=TRUE" else "remove_nonsignificant=FALSE",
      ". This combination is not yet supported for polychoric/tetrachoric estimation; ",
      "falling back to Pearson correlations."
    )
    warning(cor_fallback_note, call. = FALSE)
  }

  # Objects produced by the correlation engine
  res_in_cor <- NULL
  res_in_p   <- NULL
  cor_engine_details <- list()

  # Dispatch
  if (cor_method_used == "polychoric") {

    # C1/C2 only (unweighted, no p-values)
    cor_backend <- "psych::tetrachoric"
    p_backend <- "not computed"

    res_in_cor <- tetrachoric_matrix(
      df_dummies, missing = missing_cor,
      item_id = item_id_per_dummycol,
      correct = 0.5)

    res_in_p   <- NULL

  } else {
    # Pearson correlation routes

    if (!has_weights && !need_pvals) {
      cor_backend <- "stats::cor"
      p_backend <- "not computed"

      if (missing_cor == "pairwise") {
        res_in_cor <- stats::cor(df_dummies, method = "pearson", use = "pairwise.complete.obs")
      } else {
        res_in_cor <- stats::cor(df_dummies, method = "pearson", use = "complete.obs")
      }

      res_in_p <- NULL
    }

    if (!has_weights && need_pvals) {
      cor_backend <- "psych::corr.test"
      p_backend <- "psych::corr.test"

      if (missing_cor == "pairwise") {
        ct <- psych::corr.test(df_dummies,
                               method = "pearson",
                               use = "pairwise.complete.obs",
                               adjust = "none")
      } else {
        cc <- stats::complete.cases(df_dummies)
        ct <- psych::corr.test(df_dummies[cc, , drop = FALSE],
                               method = "pearson",
                               use = "pairwise.complete.obs", # no missings remain after cc subset
                               adjust = "none")
      }

      res_in_cor <- ct$r
      res_in_p   <- ct$p
      res_in_cor <- apply_p_threshold(res_in_cor, res_in_p, sign_threshold,
                                      method = remove_nonsignificant_method)
    }

    if (has_weights && !need_pvals) {
      cor_backend <- "ResIN internal weighted Pearson"
      p_backend <- "not computed"

      wp <- weighted_pearson_matrix(
        X = df_dummies,
        w = weights,
        missing = missing_cor,
        need_p = FALSE,
        item_id = item_id_per_dummycol
      )

      res_in_cor <- wp$r
      res_in_p   <- NULL
    }

    if (has_weights && need_pvals) {
      cor_backend <- "ResIN internal weighted Pearson"
      p_backend <- "ResIN internal approximate weighted Pearson p-values"

      wp <- weighted_pearson_matrix(
        X = df_dummies,
        w = weights,
        missing = missing_cor,
        need_p = TRUE,
        item_id = item_id_per_dummycol
      )

      res_in_cor <- wp$r
      res_in_p   <- wp$p
      res_in_cor <- apply_p_threshold(res_in_cor, res_in_p, sign_threshold,
                                      method = remove_nonsignificant_method)

      cor_engine_details$n_pair_matrix <- wp$n_pair
      cor_engine_details$n_eff_matrix  <- wp$n_eff
      cor_engine_details$p_values_are_approximate <- TRUE
    }
  }

  if(!is.null(weights)) {
    cor_engine_details$weights_from_df <- weights_from_df
    cor_engine_details$weights_name <- weights_name
  }

  # Safety check
  if (is.null(res_in_cor) || !is.matrix(res_in_cor)) {
    stop("Correlation backend failed to return a valid correlation matrix.", call. = FALSE)
  }

  # Ensure dimnames
  colnames(res_in_cor) <- colnames(df_dummies)
  rownames(res_in_cor) <- colnames(df_dummies)

  if (!is.null(res_in_p)) {
    colnames(res_in_p) <- colnames(df_dummies)
    rownames(res_in_p) <- colnames(df_dummies)
  }

  ## Set all inner-variable correlations to 0
  for (idx in item_blocks) {
    res_in_cor[idx, idx] <- 0
    if (!is.null(res_in_p)) res_in_p[idx, idx] <- 0
  }

  res_in_cor_neg <-  res_in_cor

  ## Removing NA's and negatives
  if (remove_negative == TRUE) {
    if (offset > 1 | offset < -1) {
      stop("Argument offset must be a numeric value between -1 and 1", call. = FALSE)
    }
    if (offset != 0) {
      res_in_cor[res_in_cor < 0] <- res_in_cor[res_in_cor < 0] + offset
    }
    res_in_cor[res_in_cor < 0] <- 0
  }
  res_in_cor[is.na(res_in_cor)] <- 0

  ## Creating the same-items list
  same_items <- factor(item_id_per_dummycol, levels = seq_len(ncol(df_nodes)))
  levels(same_items) <- colnames(df_nodes)

  ## Generating the qgraph and igraph objects
  ResIN_igraph <- igraph::graph_from_adjacency_matrix(res_in_cor, mode = "undirected", weighted = TRUE, diag = FALSE)
  ResIN_qgraph <- qgraph::qgraph(res_in_cor, DoNotPlot = TRUE, layout = "spring", labels = rownames(res_in_cor))

  ## Force directed algorithm and principle component rotation
  if(remove_negative==FALSE) {
    graph_layout <- as.data.frame(prcomp(igraph::layout_nicely(ResIN_igraph))$x)
  } else {
    graph_layout <- as.data.frame(prcomp(igraph::layout_with_fr(ResIN_igraph))$x)
    rownames(graph_layout) <- rownames(res_in_cor)
    if(!is.null(left_anchor)) {
      if(graph_layout[left_anchor,][1] > mean(graph_layout[,1], na.rm = T)) {
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

  ## Network statistics (common structuration and centralization metrics)
  if(network_stats==TRUE) {
    node_net_stats <- qgraph::centrality_auto(ResIN_qgraph, weighted = TRUE)
    structuration <- apply(node_net_stats$node.centrality, 2, FUN = mean)
    structuration[5] <- mean(node_net_stats$ShortestPathLengths)
    structuration[6] <- DirectedClustering::ClustF(res_in_cor)$GlobalCC
    structuration[7] <- sum(res_in_cor[lower.tri(res_in_cor)])/sum(lower.tri(res_in_cor))
    structuration[8] <- (max(node_frame$x, na.rm = TRUE) - min(node_frame$x, na.rm = TRUE))/(max(node_frame$y, na.rm = TRUE) - min(node_frame$y, na.rm = TRUE))
    names(structuration) <- c("average_betweenness", "average_closeness", "average_strength", "average_expected_influence", "average_path_length", "global_clustering", "link_density", "linearization")
    centralization <- apply(node_net_stats$node.centrality, 2, FUN = sd)
    names(centralization) <- c("betweenness_centralization", "closeness_centralization", "strength_centralization", "expected_influence_centralization")
  } else {
    structuration <- c("not estimated")
    centralization <- c("not estimated")
  }

  ### Adding edge-betweenness if desired
  if(network_stats==TRUE) {
    edgelist_frame$from_to <- paste(edgelist_frame$from, edgelist_frame$to, sep = "_")
    node_net_stats$edge.betweenness.centrality$from_to <- paste(node_net_stats$edge.betweenness.centrality$from,
                                                                node_net_stats$edge.betweenness.centrality$to, sep = "_")

    node_net_stats$edge.betweenness.centrality$from <- NULL
    node_net_stats$edge.betweenness.centrality$to <- NULL

    edgelist_frame <- dplyr::left_join(edgelist_frame, node_net_stats$edge.betweenness.centrality, by = "from_to")
  }

  ## Integrating node-level network stats only into node-frame
  if(network_stats==TRUE) {
    node_frame <- cbind(node_frame, node_net_stats$node.centrality)
  }



  ## Perform clustering analysis
  if (detect_clusters == TRUE) {
    if (is.null(cluster_method)) {
      cluster <- do.call(igraph::cluster_leading_eigen, c(list(graph = ResIN_igraph), cluster_arglist))
    } else {
      if (cluster_method == "cluster_leading_eigen") {
        cluster <- do.call(igraph::cluster_leading_eigen, c(list(graph = ResIN_igraph), cluster_arglist))
      }
      if (cluster_method == "cluster_fast_greedy") {
        cluster <- do.call(igraph::cluster_fast_greedy, c(list(graph = ResIN_igraph), cluster_arglist))
      }
      if (cluster_method == "cluster_spinglass") {
        cluster <- do.call(igraph::cluster_spinglass, c(list(graph = ResIN_igraph), cluster_arglist))
      }
      if (cluster_method == "cluster_edge_betweenness") {
        cluster <- suppressWarnings(
          do.call(igraph::cluster_edge_betweenness, c(list(graph = ResIN_igraph), cluster_arglist)))
      }
      if (cluster_method == "cluster_louvain") {
        cluster <- do.call(igraph::cluster_louvain, c(list(graph = ResIN_igraph), cluster_arglist))
      }
      if (cluster_method == "cluster_walktrap") {
        cluster <- do.call(igraph::cluster_walktrap, c(list(graph = ResIN_igraph), cluster_arglist))
      }
      if (cluster_method == "cluster_infomap") {
        cluster <- do.call(igraph::cluster_infomap, c(list(graph = ResIN_igraph), cluster_arglist))
      }
      if (cluster_method == "cluster_optimal") {
        cluster <- do.call(igraph::cluster_optimal, c(list(graph = ResIN_igraph), cluster_arglist))
      }
      if (cluster_method == "cluster_fluid_communities") {

        if (is.null(cluster_arglist)) {
          cluster_arglist <- list(no.of.communities = 2)
          warning(
            "cluster_method = 'cluster_fluid_communities' requires 'no.of.communities'. ",
            "No cluster_arglist was supplied, so no.of.communities was set to 2.",
            call. = FALSE
          )
        } else if (is.null(cluster_arglist$no.of.communities)) {
          cluster_arglist$no.of.communities <- 2
          warning(
            "cluster_method = 'cluster_fluid_communities' requires 'no.of.communities'. ",
            "It was missing from cluster_arglist, so no.of.communities was set to 2.",
            call. = FALSE
          )
        }

        cluster <- do.call(
          igraph::cluster_fluid_communities,
          c(list(graph = ResIN_igraph), cluster_arglist)
        )
      }
    }

    communities <- igraph::membership(cluster)
    nodes <- names(communities)
    outcome <- data.frame(
      cluster = as.numeric(communities),
      from = nodes,
      stringsAsFactors = FALSE
    )

    ## Drop very small clusters (< 3 nodes)
    cluster_sizes <- table(outcome$cluster)
    small_clusters <- names(cluster_sizes[cluster_sizes < 3])
    outcome$cluster[outcome$cluster %in% small_clusters] <- NA
    outcome$cluster <- as.numeric(outcome$cluster)

    node_frame <- dplyr::left_join(node_frame, outcome, by = "from")
  }

  ## Cluster assignment matrix and vectors
  if (detect_clusters == TRUE & cluster_assignment == TRUE) {

    valid_clusters <- sort(unique(stats::na.omit(node_frame$cluster)))

    if (length(valid_clusters) == 0) {
      cluster_probs <- "not estimated"
      max_cluster <- "not estimated"
    } else {
      cluster_dummies <- as.matrix(df_dummies)

      for (i in seq_len(ncol(cluster_dummies))) {
        cluster_dummies[, i][cluster_dummies[, i] == 1] <-
          node_frame$cluster[node_frame$node_names == node_frame$node_names[i]]
      }

      cluster_dummies[cluster_dummies == 0] <- NA

      cluster_probs <- as.data.frame(
        matrix(NA_real_, nrow(df_dummies), length(valid_clusters))
      )

      for (j in seq_along(valid_clusters)) {
        cl_id <- valid_clusters[j]
        for (i in seq_len(nrow(df_dummies))) {
          denom <- sum(!is.na(cluster_dummies[i, ]))
          if (denom > 0) {
            cluster_probs[i, j] <-
              sum(cluster_dummies[i, ][cluster_dummies[i, ] == cl_id] / cl_id, na.rm = TRUE) / denom
          } else {
            cluster_probs[i, j] <- NA_real_
          }
        }
      }

      colnames(cluster_probs) <- paste0("cluster_", valid_clusters)

      temp_probs <- cluster_probs + stats::rnorm(nrow(cluster_probs) * ncol(cluster_probs), 0, 0.001)
      temp_probs <- dplyr::mutate(temp_probs, max_ind = max.col(temp_probs))
      max_cluster <- temp_probs$max_ind
    }

  } else {
    cluster_probs <- "not estimated"
    max_cluster <- "not estimated"
  }

  ## Calculating summary statistics based on co-variates
  if(!(is.null(node_covars)) & !(is.null(node_costats))) {
    if (length(node_covars) != length(node_costats)) {
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
  if (ResIN_scores && remove_negative) {

    score_dummies_x <- as.matrix(df_dummies)
    score_dummies_y <- as.matrix(df_dummies)

    for (i in seq_along(node_frame$node_names)) {
      item <- node_frame$node_names[i]
      score_dummies_x[score_dummies_x[, item] == 1, item] <- node_frame$x[i]
      score_dummies_y[score_dummies_y[, item] == 1, item] <- node_frame$y[i]
    }
    score_dummies_x[score_dummies_x == 0] <- NA
    score_dummies_y[score_dummies_y == 0] <- NA

    raw_x <- rowMeans(score_dummies_x, na.rm = TRUE)
    raw_y <- rowMeans(score_dummies_y, na.rm = TRUE)

    ### helper vectors for score estimates
    n_items     <- rowSums(df_dummies)
    popularity  <- colSums(df_dummies)
    items_list  <- lapply(seq_len(nrow(df_dummies)),
                          function(p) which(df_dummies[p, ] == 1))

    ### Heuristic lambda  (rarer items -> more pooling)
    k           <- stats::median(popularity)
    lambda_h    <- sapply(items_list, function(Ip) {
      num <- sum(popularity[Ip])
      1 - num / (num + k)
    })

    item_means_x <- colMeans(sweep(df_dummies, 1, raw_x, `*`), na.rm = TRUE)
    item_means_y <- colMeans(sweep(df_dummies, 1, raw_y, `*`), na.rm = TRUE)

    heur_x <- heur_y <- numeric(length(raw_x))
    for (p in seq_along(items_list)) {
      Ip       <- items_list[[p]]
      if (length(Ip) == 0L) next
      group_x  <- mean(item_means_x[Ip], na.rm = TRUE)
      group_y  <- mean(item_means_y[Ip], na.rm = TRUE)
      heur_x[p] <- (1 - lambda_h[p]) * raw_x[p] + lambda_h[p] * group_x
      heur_y[p] <- (1 - lambda_h[p]) * raw_y[p] + lambda_h[p] * group_y
    }

    ### Empirical-Bayes lambda (James-Stein shrinkage)
    #### Variance components for *x*
    within_var_x <- rowMeans((score_dummies_x - raw_x)^2, na.rm = TRUE)
    sigma2_x     <- mean(within_var_x, na.rm = TRUE)
    mu_x         <- mean(raw_x, na.rm = TRUE)
    tau2_x       <- max(0, stats::var(raw_x, na.rm = TRUE) -
                          sigma2_x * mean(1 / n_items, na.rm = TRUE))

    lambda_eb_x  <- (sigma2_x / n_items) / (tau2_x + sigma2_x / n_items)
    eb_x         <- (1 - lambda_eb_x) * raw_x + lambda_eb_x * mu_x

    #### Variance components for *y*
    within_var_y <- rowMeans((score_dummies_y - raw_y)^2, na.rm = TRUE)
    sigma2_y     <- mean(within_var_y, na.rm = TRUE)
    mu_y         <- mean(raw_y, na.rm = TRUE)
    tau2_y       <- max(0, stats::var(raw_y, na.rm = TRUE) -
                          sigma2_y * mean(1 / n_items, na.rm = TRUE))

    lambda_eb_y  <- (sigma2_y / n_items) / (tau2_y + sigma2_y / n_items)
    eb_y         <- (1 - lambda_eb_y) * raw_y + lambda_eb_y * mu_y

    ### Score output
    scores <- data.frame(
      raw_x  = raw_x,  raw_y  = raw_y,
      heur_x = heur_x, heur_y = heur_y,
      eb_x   = eb_x,   eb_y   = eb_y
    )
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
    } else {
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
    if (!is.null(plot_whichstat)) {

      ## Helper: add either fill- or colour-scales + labs depending on what is mapped
      add_discrete_scales <- function(p, aesthetic = c("fill", "colour"),
                                      palette, direction, label) {
        aesthetic <- match.arg(aesthetic)
        if (aesthetic == "fill") {
          p <- p +
            ggplot2::scale_fill_brewer(palette = palette, direction = direction) +
            ggplot2::labs(fill = label)
        } else {
          p <- p +
            ggplot2::scale_colour_brewer(palette = palette, direction = direction) +
            ggplot2::labs(colour = label)}
        p
      }


      add_continuous_scales <- function(p, aesthetic = c("fill", "colour"),
                                        palette, direction, label) {
        aesthetic <- match.arg(aesthetic)
        if (aesthetic == "fill") {
          p <- p +
            ggplot2::scale_fill_distiller(palette = palette, direction = direction) +
            ggplot2::labs(fill = label)
        } else {
          p <- p +
            ggplot2::scale_colour_distiller(palette = palette, direction = direction) +
            ggplot2::labs(colour = label)}
        p
      }

      ### Helper to add cluster method name
      pretty_algo <- function(cm) {
        if (is.null(cm) || is.na(cm) || !nzchar(cm)) return(NULL)
        out <- sub("^cluster_", "", cm)
        out <- gsub("_", " ", out)
        tools::toTitleCase(out)
      }

      #### Cluster legend title & counts
      cluster_legend_title <- function(cluster_vec, method_used) {
        algo_label <- pretty_algo(method_used)
        title <- if (!is.null(algo_label)) paste0("Cluster (", algo_label, ")") else "Cluster"

        #### Counts
        cl <- cluster_vec[!is.na(cluster_vec)]
        if (length(cl) == 0L) return(title)

        tab <- table(cl, useNA = "no")
        counts_line <- paste0(
          "Nodes per cluster: ",
          paste0(names(tab), "=", as.integer(tab), collapse = "; "))
        paste0(title, "\n", counts_line)
      }

      # Color by clusters
      if (plot_whichstat == "cluster") {
        if (isFALSE(detect_clusters)) {
          stop("You must set detect_clusters = TRUE in order to visualize node clusters")
        }

        # infer the method actually used in ResIN() clustering logic
        cluster_method_used <- if (is.null(cluster_method)) "cluster_leading_eigen" else cluster_method

        # legend title with algorithm + counts
        legend_label <- cluster_legend_title(node_frame$cluster, cluster_method_used)

        ResIN_ggplot <- remove_layer(ResIN_ggplot, c(2, 3))

        if (isFALSE(plot_responselabels)) {
          # Fill
          ResIN_ggplot <- ResIN_ggplot +
            ggplot2::geom_point(
              ggplot2::aes(x = node_frame$x, y = node_frame$y, fill = as.factor(node_frame$cluster)),
              shape = 21, size = 3)

          ResIN_ggplot <- add_discrete_scales(
            ResIN_ggplot, aesthetic = "fill",
            palette = color_palette, direction = direction,
            label = legend_label
          )

        } else {
          # Color
          ResIN_ggplot <- ResIN_ggplot +
            shadowtext::geom_shadowtext(
              ggplot2::aes(
                x = node_frame$x, y = node_frame$y, label = node_frame$node_names,
                colour = as.factor(node_frame$cluster)
              ), size = 3.8, bg.r = 0.1)

          ResIN_ggplot <- add_discrete_scales(
            ResIN_ggplot, aesthetic = "colour",
            palette = color_palette, direction = direction,
            label = legend_label
          )
        }

        ResIN_ggplot <- ResIN_ggplot +
          ggplot2::theme(legend.text = ggplot2::element_text(size = 10))
      }

      # Color by response choices
      if (plot_whichstat == "choices") {
        ResIN_ggplot <- remove_layer(ResIN_ggplot, c(2, 3))

        if (isFALSE(plot_responselabels)) {
          # Fill
          ResIN_ggplot <- ResIN_ggplot +
            ggplot2::geom_point(
              ggplot2::aes(x = node_frame$x, y = node_frame$y, fill = node_frame$choices),
              shape = 21, size = 3)
          ResIN_ggplot <- add_discrete_scales(
            ResIN_ggplot, aesthetic = "fill",
            palette = color_palette, direction = direction,
            label = "Response choices")
        } else {
          # Color
          ResIN_ggplot <- ResIN_ggplot +
            shadowtext::geom_shadowtext(
              ggplot2::aes(
                x = node_frame$x, y = node_frame$y, label = node_frame$node_names,
                colour = node_frame$choices), size = 3.8, bg.r = 0.1)
          ResIN_ggplot <- add_discrete_scales(
            ResIN_ggplot, aesthetic = "colour",
            palette = color_palette, direction = direction,
            label = "Response choices"
          )
        }

        ResIN_ggplot <- ResIN_ggplot +
          ggplot2::theme(legend.text = ggplot2::element_text(size = 10))
      }

      # Color by centrality stats (continuous)
      if (plot_whichstat %in% c("Strength", "Betweenness", "Closeness", "ExpectedInfluence")) {
        if (isFALSE(network_stats)) {
          stop("You must set network_stats = TRUE in order to visualize a node-level centrality metric.")
        }

        node_frame <- node_frame[order(node_frame[, plot_whichstat], decreasing = FALSE), ]
        ResIN_ggplot <- remove_layer(ResIN_ggplot, c(2, 3))

        if (isFALSE(plot_responselabels)) {
          # Fill
          ResIN_ggplot <- ResIN_ggplot +
            ggplot2::geom_point(
              ggplot2::aes(x = node_frame$x, y = node_frame$y, fill = node_frame[, plot_whichstat]),
              shape = 21, size = 3
            )
          ResIN_ggplot <- add_continuous_scales(
            ResIN_ggplot, aesthetic = "fill",
            palette = color_palette, direction = direction,
            label = plot_whichstat
          )
        } else {
          # Color
          ResIN_ggplot <- ResIN_ggplot +
            ggplot2::geom_text(
              ggplot2::aes(
                x = node_frame$x, y = node_frame$y, label = node_frame$node_names,
                colour = node_frame[, plot_whichstat]),size = 3.8)

          ResIN_ggplot <- add_continuous_scales(
            ResIN_ggplot, aesthetic = "colour",
            palette = color_palette, direction = direction,
            label = plot_whichstat)
        }

        ResIN_ggplot <- ResIN_ggplot +
          ggplot2::theme(legend.text = ggplot2::element_text(size = 10))
      }

      # Color by co-stats (continuous)
      if (!(plot_whichstat %in% c("cluster", "choices", "Strength", "Betweenness", "Closeness", "ExpectedInfluence"))) {
        if (is.null(node_covars) || is.null(node_costats)) {
          stop("Select node_covars and node_costats to visualize covariate summaries.")
        }

        ResIN_ggplot <- remove_layer(ResIN_ggplot, c(2, 3))

        if (isFALSE(plot_responselabels)) {
          # Fill
          ResIN_ggplot <- ResIN_ggplot +
            ggplot2::geom_point(
              ggplot2::aes(x = node_frame$x, y = node_frame$y, fill = node_frame[, plot_whichstat]),
              shape = 21, size = 3)
          ResIN_ggplot <- add_continuous_scales(
            ResIN_ggplot, aesthetic = "fill",
            palette = color_palette, direction = direction,
            label = plot_whichstat )
        } else {
          # Color
          ResIN_ggplot <- ResIN_ggplot +
            ggplot2::geom_text(
              ggplot2::aes(
                x = node_frame$x, y = node_frame$y, label = node_frame$node_names,
                colour = node_frame[, plot_whichstat]
              ), size = 3.8)
          ResIN_ggplot <- add_continuous_scales(
            ResIN_ggplot, aesthetic = "colour",
            palette = color_palette, direction = direction,
            label = plot_whichstat)
        }
        ResIN_ggplot <- ResIN_ggplot +
          ggplot2::theme(legend.text = ggplot2::element_text(size = 10))
      }
    }
  }

  ## Plotting ggplot graph:
  if(plot_ggplot==TRUE) {
    print(ResIN_ggplot)
  }

  # Multimodal graph (new as of version 2.3.1)
  multimodal_edge_overlay <- match.arg(multimodal_edge_overlay)

  if (multimodal) {
    if (!is.null(seed)) set.seed(seed)

    ## Long incidence table (Participant -> Response node)
    df_long <- df_dummies %>%
      dplyr::mutate(participant = as.character(seq_len(nrow(.)))) %>%
      tidyr::pivot_longer(
        cols      = -dplyr::all_of("participant"),
        names_to  = "item",
        values_to = "value"
      ) %>%
      dplyr::filter(.data[["value"]] == 1)

    ## Multimodal edge list
    ResIN_edgelist_multimodal <- data.frame(
      from   = df_long$participant,
      to     = df_long$item,
      weight = 1,
      stringsAsFactors = FALSE
    )

    ## Base vertex table (participants + response nodes)
    ### Anchor coordinates for response nodes
    anchors <- node_frame %>%
      dplyr::select(dplyr::all_of(c("x", "y", "node_names")))

    vertex_names <- unique(c(df_long$participant, df_long$item, anchors$node_names))

    vertex_df <- data.frame(
      name = vertex_names,
      stringsAsFactors = FALSE
    )

    ## Identify node type
    vertex_df$node_type <- ifelse(vertex_df$name %in% anchors$node_names, "Response node", "Participant")

    ## Attach metadata to nodes
    ### Response-node metadata: merge full node_frame columns
    resp_meta <- node_frame
    resp_meta$name <- resp_meta$node_names
    resp_meta$node_names <- NULL

    vertex_df <- dplyr::left_join(vertex_df, resp_meta, by = "name")

    # Participant-level covariates: raw values from df for node_covars
    if (!is.null(node_covars) && length(node_covars) > 0) {
      # Make sure requested covars exist
      node_covars_ok <- node_covars[node_covars %in% names(df)]
      if (length(node_covars_ok) > 0) {
        part_meta <- data.frame(
          name = as.character(seq_len(nrow(df))),
          stringsAsFactors = FALSE
        )
        part_meta <- cbind(part_meta, df[, node_covars_ok, drop = FALSE])
        vertex_df <- dplyr::left_join(vertex_df, part_meta, by = "name")
      }
    }

    ## Build multimodal igraph (store node attrs)
    gt <- igraph::graph_from_data_frame(
      dplyr::select(df_long, dplyr::all_of(c("participant", "item"))),
      directed = FALSE,
      vertices = vertex_df
    )

    # Ensure node_type is a vertex attribute
    igraph::V(gt)$node_type <- vertex_df$node_type[match(igraph::V(gt)$name, vertex_df$name)]

    # Optional: set edge weights
    igraph::E(gt)$weight <- 1

    ## Multimodal clustering with same algorithm as for the original network
    bi_cluster <- NULL
    bi_cluster_method_used <- NA_character_

    if (isTRUE(detect_clusters)) {
      # Default method: leading eigen
      cm <- cluster_method
      if (is.null(cm)) cm <- "cluster_leading_eigen"
      bi_cluster_method_used <- cm

      fun <- tryCatch(utils::getFromNamespace(cm, "igraph"), error = function(e) NULL)

      if (!is.null(fun)) {
        bi_cluster <- tryCatch(
          do.call(fun, c(list(graph = gt), cluster_arglist)),
          error = function(e) NULL
        )
      }
    }

    if (!is.null(bi_cluster)) {
      igraph::V(gt)$cluster <- as.integer(igraph::membership(bi_cluster))
    } else {
      igraph::V(gt)$cluster <- NA_integer_
    }

    ## Layout: fix response nodes to their ResIN coordinates
    idx_fix <- match(anchors$node_names, igraph::V(gt)$name)

    n    <- igraph::vcount(gt)
    minx <- rep(-Inf, n);  maxx <- rep( Inf, n)
    miny <- rep(-Inf, n);  maxy <- rep( Inf, n)

    # Fix anchors
    minx[idx_fix] <- maxx[idx_fix] <- anchors$x
    miny[idx_fix] <- maxy[idx_fix] <- anchors$y

    lay <- igraph::layout_with_fr(
      gt,
      minx = minx, maxx = maxx,
      miny = miny, maxy = maxy,
      niter = 3000
    )

    bi_rot <- stats::princomp(lay)$scores

    ## Store coordinates back into vertex_df and graph
    vertex_df <- igraph::as_data_frame(gt, what = "vertices")
    vertex_df$x <- bi_rot[, 1]
    vertex_df$y <- bi_rot[, 2]

    igraph::V(gt)$x <- vertex_df$x
    igraph::V(gt)$y <- vertex_df$y

    ## Determine multimodal plot coloring based on plot_whichstat ----
    plot_mode <- if (is.null(plot_whichstat)) "node_type" else plot_whichstat

    # Helper: infer raw covariate name for participants from a node stat name like partisan_mean
    infer_raw_covar <- function(stat_name, node_costats) {
      if (is.null(node_costats) || length(node_costats) == 0) return(stat_name)
      for (st in node_costats) {
        suf <- paste0("_", st, "$")
        if (grepl(suf, stat_name)) return(sub(suf, "", stat_name))
      }
      stat_name
    }

    # Initialize plot_val
    igraph::V(gt)$plot_val <- NA
    igraph::V(gt)$plot_grp <- NA

    if (identical(plot_mode, "cluster")) {
      ## color by multimodal clusters (if available); fallback to node_type
      if (isTRUE(detect_clusters) && any(!is.na(igraph::V(gt)$cluster))) {
        igraph::V(gt)$plot_grp <- as.factor(igraph::V(gt)$cluster)
      } else {
        igraph::V(gt)$plot_grp <- as.factor(igraph::V(gt)$node_type)
      }
    } else if (identical(plot_mode, "choices")) {
      vtab <- igraph::as_data_frame(gt, what = "vertices")
      igraph::V(gt)$plot_grp <- as.factor(vtab$choices)
    } else {
      # Try to treat plot_mode as a numeric covariate/statistic
      vtab <- igraph::as_data_frame(gt, what = "vertices")

      ## For response nodes: use node_frame column if it exists in vertex attrs
      ## For participants: use raw df covariate if stat_name corresponds to node_costat
      raw_name <- infer_raw_covar(plot_mode, node_costats)

      resp_vals <- if (plot_mode %in% names(vtab)) vtab[[plot_mode]] else NA
      part_vals <- if (raw_name %in% names(vtab)) vtab[[raw_name]] else NA

      plot_val <- ifelse(vtab$node_type == "Response node", resp_vals, part_vals)
      igraph::V(gt)$plot_val <- plot_val
    }

    pretty_algo <- function(cm) {
      if (is.null(cm) || is.na(cm) || !nzchar(cm)) return(NULL)
      out <- sub("^cluster_", "", cm)
      out <- gsub("_", " ", out)
      tools::toTitleCase(out)
    }

    algo_label <- pretty_algo(bi_cluster_method_used)

    legend_title <- switch(
      plot_mode,
      "node_type" = "Node type",
      "choices"   = "Response choice",
      "cluster"   = if (!is.null(algo_label)) paste0("Cluster (", algo_label, ")") else "Cluster",
      plot_mode   # fallback: use the plotted statistic name
    )

    ## Optionally add counts under legend title
    if (identical(plot_mode, "cluster") && any(!is.na(igraph::V(gt)$cluster))) {

      # Relying on vertex_df already in memory
      vtab_counts <- igraph::as_data_frame(gt, what = "vertices")

      tab <- table(vtab_counts$cluster, vtab_counts$node_type, useNA = "no")

      p_cnt <- if ("Participant" %in% colnames(tab)) tab[, "Participant"] else rep(0L, nrow(tab))
      r_cnt <- if ("Response node" %in% colnames(tab)) tab[, "Response node"] else rep(0L, nrow(tab))

      cl <- rownames(tab)

      counts_line <- paste0(
        "P/R per cluster: ",
        paste0(cl, "=", as.integer(p_cnt), "/", as.integer(r_cnt), collapse = "; ")
      )

      ## Putting counts on a second line in the legend title
      legend_title <- paste0(legend_title, "\n", counts_line)
    }

    # Plot multimodal graph with ggraph
    igraph::V(gt)$pt_size <- ifelse(igraph::V(gt)$node_type == "Response node", 3.2, 1.6)

    ## choose node layer (ONLY the layer)
    node_layer <- if (identical(plot_mode, "cluster") || identical(plot_mode, "choices")) {
      ggraph::geom_node_point(
        ggplot2::aes(
          colour = .data[["plot_grp"]],
          size   = .data[["pt_size"]],
          shape  = .data[["node_type"]]
        )
      )
    } else if (!is.null(plot_whichstat)) {
      ggraph::geom_node_point(
        ggplot2::aes(
          colour = .data[["plot_val"]],
          size   = .data[["pt_size"]],
          shape  = .data[["node_type"]]
        )
      )
    } else {
      ggraph::geom_node_point(
        ggplot2::aes(
          colour = .data[["node_type"]],
          size   = .data[["pt_size"]],
          shape  = .data[["node_type"]]
        )
      )
    }

    ## Optional base-layer: original ResIN edges appear underneath multimodal edges if plotted together
    resin_edge_layer_scale <- NULL
    resin_edge_layer <- NULL
    bip_edge_layer   <- NULL

    ## Helper function for coordinate lookup from the multimodal network layout
    resin_edge_layer_scale <- NULL
    coord_x <- setNames(vertex_df$x, vertex_df$name)
    coord_y <- setNames(vertex_df$y, vertex_df$name)

    ## ResIN edges drawn with multimodal coordinates
    if (multimodal_edge_overlay %in% c("ResIN", "both")) {

      ebase <- edgelist_frame

      ## Ensuring endpoints named 'from' and 'to'
      if (!all(c("from", "to") %in% names(ebase))) {
        stop("Internal error: edgelist_frame must contain 'from' and 'to' for ResIN edge overlay.", call. = FALSE)
      }

      ## Map endpoints to multimodal x/y
      from_chr <- as.character(ebase$from)
      to_chr   <- as.character(ebase$to)

      fx <- coord_x[from_chr]; fy <- coord_y[from_chr]
      tx <- coord_x[to_chr];   ty <- coord_y[to_chr]

      ## Keep only edges whose endpoints exist in the multimodal graph
      ok <- is.finite(fx) & is.finite(fy) & is.finite(tx) & is.finite(ty)
      eplot <- ebase[ok, , drop = FALSE]
      eplot$from.x <- fx[ok]; eplot$from.y <- fy[ok]
      eplot$to.x   <- tx[ok]; eplot$to.y   <- ty[ok]

      if (nrow(eplot) == 0L) {
        warning("multimodal_edge_overlay requested ResIN edges, but no ResIN endpoints were found in multimodal coordinates.", call. = FALSE)
      } else {

        ## Might want to add customizability here later
        base_alpha <- 0.7
        base_curv  <- 0.2
        base_col   <- "black"

        if (is.null(plot_edgestat) || !plot_edgestat %in% names(eplot)) {
          resin_edge_layer <- ggplot2::geom_curve(
            data = eplot,
            ggplot2::aes(x = .data[["from.x"]], y = .data[["from.y"]],
                         xend = .data[["to.x"]],   yend = .data[["to.y"]]),
            curvature = base_curv,
            colour    = base_col,
            alpha     = base_alpha,
            linewidth = 0.9,
            inherit.aes = FALSE
          )
        } else {
          ## ordering thin->thick so thick edges appear on top within the overlay
          eplot <- eplot[order(eplot[[plot_edgestat]], decreasing = FALSE), , drop = FALSE]

          resin_edge_layer <- ggplot2::geom_curve(
            data = eplot,
            ggplot2::aes(x = .data[["from.x"]], y = .data[["from.y"]],
                         xend = .data[["to.x"]],   yend = .data[["to.y"]],
                         linewidth = .data[[plot_edgestat]]),
            curvature = base_curv,
            colour    = base_col,
            alpha     = base_alpha,
            inherit.aes = FALSE
          )
          resin_edge_layer_scale <- ggplot2::scale_linewidth(range = c(0.2, 2.2), guide = "none")
        }
      }
    }

    ## Multimodal edges (participant-response)
    if (multimodal_edge_overlay %in% c("multimodal", "both")) {
      bip_edge_layer <- ggraph::geom_edge_link(alpha = 0.25, colour = "grey60")
    }

    ## Assembling the plot in the correct layer order
    multimodal_graph <- ggraph::ggraph(
      gt, layout = "manual",
      x = igraph::V(gt)$x,
      y = igraph::V(gt)$y
    )

    ## ResIN edges first (base layer)
    if (!is.null(resin_edge_layer)) {
      multimodal_graph <- multimodal_graph + resin_edge_layer
    }
    if (!is.null(resin_edge_layer_scale)) {
      multimodal_graph <- multimodal_graph + resin_edge_layer_scale
    }

    ## Multimodal edges second (participant--response)
    if (!is.null(bip_edge_layer)) {
      multimodal_graph <- multimodal_graph + bip_edge_layer
    }

    ## Nodes on top + legend + theme
    multimodal_graph <- multimodal_graph +
      node_layer +
      ggplot2::labs(colour = legend_title) +
      ggplot2::scale_size_identity(guide = "none") +
      ggplot2::scale_shape_manual(
        name   = "Node type",
        values = c("Participant" = 16, "Response node" = 17)
      ) +
      ggplot2::theme_void(base_size = 11) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::ggtitle("Multimodal ResIN graph")

    ## Return tidy output
    ResIN_nodeframe_multimodal <- vertex_df

    multimodal_output <- list(
      multimodal_igraph         = gt,
      coordinate_df            = ResIN_nodeframe_multimodal[, c("name", "x", "y", "node_type"), drop = FALSE],
      multimodal_ggraph         = multimodal_graph,
      ResIN_edgelist_multimodal = ResIN_edgelist_multimodal,
      ResIN_nodeframe_multimodal = ResIN_nodeframe_multimodal
    )

  } else {
    multimodal_output <- "not generated"
  }

  ## Collecting meta-information for transparency and reproducibility
  correlation_meta <- list(
    cor_method_requested = cor_method_requested,
    cor_method_used = cor_method_used,
    missing_cor = missing_cor,
    has_weights = has_weights,
    remove_nonsignificant = need_pvals,
    sign_threshold = if (need_pvals) sign_threshold else NA_real_,
    correlation_backend = cor_backend,
    pvalue_backend = p_backend,
    fallback_note = cor_fallback_note)

  ## Final bit of housekeeping
  node_frame$from <- NULL
  edgelist_frame$x <- NULL
  edgelist_frame$y <- NULL
  edgelist_frame$node_names <- NULL

  # Exporting additional features:
  graph_stats <- list(structuration, centralization)
  meta <- list(call = match.call(), created = created, seed = seed, df_id = df_id, df_n = nrow(df), df_p = ncol(df), node_vars = node_vars, correlation = correlation_meta, ResIN_version = resin_version, R_version = r_version)
  aux_objects <- list(res_in_cor, res_in_cor_neg, same_items, df_dummies, cluster_probs, max_cluster, ResIN_arglist, meta = meta)
  names(aux_objects) <- c("adj_matrix", "adj_matrix_neg", "same_items", "df_dummies", "cluster_probabilities", "max_clusterprob", "ResIN_arglist", "meta_information")

  output <- list(edgelist_frame, node_frame, ResIN_ggplot, scores, graph_stats, aux_objects, multimodal_output)
  names(output) <- c("ResIN_edgelist", "ResIN_nodeframe", "ResIN_ggplot", "ResIN_scores", "graph_stats", "aux_objects", "multimodal_output")

  class(output) <- c("ResIN", "list")

  return(output)
}
