## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----silent intro, echo=FALSE, warning=FALSE, message=FALSE, fig.width = 7.2, fig.height = 5----
## Loading the required packages
library(ResIN)
library(dplyr, warn.conflicts = FALSE)

## Loading the data
BrJSocPsychol_2024 <- ResIN::BrJSocPsychol_2024

## Sub-setting and re-coding items in a liberal-conservative direction
Core_Items <- BrJSocPsychol_2024 %>% dplyr::select(Q9_1, Q9_2, Q9_3, Q9_4, 
                                                   Q9_5, Q9_6, Q9_7, Q9_8) %>% 
   dplyr::mutate(Q9_1 = recode(Q9_1, "Strongly Disagree" = "Strongly Agree",
                                     "Somewhat Disagree" = "Somewhat Agree",
                                     "Neutral" = "Neutral",
                                     "Somewhat Agree" = "Somewhat Disagree",
                                     "Strongly Agree" = "Strongly Disagree"),
                Q9_3 = recode(Q9_3,  "Strongly Disagree" = "Strongly Agree",
                                     "Somewhat Disagree" = "Somewhat Agree",
                                     "Neutral" = "Neutral",
                                     "Somewhat Agree" = "Somewhat Disagree",
                                     "Strongly Agree" = "Strongly Disagree"))

## Relabeling the attitudes
colnames(Core_Items) <- c("legal_abort", "equalize_incomes", "keep_immigrants", 
                          "welfare_spending", "gay_marriage", "protect_environ", 
                          "gun_control", "aid_blacks")

# Assigning response symbols for easier interpretation
Core_Items <- Core_Items %>%
  mutate(across(everything(), ~ recode(.,
    "Strongly Agree" = "++",
    "Somewhat Agree" = "+",
    "Neutral" = "+/-",
    "Somewhat Disagree" = "-",
    "Strongly Disagree" = "--",
  )))

Core_Items$dem_bias  <- as.numeric(BrJSocPsychol_2024$Q15_1) - as.numeric(BrJSocPsychol_2024$Q15_2)

## Separately specifying attitude nodes and covariate here:

ResIN_out <- ResIN(Core_Items,
                   node_vars = c("legal_abort", "equalize_incomes", "keep_immigrants",
                                 "welfare_spending", "gay_marriage", "protect_environ",
                                 "gun_control", "aid_blacks"), node_covars = c("dem_bias"),
                   node_costats = c("mean"), plot_whichstat = "dem_bias_mean",
                   plot_responselabels = FALSE, left_anchor = "legal_abort_++",
                   plot_title = "", plot_ggplot = FALSE,
                   color_palette = "RdBu", seed = 22)

ResIN_out$ResIN_ggplot + ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_blank())


## ----recoding items, warning=FALSE, message = FALSE---------------------------
## Loading and installing the required packages
if(!require("ResIN")) install.packages('ResIN')
library(ResIN)
if(!require("dplyr")) install.packages('dplyr')
library(dplyr, warn.conflicts = FALSE)

## Loading the data
BrJSocPsychol_2024 <- ResIN::BrJSocPsychol_2024

## Sub-setting and re-coding items in a liberal-conservative direction
Core_Items <- BrJSocPsychol_2024 %>% dplyr::select(Q9_1, Q9_2, Q9_3, Q9_4, 
                                                   Q9_5, Q9_6, Q9_7, Q9_8) %>% 
   dplyr::mutate(Q9_1 = dplyr::recode(Q9_1, "Strongly Disagree" = "Strongly Agree",
                                     "Somewhat Disagree" = "Somewhat Agree",
                                     "Neutral" = "Neutral",
                                     "Somewhat Agree" = "Somewhat Disagree",
                                     "Strongly Agree" = "Strongly Disagree"),
                Q9_3 = dplyr::recode(Q9_3,  "Strongly Disagree" = "Strongly Agree",
                                     "Somewhat Disagree" = "Somewhat Agree",
                                     "Neutral" = "Neutral",
                                     "Somewhat Agree" = "Somewhat Disagree",
                                     "Strongly Agree" = "Strongly Disagree"))

## Relabeling the attitudes
colnames(Core_Items) <- c("legal_abort", "equalize_incomes", "keep_immigrants", 
                          "welfare_spending", "gay_marriage", "protect_environ", 
                          "gun_control", "aid_blacks")

# Assigning response symbols for easier interpretation
Core_Items <- Core_Items %>%
  mutate(across(everything(), ~ dplyr::recode(.,
    "Strongly Agree" = "++",
    "Somewhat Agree" = "+",
    "Neutral" = "+/-",
    "Somewhat Disagree" = "-",
    "Strongly Disagree" = "--",
  )))

## Setting the seed for consistency
set.seed(22)


## ----head data, echo=TRUE-----------------------------------------------------
head(Core_Items)


## ----first ResIN, fig.width = 7.2, fig.height = 5-----------------------------
ResIN_out <- ResIN(Core_Items)

## ----first ResIN with geom_ploint, fig.width = 7.2, fig.height = 5------------
ResIN_out <- ResIN(Core_Items, plot_ggplot=FALSE,
                  left_anchor = "legal_abort_++", seed = 22)

ResIN_out$ResIN_ggplot

## ----fig.width = 7.2, fig.height = 4------------------------------------------
ResIN_out <- ResIN(Core_Items, left_anchor = "legal_abort_++",
                   plot_responselabels=FALSE, seed = 22)


## ----figure 2a, fig.width = 7.2, fig.height = 5-------------------------------
ResIN_out <- ResIN(Core_Items, plot_whichstat = "choices", 
                   response_levels = c("--", "-", "+/-" , "+", "++"), 
                   plot_responselabels = FALSE, 
                   plot_title = "BrJSocPsychol 2024 ResIN Network",
                   left_anchor = "legal_abort_++", seed = 22)


## ----fig.width = 7.2, fig.height = 5------------------------------------------
## Using leading eigenvalue by default:
ResIN_out <- ResIN(Core_Items, detect_clusters = TRUE, plot_whichstat = "cluster", 
                   plot_responselabels = FALSE, plot_title = "Leading eigenvalue community detection",
                   color_palette = "Set2", seed = 22, left_anchor = "legal_abort_++")

## Switching to edge-betweenness cluster detection:
ResIN_out <- ResIN(Core_Items, detect_clusters = TRUE, plot_whichstat = "cluster",
                   cluster_method = "cluster_fast_greedy", plot_responselabels = FALSE,
                   plot_title = "Fast and greedy community detection",
                   color_palette = "Set1", seed = 22, left_anchor = "legal_abort_++")


## -----------------------------------------------------------------------------
head(ResIN_out$aux_objects$cluster_probabilities)

## ----fig.width = 7.2, fig.height = 5------------------------------------------
ResIN_out <- ResIN(Core_Items, plot_whichstat = "Betweenness", plot_responselabels = TRUE, 
                   plot_title = "ResIN node betweenness centrality", seed = 22, color_palette = "Greens",
                   left_anchor = "legal_abort_++")


## ----warning=FALSE, fig.width = 7.2, fig.height = 5---------------------------
ResIN_out <- ResIN(Core_Items, detect_clusters = TRUE, plot_whichstat = "cluster",
                   cluster_method = "cluster_edge_betweenness", 
                   plot_edgestat = "edgebetweenness",
                   plot_responselabels = FALSE, 
                   plot_title = "Edge weight based on edge-betweenness centrality",
                   seed = 22, color_palette = "Set1", left_anchor = "legal_abort_++")


## ----warning=FALSE, fig.width = 7.2, fig.height = 5---------------------------
## Calculating the relative preference of Democrats over Republicans 
  ##(Democrat feelings thermometer minus republican feelings thermometer)
Core_Items$dem_bias  <- as.numeric(BrJSocPsychol_2024$Q15_1) - as.numeric(BrJSocPsychol_2024$Q15_2)

## Separately specifying attitude nodes and covariate here:

ResIN_out <- ResIN(Core_Items,
                   node_vars = c("legal_abort", "equalize_incomes", "keep_immigrants",
                                 "welfare_spending", "gay_marriage", "protect_environ",
                                 "gun_control", "aid_blacks"), node_covars = c("dem_bias"),
                   node_costats = c("mean"), plot_whichstat = "dem_bias_mean",
                   plot_responselabels = FALSE, left_anchor = "legal_abort_++",
                   plot_title = "Affective preference of Democrats over Republicans", 
                   color_palette = "RdBu", seed = 22)


## ----warning=FALSE, fig.width = 7.2, fig.height = 5---------------------------
ResIN_out <- ResIN(Core_Items,
                   node_vars = c("legal_abort", "equalize_incomes", "keep_immigrants",
                                 "welfare_spending", "gay_marriage", "protect_environ",
                                 "gun_control", "aid_blacks"), node_covars = c("dem_bias"),
                   node_costats = c("mean"), plot_whichstat = "dem_bias_mean",
                   plot_responselabels = TRUE, left_anchor = "legal_abort_++",
                   plot_title = "Affective preference of Democrats over Republicans", 
                   color_palette = "RdBu", seed = 22)

## ----warning=FALSE------------------------------------------------------------
head(ResIN_out$ResIN_nodeframe[, 8:9], 10)

## ----warning=FALSE, message=FALSE, fig.width = 7.2, fig.height = 5------------
## Further attaching partisan identification
Core_Items <- Core_Items %>% dplyr::mutate(partisan = as.numeric(recode(BrJSocPsychol_2024$Q13, 
                                       "Democrat" = 2,
                                       "Independent" = 1,
                                       "Republican" = 0)))

ResIN_out <- ResIN(Core_Items, node_vars = c("legal_abort", "equalize_incomes", 
                                             "keep_immigrants", "welfare_spending",
                                             "gay_marriage", "protect_environ",
                                             "gun_control", "aid_blacks"),
                   node_covars = c("dem_bias", "partisan"), node_costats = c("mean", "mean"),
                   plot_ggplot = FALSE)

## Loading the psych package to run the correlation test.
if(!require("psych")) install.packages('psych')
library(psych)
## Partisanship
corr.test(ResIN_out$ResIN_nodeframe$x, ResIN_out$ResIN_nodeframe$partisan_mean)

## Affective polarization
corr.test(ResIN_out$ResIN_nodeframe$x, ResIN_out$ResIN_nodeframe$dem_bias_mean)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
## Partisanship at the individual level
corr.test(Core_Items$partisan, ResIN_out$ResIN_scores$scores_x)

## Affective polarization at the individual level
corr.test(Core_Items$dem_bias, ResIN_out$ResIN_scores$scores_x)

## ----warning=FALSE, message = FALSE, fig.width = 7.2, fig.height = 5----------
## Let's generate a new, more lean ResIN analysis by omitting network statistics calculations, 
## plot generation, and individual-level scoring. This will optimize the execution time.
ResIN_out <- ResIN(Core_Items, node_vars = c("legal_abort", "equalize_incomes", 
                                             "keep_immigrants", "welfare_spending",
                                             "gay_marriage", "protect_environ",
                                             "gun_control", "aid_blacks"), 
                   node_covars = c("partisan"), node_costats = c("mean"), 
                   left_anchor = "legal_abort_++", network_stats = FALSE,
                   generate_ggplot = FALSE, plot_ggplot = FALSE,
                   ResIN_scores = FALSE, detect_clusters = FALSE)

ResIN_prepped <- ResIN_boots_prepare(ResIN_out, n = 1000, boots_type = "resample")

## Running the bootstrap might take a while; uncomment to try yourself
# ResIN_executed <- ResIN_boots_execute(ResIN_prepped, parallel = TRUE, n_cores = 2L)
# 
# saveRDS(ResIN_executed, "Bootstrap_example.RDS")
Bootstrap_example <- ResIN::Bootstrap_example

## Extracting the mean level partisanship per node across all iterations
partisan_means <- ResIN_boots_extract(Bootstrap_example, what = "partisan_mean")

## Extracting the node-level latent space coordinate across all iterations
x_postions <- ResIN_boots_extract(Bootstrap_example, what = "x")

## Correlating each list element and storing results in a new vector
correlations <- list()
for(i in 1:length(partisan_means)){
  correlations[[i]] <- cor(partisan_means[[i]], x_postions[[i]])
}

correlations <- unlist(correlations)

## Ignoring a handful few results where left and right are still flipped
correlations[correlations<(-0.75)] <- NA

summary(correlations)

## Let's plot the result with 95% CI lines:
correlations <- as.data.frame(correlations)
prob_lines <- quantile(correlations$correlations, c(0.025,0.5, 0.975), na.rm=TRUE) 

if(!require("ggplot2")) install.packages('ggplot2')
library(ggplot2)
ggplot(correlations, aes(x = correlations))+
  geom_density(fill = "lightblue")+
  ggtitle("Density of bootstrapped correlations between node position and partisanship")+
  labs(y = "Probability density", x = "Correlation between ResIN attitude node 
       position and average partisan identity")+
  geom_vline(xintercept = prob_lines[1], color = "darkred",linetype = 2)+
  geom_vline(xintercept = prob_lines[2], color = "black",linetype = 2, size = 1)+
  geom_vline(xintercept = prob_lines[3], color = "darkred",linetype = 2)+
  xlim(c(0.85, 0.87))+
  theme_classic() 


## -----------------------------------------------------------------------------
## Easily convert a ResIN object to igraph:
ResIN_igraph <- ResIN_to_igraph(ResIN_out)
class(ResIN_igraph)

ResIN_qgraph <- ResIN_to_qgraph(ResIN_out)
class(ResIN_qgraph)

# ResIN_to_gephi(ResIN_out, file = "ResIN_gephi.csv")

