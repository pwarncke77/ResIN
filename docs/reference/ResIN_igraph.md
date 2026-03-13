# ResIN_igraph

Performs Response Item-Network analysis (ResIN) and exports the results
as an `igraph` object.

## Usage

``` r
ResIN_igraph(
  df,
  node_vars = NULL,
  cor_method = "auto",
  weights = NULL,
  method_wCorr = "Polychoric",
  remove_negative = TRUE,
  igraph_arglist = NULL,
  EBICglasso = FALSE,
  EBICglasso_arglist = NULL,
  cluster = TRUE,
  seed = 42
)
```

## Arguments

- df:

  A data-frame object containing the raw data.

- node_vars:

  An optional character string detailing the attitude item columns to be
  selected for ResIN analysis (i.e. the subset of attitude variables in
  df).

- cor_method:

  Which correlation method should be used? Defaults to "auto" which
  applies the `cor_auto` function from the `qgraph` package. Possible
  arguments are `"auto"`, `"pearson"`, `"kendall"`, and `"spearman"`.

- weights:

  An optional continuous vector of survey weights. Should have the same
  length as number of observations in df. If weights are provided,
  weighted correlation matrix will be estimated with the `weightedCorr`
  function from the `wCorr` package.

- method_wCorr:

  If weights are supplied, which method for weighted correlations should
  be used? Defaults to `"Polychoric"`. See
  [`wCorr::weightedCorr`](https://american-institutes-for-research.github.io/wCorr/reference/weightedCorr.html)
  for all correlation options.

- remove_negative:

  Should all negative correlations be removed? Defaults to TRUE (highly
  recommended). Setting to FALSE makes it impossible to estimate a
  force-directed network layout. Function will use igraph::layout_nicely
  instead.

- igraph_arglist:

  An optional argument list feeding additional instructions to `igraph`.
  Needs to be specified as an object list containing the arguments to be
  passed down.

- EBICglasso:

  Should a sparse, Gaussian-LASSO ResIN network be estimated? Defaults
  to FALSE. If set to TRUE, `EBICglasso` function from the `qgraph`
  packages performs regularization on (nearest positive-semi-definite)
  ResIN correlation matrix.

- EBICglasso_arglist:

  An argument list feeding additional instructions to the `EBICglasso`
  function if `EBICglasso` is set to TRUE.

- cluster:

  Optional, should community detection be performed on item response
  network? Defaults to FALSE. If set to TRUE, performs
  "cluster_leading_eigen" function from the igraph package and stores
  results in plotting_frame.

- seed:

  Random seed for force-directed algorithm.

## Value

A list object containing the `igraph` output object, a numeric vector
detailing which item responses belong to which item (`same_items`), and
optionally a matrix detailing community membership of different item
nodes (`clustering`).

## References

Csardi G, Nepusz T (2006). “The igraph software package for complex
network research.” InterJournal, Complex Systems, 1695.
https://igraph.org.

## Examples

``` r

## Load the 12-item simulated Likert-type ResIN toy dataset
data(lik_data)

## Run the function:
# \donttest{
ResIN_igraph <-  ResIN_igraph(lik_data)
#> Variables detected as ordinal: Item_1_1; Item_1_2; Item_1_3; Item_1_4; Item_1_5; Item_2_1; Item_2_2; Item_2_3; Item_2_4; Item_2_5; Item_3_1; Item_3_2; Item_3_3; Item_3_4; Item_3_5; Item_4_1; Item_4_2; Item_4_3; Item_4_4; Item_4_5; Item_5_1; Item_5_2; Item_5_3; Item_5_4; Item_5_5; Item_6_1; Item_6_2; Item_6_3; Item_6_4; Item_6_5; Item_7_1; Item_7_2; Item_7_3; Item_7_4; Item_7_5; Item_8_1; Item_8_2; Item_8_3; Item_8_4; Item_8_5; Item_9_1; Item_9_2; Item_9_3; Item_9_4; Item_9_5; Item_10_1; Item_10_2; Item_10_3; Item_10_4; Item_10_5; Item_11_1; Item_11_2; Item_11_3; Item_11_4; Item_11_5; Item_12_1; Item_12_2; Item_12_3; Item_12_4; Item_12_5


## Plot and/or investigate as you wish:
igraph::plot.igraph(ResIN_igraph$igraph_obj)

# }
```
