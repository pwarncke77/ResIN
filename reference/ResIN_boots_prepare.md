# Create a bootstrap plan for re-estimating ResIN objects to derive statistical uncertainty estimates

Provides instructions for how to bootstrap a ResIN network to derive
uncertainty estimates around core quantities of interest. Requires
output of `ResIN` function.

## Usage

``` r
ResIN_boots_prepare(
  ResIN_object,
  n = 10000,
  boots_type = "resample",
  resample_size = NULL,
  weights = NULL,
  save_input = FALSE,
  seed_boots = 42
)
```

## Arguments

- ResIN_object:

  A ResIN object to prepare bootstrapping workflow.

- n:

  Bootstrapping sample size. Defaults to 10.000.

- boots_type:

  What kind of bootstrapping should be performed? If set to "resample",
  function performs row-wise re-sampling of raw data (useful for e.g.,
  sensitivity or power analysis). If set to "permute", function will
  randomly reshuffle raw item responses (useful e.g., for simulating
  null-hypothesis distributions). Defaults to "resample".

- resample_size:

  Optional parameter determining sample size when `boots_type` is set to
  "resample". Defaults of to number of rows in raw data.

- weights:

  An optional weights vector that can be used to adjust the re-sampling
  of observations. Should either be NULL (default) or a positive numeric
  vector of the same length as the original data.

- save_input:

  Should all input information for each bootstrap iteration (including
  re-sampled/permuted data) be stored. Set to FALSE by default to save a
  lot of memory and disk storage.

- seed_boots:

  Random seed for bootstrap samples

## Value

An object of class `"ResIN_boots_prepped"` containing a bootstrap plan
(specification) used by
[`ResIN_boots_execute`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_execute.md).
Use [`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`length()`](https://rdrr.io/r/base/length.html), and `[` to inspect or
subset the plan. See
[`ResIN_boots_prepped`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_prepped.md)
for details.

## Examples

``` r
## Load the 12-item simulated Likert-type toy dataset
data(lik_data)

# Apply the ResIN function to toy Likert data:
ResIN_obj <- ResIN(lik_data, cor_method = "spearman", network_stats = TRUE,
                      generate_ggplot = FALSE, plot_ggplot = FALSE)

# \donttest{
# Prepare for bootstrapping
prepped_boots <- ResIN_boots_prepare(ResIN_obj, n=100, boots_type="resample")

# Execute the prepared bootstrap list
executed_boots <-  ResIN_boots_execute(prepped_boots, parallel = TRUE, detect_cores = TRUE)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%
#> 

# Extract results - here for example, the network (global)-clustering coefficient
ResIN_boots_extract(executed_boots, what = "global_clustering", summarize_results = TRUE)
#>                what n_total n_ok n_failed       min    q2.5        q5       q25
#> 1 global_clustering     100  100        0 0.2548646 0.26362 0.2660508 0.2762006
#>      median      mean       q75       q95     q97.5       max          sd
#> 1 0.2812725 0.2814186 0.2869986 0.2949319 0.2977245 0.3029203 0.009052593
# }
```
