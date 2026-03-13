# Create a bootstrap plan for re-estimating ResIN objects to derive statistical uncertainty estimates

Provides instructions for how to bootstrap a ResIN network to derive
uncertainty estimates around core quantities of interest. Requires
output of `ResIN` function.

## Usage

``` r
ResIN_boots_prepare(
  ResIN_object,
  n = 1000,
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
ResIN_obj <- ResIN(lik_data, network_stats = TRUE,
                      generate_ggplot = FALSE, plot_ggplot = FALSE)

# \donttest{
# Prepare for bootstrapping
prepped_boots <- ResIN_boots_prepare(ResIN_obj, n=50, boots_type="resample")

# Execute the prepared bootstrap list
executed_boots <-  ResIN_boots_execute(prepped_boots, parallel = TRUE,
                      detect_cores = TRUE, verbose = FALSE)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   2%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   6%  |                                                                              |======                                                                |   8%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  12%  |                                                                              |==========                                                            |  14%  |                                                                              |===========                                                           |  16%  |                                                                              |=============                                                         |  18%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  22%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  26%  |                                                                              |====================                                                  |  28%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  32%  |                                                                              |========================                                              |  34%  |                                                                              |=========================                                             |  36%  |                                                                              |===========================                                           |  38%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  42%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  46%  |                                                                              |==================================                                    |  48%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  52%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  56%  |                                                                              |=========================================                             |  58%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  62%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  66%  |                                                                              |================================================                      |  68%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  72%  |                                                                              |====================================================                  |  74%  |                                                                              |=====================================================                 |  76%  |                                                                              |=======================================================               |  78%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  82%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  86%  |                                                                              |==============================================================        |  88%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  92%  |                                                                              |==================================================================    |  94%  |                                                                              |===================================================================   |  96%  |                                                                              |===================================================================== |  98%  |                                                                              |======================================================================| 100%

# Extract results - here for example, the network (global)-clustering coefficient
ResIN_boots_extract(executed_boots, what = "global_clustering", summarize_results = TRUE)
#>                what n_total n_ok n_failed       min      q2.5        q5
#> 1 global_clustering      50   50        0 0.2636862 0.2696653 0.2701526
#>         q25    median      mean      q75       q95     q97.5       max
#> 1 0.2772609 0.2833676 0.2831208 0.289285 0.2950019 0.2972367 0.3029203
#>            sd
#> 1 0.008524506
# }
```
