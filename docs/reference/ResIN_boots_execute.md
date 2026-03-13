# Carry out prepared bootstrap analyses on ResIN networks

Executes a bootstrap plan created by
[`ResIN_boots_prepare`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_prepare.md)
by repeatedly re-estimating ResIN on resampled or permuted versions of
the original data. Can optionally leverage CPU parallelism.

## Usage

``` r
ResIN_boots_execute(
  ResIN_boots_prepped,
  parallel = FALSE,
  detect_cores = TRUE,
  core_offset = 1L,
  n_cores = 2L,
  inorder = FALSE,
  verbose = TRUE
)
```

## Arguments

- ResIN_boots_prepped:

  A `"ResIN_boots_prepped"` bootstrap plan (output of
  [`ResIN_boots_prepare`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_prepare.md)).

- parallel:

  Logical; should execution use parallelism via `foreach` + a PSOCK
  cluster? Defaults to FALSE.

- detect_cores:

  Logical; should available CPU cores be detected automatically?
  Defaults to TRUE (ignored when `parallel = FALSE`).

- core_offset:

  Integer offset subtracted from the number of detected cores. Defaults
  to 1L. Change to 0L on low-overhead systems or if sure that system
  won't stall.

- n_cores:

  Manually specify number of cores (ignored if `detect_cores = TRUE` or
  `parallel = FALSE`).

- inorder:

  Logical; should parallel execution preserve sequential ordering?
  Defaults to FALSE.

- verbose:

  Logical; should the type of computational execution (parallel or
  sequential), the parallel engine (if any) and the number of cores be
  returned to the dashboard while the function is running?

## Value

An object of class `"ResIN_boots_executed"` containing `n` bootstrapped
`ResIN` fits. Use [`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`length()`](https://rdrr.io/r/base/length.html), and `[` to inspect or
subset results. See
[`ResIN_boots_executed`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_executed.md)
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
