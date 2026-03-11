# Extract and summarize the results of a bootstrap simulation undertaken on a ResIN object

Extract bootstrap draws from `"ResIN_boots_executed"` results. Failed
iterations (stored as `NULL`) are skipped automatically.

## Usage

``` r
ResIN_boots_extract(
  ResIN_boots_executed,
  what,
  summarize_results = FALSE,
  allow_missing = FALSE
)
```

## Arguments

- ResIN_boots_executed:

  An object of class `"ResIN_boots_executed"` (output of
  [`ResIN_boots_execute`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_execute.md)).

- what:

  Character scalar naming the quantity to extract (e.g.,
  `"global_clustering"`).

- summarize_results:

  Logical; if TRUE return a small summary table, otherwise return draws.

- allow_missing:

  Logical; if FALSE (default) the function errors if `what` is missing
  in any successful fit. If TRUE, missing iterations are kept as `NULL`
  and summaries are computed from available values.

## Value

If the extracted quantity is scalar per iteration, returns an object of
class `"ResIN_boots_draws"` (a numeric vector with attributes). If
`summarize_results = TRUE`, returns a one-row data.frame of summary
statistics.

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
