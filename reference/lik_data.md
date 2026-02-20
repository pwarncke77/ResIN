# Likert-type, mock-response data for "ResIN" package examples

An artificially created data-set (n=1000) of 12, 5-point Likert data.
Modeled on the basis of a standard normal data-generating process.
Likert scales contain 20 percent uncorrelated, homoscedastic measurement
error. This data-set is used for the examples in the "ResIN" package
vignette.

## Usage

``` r
data(lik_data)
```

## Format

An object of class `"data.frame"`

## References

This data set was artificially created for the ResIN package.

## Examples

``` r
data(lik_data)
head(lik_data)
#>   Item_1 Item_2 Item_3 Item_4 Item_5 Item_6 Item_7 Item_8 Item_9 Item_10
#> 1      4      4      4      5      4      5      3      5      5       5
#> 2      2      2      3      1      3      4      1      3      1       3
#> 3      3      2      4      4      3      5      2      3      2       5
#> 4      3      2      4      5      3      5      2      4      3       5
#> 5      3      2      4      3      3      5      2      3      3       5
#> 6      2      2      3      1      3      5      1      3      2       4
#>   Item_11 Item_12
#> 1       5       5
#> 2       1       3
#> 3       1       4
#> 4       3       5
#> 5       2       4
#> 6       1       3

```
