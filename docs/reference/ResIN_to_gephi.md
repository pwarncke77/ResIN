# (Deprecated.) Convert ResIN networks to Gephi-readable csv tables. Use `as.gephi()` method instead.

Deprecated/legacy function. Saves a ResIN graph as a series of csv files
readable by Gephi. Now supplanted by
[`as.gephi()`](https://pwarncke77.github.io/ResIN/reference/ResIN-coercion-generics.md)
method.

## Usage

``` r
ResIN_to_gephi(
  ResIN_object,
  file = "ResIN_gephi.csv",
  edges_only = TRUE,
  dont_save_csv = FALSE
)
```

## Arguments

- ResIN_object:

  The output of the ResIN function (a list with class ResIN).

- file:

  The name with .csv extension for the Gephi readable file to be output
  at. Defaults to "ResIN_gephi.csv".

- edges_only:

  Logical; if TRUE write/return only edges.

- dont_save_csv:

  Logical; set TRUE to disable writing.

## Value

A series of csv files readable by Gephi

## References

Source code of original function (\< version 2.2.0) had been adapted
from: https://github.com/RMHogervorst/gephi?tab=MIT-1-ov-file#readme

## Examples

``` r
## Load the 12-item simulated Likert-type ResIN toy dataset
data(lik_data)

## Estimate a ResIN network
res <- ResIN(lik_data, generate_ggplot = FALSE)
#> [1] "not generated"

## Create Gephi edge table without writing files
edges <- as.gephi(res, dont_save_csv = TRUE)
head(edges)
#>     Source   Target    Weight
#> 1 Item_1_1 Item_2_1 0.3320304
#> 2 Item_1_1 Item_3_1 0.5965548
#> 3 Item_1_1 Item_4_1 0.2213728
#> 4 Item_1_1 Item_5_1 0.6789836
#> 5 Item_1_1 Item_6_1 0.7183272
#> 6 Item_1_1 Item_6_2 0.2490927

if (FALSE) { # \dontrun{
## Write CSV file(s) for import to Gephi
## (writes "ResIN_gephi.csv" by default)
as.gephi(res, file = "ResIN_gephi.csv")

## Write both edges and nodes tables
## (writes "ResIN_gephi_edges.csv" and "ResIN_gephi_nodes.csv")
as.gephi(res, file = "ResIN_gephi.csv", edges_only = FALSE)
} # }

```
