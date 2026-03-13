# Coerce a ResIN object to Gephi CSV table(s)

Produces Gephi-readable edge (and optionally node) tables from a `ResIN`
object and (by default) writes them to CSV. Set `dont_save_csv = TRUE`
to return tables without writing files.

## Usage

``` r
# S3 method for class 'ResIN'
as.gephi(
  x,
  file = "ResIN_gephi.csv",
  edges_only = TRUE,
  dont_save_csv = FALSE,
  weight_col = "weight",
  ...
)
```

## Arguments

- x:

  A `ResIN` object.

- file:

  Output file name (legacy style).

- edges_only:

  Logical; if TRUE write/return only edges.

- dont_save_csv:

  Logical; set TRUE to disable writing.

- weight_col:

  Name of the edge-weight column in `x$ResIN_edgelist`.

- ...:

  Ignored.

## Value

If `edges_only = TRUE`, an edge table data.frame. Otherwise a list with
`edges` and `nodes`. “When `dont_save_csv = FALSE`, the return value is
returned invisibly.”

## Examples

``` r
## Load the 12-item simulated Likert-type ResIN toy dataset
data(lik_data)

## Estimate a ResIN network
res <- ResIN(lik_data, plot_ggplot = FALSE)

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
