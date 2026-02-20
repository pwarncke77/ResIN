# Coerce a ResIN object to a qgraph object

Converts a `ResIN` object to a `qgraph` object using the adjacency
matrix stored in `x$aux_objects$adj_matrix`.

## Usage

``` r
# S3 method for class 'ResIN'
as.qgraph(
  x,
  layout = "spring",
  maximum = 1,
  vsize = 6,
  DoNotPlot = TRUE,
  sampleSize = NULL,
  title = "ResIN graph in qgraph",
  mar = c(3, 8, 3, 8),
  normalize = FALSE,
  ...
)
```

## Arguments

- x:

  A `ResIN` object.

- layout, maximum, vsize, DoNotPlot, sampleSize, title, mar, normalize:

  Passed to
  [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

- ...:

  Additional arguments passed to
  [`qgraph::qgraph()`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

## Value

A `qgraph` object.

## Examples

``` r
## Load the 12-item simulated Likert-type ResIN toy dataset
data(lik_data)

## Run the function:
ResIN_qgraph <-  as.qgraph(ResIN(lik_data, plot_ggplot = FALSE))

class(ResIN_qgraph)
#> [1] "qgraph"
```
