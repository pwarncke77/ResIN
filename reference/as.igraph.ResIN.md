# Coerce a ResIN object to an igraph graph

Converts a `ResIN` object to an `igraph` graph using the adjacency
matrix stored in `x$aux_objects$adj_matrix`.

## Usage

``` r
# S3 method for class 'ResIN'
as.igraph(x, mode = "undirected", weighted = TRUE, diag = FALSE, ...)
```

## Arguments

- x:

  A `ResIN` object.

- mode, weighted, diag:

  Passed to
  [`igraph::graph_from_adjacency_matrix()`](https://r.igraph.org/reference/graph_from_adjacency_matrix.html).

- ...:

  Additional arguments passed to
  [`igraph::graph_from_adjacency_matrix()`](https://r.igraph.org/reference/graph_from_adjacency_matrix.html).

## Value

An `igraph` object.

## Examples

``` r
## Load the 12-item simulated Likert-type ResIN toy dataset
data(lik_data)

## Run the function:

igraph_output <-  as.igraph(ResIN(lik_data, plot_ggplot = FALSE))

class(igraph_output)
#> [1] "igraph"

## Plot and/or investigate as you wish:
# \donttest{
igraph::plot.igraph(igraph_output)

# }
```
