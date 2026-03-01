# Coerce a ResIN object to a tidygraph graph

Converts a `ResIN` object to a
[`tidygraph::tbl_graph`](https://tidygraph.data-imaginist.com/reference/tbl_graph.html)
object while preserving as much node- and edge-level information as
possible from `x$ResIN_nodeframe` and `x$ResIN_edgelist`.

Because `tidygraph` stores edge endpoints as integer node indices, the
original edge endpoint labels are preserved in additional edge columns
`from_name` and `to_name`.

If `ResIN_nodeframe` or `ResIN_edgelist` are unavailable, the method
falls back to a simpler conversion via
[`as.igraph()`](https://pwarncke77.github.io/ResIN/reference/ResIN-reexports.md)
followed by
[`tidygraph::as_tbl_graph()`](https://tidygraph.data-imaginist.com/reference/tbl_graph.html),
which may not preserve all metadata.

## Usage

``` r
# S3 method for class 'ResIN'
as.tidygraph(x, directed = FALSE, ...)
```

## Arguments

- x:

  A `ResIN` object.

- directed:

  Logical; should the resulting graph be treated as directed? Defaults
  to `FALSE`.

- ...:

  Ignored.

## Value

A
[`tidygraph::tbl_graph`](https://tidygraph.data-imaginist.com/reference/tbl_graph.html)
object. Node data include (when present) all columns from
`x$ResIN_nodeframe`; edge data include (when present) all columns from
`x$ResIN_edgelist`, plus `from_name`/`to_name` preserving original
endpoint labels.

## Examples

``` r
## Load toy data and estimate ResIN
data(lik_data)
res <- ResIN(lik_data, network_stats = TRUE, detect_clusters = TRUE,
             plot_ggplot = FALSE)

## Convert to tidygraph
tg <- as.tidygraph(res)

class(tg)
#> [1] "tbl_graph" "igraph"   
```
