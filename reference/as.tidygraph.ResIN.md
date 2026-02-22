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
tg
#> # A tbl_graph: 60 nodes and 570 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 60 × 10 (active)
#>        x       y node_names Betweenness Closeness Strength ExpectedInfluence
#>    <dbl>   <dbl> <chr>            <dbl>     <dbl>    <dbl>             <dbl>
#>  1 -8.34  1.59   Item_1_1             0   0.00165     5.66              5.66
#>  2 -5.45 -0.408  Item_1_2            68   0.00230     8.38              8.38
#>  3  2.00  0.294  Item_1_3            10   0.00265     5.81              5.81
#>  4  6.36 -0.743  Item_1_4             5   0.00224     7.74              7.74
#>  5  7.65  1.36   Item_1_5             0   0.00173     5.29              5.29
#>  6 -7.17  0.0201 Item_2_1           388   0.00215     9.99              9.99
#>  7  1.30 -0.109  Item_2_2           218   0.00276     6.55              6.55
#>  8  7.31 -1.39   Item_2_3             0   0.00164     2.98              2.98
#>  9  7.53 -0.252  Item_2_4             0   0.00188     5.05              5.05
#> 10  7.85  1.75   Item_2_5             0   0.00174     5.39              5.39
#> # ℹ 50 more rows
#> # ℹ 3 more variables: cluster <dbl>, choices <fct>, name <chr>
#> #
#> # Edge Data: 570 × 11
#>    from    to weight from.x from.y  to.x   to.y from_to          edgebetweenness
#>   <int> <int>  <dbl>  <dbl>  <dbl> <dbl>  <dbl> <chr>                      <dbl>
#> 1     1     6  0.332  -8.34   1.59 -7.17 0.0201 Item_1_1_Item_2…              45
#> 2     1    11  0.597  -8.34   1.59 -7.88 0.406  Item_1_1_Item_3…               6
#> 3     1    16  0.221  -8.34   1.59 -6.21 0.179  Item_1_1_Item_4…               0
#> # ℹ 567 more rows
#> # ℹ 2 more variables: from_name <chr>, to_name <chr>
```
