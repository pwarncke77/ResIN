# Convert a ResIN object to a statnet/network object

Coerces a `ResIN` object to a `network` object (from the network package
used in the statnet ecosystem). The method preserves edge-level columns
from `x$ResIN_edgelist` as edge attributes and node-level columns from
`x$ResIN_nodeframe` as vertex attributes whenever available.

## Usage

``` r
# S3 method for class 'ResIN'
as.network(x, directed = FALSE, loops = FALSE, multiple = FALSE, ...)
```

## Arguments

- x:

  A `ResIN` object.

- directed:

  Logical; should the resulting network be directed? Defaults to
  `FALSE`.

- loops:

  Logical; allow self-loops? Defaults to `FALSE`.

- multiple:

  Logical; allow multiple edges? Defaults to `FALSE`.

- ...:

  Additional arguments passed to
  [`network::as.network()`](https://rdrr.io/pkg/network/man/network.html).

## Value

An object of class `network`.

## Examples

``` r
data(lik_data)
res <- ResIN(lik_data, generate_ggplot = FALSE, plot_ggplot = FALSE)

# ResIN re-exports network::as.network()
net <- as.network.ResIN(res) ## alternatively: as.network(res)
net
#>  Network attributes:
#>   vertices = 60 
#>   directed = FALSE 
#>   hyper = FALSE 
#>   loops = FALSE 
#>   multiple = FALSE 
#>   bipartite = FALSE 
#>   ResIN_source_class = ResIN 
#>   ResIN_graph_stats:
#>      Length Class  Mode   
#> [1,] 8      -none- numeric
#> [2,] 4      -none- numeric
#>   total edges= 570 
#>     missing edges= 0 
#>     non-missing edges= 570 
#> 
#>  Vertex attribute names: 
#>     Betweenness Closeness ExpectedInfluence Strength choices vertex.names x y 
#> 
#>  Edge attribute names: 
#>     edgebetweenness from.x from.y from_to to.x to.y weight 
```
