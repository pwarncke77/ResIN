# (Deprecated.) Convert a ResIN network into an igraph object. Use `as.igraph()` method instead.

Deprecated/legacy function. Transforms the output of the `ResIN`
function into an
\[igraph\](https://igraph.org/r/doc/cluster_leading_eigen.html) object.
Now simply a wrapper for the
[`as.igraph()`](https://pwarncke77.github.io/ResIN/reference/ResIN-reexports.md)
method.

## Usage

``` r
ResIN_to_igraph(ResIN_object, igraph_arglist = NULL)
```

## Arguments

- ResIN_object:

  the output of the ResIN function (a list with class ResIN).

- igraph_arglist:

  an optional argument list to be supplied to the
  igraph::graph_from_adjacency_matrix function. If NULL, default is:
  list(mode = "undirected", weighted = TRUE, diag = FALSE).

## Value

A class `igraph` object.

## References

Csardi G, Nepusz T (2006). “The igraph software package for complex
network research.” InterJournal, Complex Systems, 1695.
https://igraph.org.

## See also

[`as.igraph`](https://pwarncke77.github.io/ResIN/reference/as.igraph.ResIN.md)
as the recommended interface.

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
