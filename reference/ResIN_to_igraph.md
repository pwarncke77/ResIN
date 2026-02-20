# Convert a ResIN network into an igraph object

Transforms the output of the `ResIN` function into an
\[igraph\](https://igraph.org/r/doc/cluster_leading_eigen.html) object

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

## Examples

``` r
## Load the 12-item simulated Likert-type ResIN toy dataset
data(lik_data)

## Run the function:
# \donttest{
igraph_output <-  ResIN_to_igraph(ResIN(lik_data))



## Plot and/or investigate as you wish:
igraph::plot.igraph(igraph_output)

# }
```
