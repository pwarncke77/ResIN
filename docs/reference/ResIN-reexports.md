# Re-exported functions used by ResIN

These functions are re-exported so users can call common coercion
generics (e.g., `as.igraph()` and `as.network()`) directly after loading
ResIN, with S3 dispatch to `ResIN` methods.

## Usage

``` r
as.igraph(x, ...)

as.network(x, ...)
```
