# Export a ResIN object to NetworkX (Python) tables

Produces NetworkX-ready edge (and optionally node) tables from a `ResIN`
object for further manipulation in Python. By default, this method
writes CSV files that can be imported into Python via pandas and
networkx. Node and edge metadata are preserved as table columns.

## Usage

``` r
# S3 method for class 'ResIN'
as.networkx(
  x,
  file = "ResIN_networkx.csv",
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

  Output file name (legacy style). If `edges_only = TRUE`, the edge
  table is written to `file`. If `edges_only = FALSE`, `file` is treated
  as a prefix and `"_edges.csv"` / `"_nodes.csv"` are appended (with any
  trailing `.csv` removed).

- edges_only:

  Logical; if TRUE (default), only write/return edge table.

- dont_save_csv:

  Logical; if FALSE (default), write CSV output. If TRUE, no files are
  written and the resulting table(s) are returned visibly.

- weight_col:

  Preferred edge-weight column name. Defaults to `"weight"`.

- ...:

  Ignored.

## Value

If `edges_only = TRUE`, an edge table `data.frame`. Otherwise a list
with elements `edges` and `nodes`.

## Examples

``` r
if (FALSE) { # \dontrun{
data(lik_data)
res <- ResIN(lik_data, generate_ggplot = FALSE, plot_ggplot = FALSE)

# Return tables only (no files written)
nx_tbls <- as.networkx(res, dont_save_csv = TRUE, edges_only = FALSE)

# Default behavior writes CSV files
# as.networkx(res, file = "ResIN_networkx.csv", edges_only = FALSE)
} # }
```
