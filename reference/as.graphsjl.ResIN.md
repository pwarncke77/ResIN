# Export a ResIN object to Graphs.jl (Julia) tables

Produces Graphs.jl-style edge (and optionally node) tables from a
`ResIN` object. Edge endpoints are mapped to integer vertex IDs (`src`,
`dst`) to align with Graphs.jl in Julia. Node and edge metadata are
preserved as table columns. The node table stores the vertex mapping and
additional ResIN metadata.

## Usage

``` r
# S3 method for class 'ResIN'
as.graphsjl(
  x,
  file = "ResIN_graphsjl.csv",
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
with elements `edges` and `nodes`. The node table includes integer
`vertex_id` values and preserved node metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
data(lik_data)
res <- ResIN(lik_data, generate_ggplot = FALSE, plot_ggplot = FALSE)

# Return tables only (no files written)
jl_tbls <- as.graphsjl(res, dont_save_csv = TRUE, edges_only = FALSE)

# Default behavior writes CSV files
# as.graphsjl(res, file = "ResIN_graphsjl.csv", edges_only = FALSE)
} # }
```
