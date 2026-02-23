# Python NetworkX coercion helpers

Generic for exporting objects to a lightweight NetworkX-ready
representation. For `ResIN` objects, this creates edge and node CSV
tables that can be read via pandas and converted to a networkx graph.

## Usage

``` r
as.networkx(x, ...)
```

## Arguments

- x:

  Object to coerce/export.

- ...:

  Passed to methods.
