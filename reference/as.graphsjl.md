# Julia Graphs.jl coercion helpers

Generic for exporting objects to a lightweight Graphs.jl-ready
representation. For `ResIN` objects, this creates edge and node CSV
tables suitable for loading with CSV.jl/DataFrames.jl, with integer
vertex IDs for direct use in Graphs.jl.

## Usage

``` r
as.graphsjl(x, ...)
```

## Arguments

- x:

  Object to coerce/export.

- ...:

  Passed to methods.
