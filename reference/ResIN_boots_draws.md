# Extract bootrstrap draws from ResIN objects

`"ResIN_boots_draws"` objects are typically returned by
[`ResIN_boots_extract`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_extract.md)
when the requested quantity is scalar per bootstrap iteration (e.g.,
`"global_clustering"`). The object is a numeric vector with attributes
describing the bootstrap context.

## Details

Common attributes include:

- `what`:

  Name of the extracted quantity.

- `n_total`, `n_ok`, `n_failed`:

  Counts of total, successful, and failed iterations.

- `plan`:

  The `"ResIN_boots_prepped"` plan used to generate the results (if
  attached).

- `created`:

  Timestamp of execution (if attached).

## Methods

- `print(x)`:

  Compact display including median and 95% CI.

- `summary(object)`:

  Return descriptive statistics and quantiles.

- `confint(object)`:

  Quantile-based confidence intervals.

- `plot(x)`:

  Histogram with CI markers.
