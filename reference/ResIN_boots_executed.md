# Bootstrapped results

The `"ResIN_boots_executed"` object is returned by
[`ResIN_boots_execute`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_execute.md).
It contains a list of fitted `"ResIN"` objects, typically produced by
refitting ResIN on resampled/permuted versions of the original data.

## Details

The object is a list of length `n` where each element is (typically) a
[`ResIN`](https://pwarncke77.github.io/ResIN/reference/ResIN.md) fit.
For reproducibility, the following attributes may be present:

- `plan`:

  A `"ResIN_boots_prepped"` plan used to generate the results.

- `boot_inputs`:

  Optional list of bootstrap input data sets (only if
  `save_input = TRUE` in the plan).

- `created`:

  Time stamp when the executed object was created.

## Methods

- `print(x)`:

  Print a compact overview of the bootstrap results.

- `summary(object)`:

  Summarize the results (iterations, successes/failures, plan details).

- `length(x)`:

  Return the number of iterations.

- `x[i]`:

  Subset the results while preserving attached attributes.

## See also

[`ResIN_boots_prepare`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_prepare.md),
[`ResIN_boots_execute`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_execute.md),
[`ResIN_boots_extract`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_extract.md),
[`ResIN_boots_prepped`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_prepped.md)
