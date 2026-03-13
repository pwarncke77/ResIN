# Prepared bootstrap plan

The `"ResIN_boots_prepped"` object is created by
[`ResIN_boots_prepare`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_prepare.md)
and provides a reproducible specification for running a bootstrap with
[`ResIN_boots_execute`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_execute.md).

## Details

A `"ResIN_boots_prepped"` object is a list with (at least) the following
elements:

- call:

  The matched call used to create the plan.

- boots_type:

  Character; `"resample"` or `"permute"`.

- n:

  Integer; number of bootstrap iterations.

- resample_size:

  Integer; sample size used when resampling rows.

- weights:

  Optional numeric vector of sampling weights (or `NULL`).

- save_input:

  Logical; whether to store bootstrap inputs during execution.

- seed_boots:

  Integer; seed used to generate per-iteration seeds.

- iter_seeds:

  Integer vector; per-iteration RNG seeds (useful for parallel
  execution).

- arglist:

  A list of arguments passed to
  [`ResIN`](https://pwarncke77.github.io/ResIN/reference/ResIN.md) for
  each re-fit (typically based on the original ResIN call, with plotting
  disabled).

- df_id:

  Character; MD5 hash identifying the raw data used to create the plan.

- ResIN_version:

  Character; ResIN package version used to create the plan.

## Methods

- `print(x)`:

  Print a compact summary of the bootstrap plan.

- `summary(object)`:

  Return a structured summary of the plan.

- `length(x)`:

  Return the number of iterations `n`.

- `x[i]`:

  Subset the plan to selected iteration indices (also updates `n`).

## See also

[`ResIN_boots_prepare`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_prepare.md),
[`ResIN_boots_execute`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_execute.md),
[`ResIN_boots_extract`](https://pwarncke77.github.io/ResIN/reference/ResIN_boots_extract.md)
