# ResIN 2.3.0

## Major changes
- **Correlation engine overhaul.** ResIN now uses a unified, more efficient correlation backend that supports:
  - `cor_method = "pearson"` (fully supported across weighted/unweighted settings, pairwise/listwise missingness, and optional p-value pruning)
  - `cor_method = "polychoric"` (currently supported for unweighted correlation estimation; unsupported option combinations transparently fall back to Pearson correlation)
- **New missingness control for correlations.** Added `missing_cor = c("pairwise","listwise")` to explicitly control missing-data handling during correlation estimation (default: `"pairwise"`).

## Weighted correlations
- **Improved weights interface.** Survey `weights` can now be supplied either as:
  - a numeric vector of length `nrow(df)`, or
  - a character scalar naming a weights column in `df` (e.g., `weights = "psweights"`).  
  When `weights` is supplied as a column name and `node_vars = NULL`, the weights column is automatically excluded from the response-node variables used for estimation.

## Edge pruning / sparsification
- **New multiple-testing option for pruning.** Added `remove_nonsignificant_method`, enabling Benjamini–Hochberg FDR control (`"fdr"`) as an alternative to raw p-value pruning (`"default"`). `sign_threshold` is interpreted as the raw p-value cutoff under `"default"` and as the target FDR level under `"fdr"`.
- **EBICglasso retired.** `EBICglasso` and `EBICglasso_arglist` are retained for backwards compatibility but are **deprecated and ignored** as of 2.3.0. This feature is not statistically appropriate for the rank-deficient one-hot encoded ResIN correlation matrix.

## Bootstrapping workflow (S3 methods & robustness)
- Added/expanded S3 methods and documentation for ResIN bootstrap workflow objects:
  - `ResIN_boots_prepped` and `ResIN_boots_executed` now have clearer print/summary behavior.
  - `ResIN_boots_execute()` is more robust (handles failed iterations gracefully) and improves progress-bar formatting.
- `ResIN_boots_extract()` now supports multi-quantity extraction and improved summarization behavior, including consistent data-frame output for summaries.

## Conversion of ResIN objects for interoperability and manipulation in other software environments
- Added conventional S3 coercions for ResIN objects:
  - `as.igraph.ResIN()`, `as.qgraph.ResIN()`, `as.gephi.ResIN()`
  - `as.network.ResIN()` (statnet/network ecosystem), with `as.network()` re-exported for convenience
- Added lightweight cross-language export helpers:
  - `as.networkx.ResIN()` exports NetworkX compatible edge & node CSV tables for Python workflows (loadable via `pandas` + `networkx`).
  - `as.graphsjl.ResIN()` exports Graphs.jl compatible  edge & node CSV tables for Julia workflows (with integer vertex IDs suitable for `Graphs.jl`), while preserving node/edge metadata as table columns.
- Legacy conversion functions (e.g., `ResIN_to_igraph()`, `ResIN_to_qgraph()`, `ResIN_to_gephi()`) are retained for compatibility and delegate to the new coercion methods where appropriate.

## Documentation
- Improved help page titles and vignette metadata (more informative package-level index display).
- Added/updated documentation for new S3 classes/methods, conversions, and pruning options.

# ResIN 2.2.1

- Prior release.
