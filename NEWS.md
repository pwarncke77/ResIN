# ResIN 2.3.2

## Node label plotting
- **Native response label sizing.** Added `responselabel_size` to `ResIN()`, which sets the font size of printed response-node labels directly, without post-hoc modification of the returned `ggplot2` object.
- **Label repelling.** Added `responselabel_repel`, which delegates response-node label placement to `ggrepel`: labels are iteratively displaced until they no longer collide, and short guide lines connect each label back to its node. `responselabel_tolerance` controls the strength with which labels are pushed apart, trading off legibility against the positional fidelity of labels relative to their nodes (a value of `0.5` generally offers a good compromise).

# ResIN 2.3.1
