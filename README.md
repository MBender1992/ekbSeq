# ekbSeq

`ekbSeq` is a development R package containing a collection of helper functions for RNA-seq and transcriptomics analyses.

The package was developed primarily to support recurring analysis tasks across projects at the EKB laboratory and includes functions for data processing, differential expression analysis, annotation, visualization, and other commonly used transcriptomics workflows.

## Development status

> [!IMPORTANT]
> `ekbSeq` is a **development package** and should be considered a loosely maintained collection of analysis utilities rather than a stable, production-ready R package.

The package is under continuous development and may undergo frequent changes. Functions can be modified, renamed, replaced, or removed without prior notice, and backward compatibility is not guaranteed.

Some functions are actively used in current workflows, whereas others originate from older analyses and are retained mainly for compatibility with legacy scripts or because they may still be useful in specific situations. Consequently, not all functions receive the same level of maintenance or testing.

The repository is therefore primarily intended to:

- provide reusable helper functions for internal and project-specific RNA-seq analyses;
- reduce duplication of commonly used analysis code;
- facilitate reproducibility of workflows that depend on `ekbSeq`;
- serve as a development space for functions that may evolve as analysis strategies change.

It is **not intended to represent a comprehensively tested or formally maintained bioinformatics software package**.

## Installation

The current development version can be installed directly from GitHub using `devtools`:

```r
if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
}

devtools::install_github("MBender1992/ekbSeq")