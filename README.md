# biocUniTools reports

This branch holds the Quarto website that compares Bioconductor Build System
(BBS) and R Universe results. The biocUniTools package is on the `devel`
branch. `.github/workflows/quarto.yml` on `devel` renders this branch daily and
publishes it to `gh-pages`.

## Preview locally

From a checkout of this branch:

```sh
# Copy the vignettes from a devel checkout so the menu links work
mkdir -p vignettes
cp /path/to/biocUniTools/vignettes/*.qmd vignettes/

# Download the data (a few minutes)
Rscript -e 'biocUniTools::write_comparisons("reports/config.yml")'

quarto preview
```

To use the R code in a devel checkout without reinstalling biocUniTools, set
`BIOCUNITOOLS_SRC` before rendering:

```sh
export BIOCUNITOOLS_SRC=/path/to/biocUniTools
```

## Files

- `reports/config.yml`: branches, package types, and platform to compare
- `reports/_compare-page.qmd`: shared body of each comparison page
- `reports/compare/*.qmd`: one page per branch and package type
- `index.qmd`: summary of all comparisons
- `reports/compare.css`: styles
