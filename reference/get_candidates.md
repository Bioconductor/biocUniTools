# Get candidate packages in R Universe based on criteria

Filters packages by same commit in BBS, vignettes passing, or check
status values.

## Usage

``` r
get_candidates(
  uni,
  uni_os_branch,
  r_version,
  bioc_version,
  os,
  arch = NA,
  vignettes = TRUE,
  commit = FALSE,
  check = c("NOTE", "WARNING", "OK")
)
```

## Arguments

- uni:

  character universe name

- uni_os_branch:

  character release or devel

- r_version:

  character

- bioc_version:

  character

- os:

  character

- arch:

  character x86_64 or arm64

- vignettes:

  logical (default TRUE) Check status of source / vignettes

- commit:

  logical (default FALSE) Check commit hash in RU and BBS?

- check:

  character vector of acceptable R CMD check statuses

- available_pkgs:

  data.frame of R Universe packages

## Value

data.frame of filtered candidate packages

## Examples

``` r
candidates <- get_candidates("bioc", "devel", "4.6.0", "3.23", "windows",
                             commit = TRUE)
#> adding rname 'https://bioconductor.org/checkResults/3.23/bioc-LATEST/BUILD_STATUS_DB.txt'
#> 
#> adding rname 'https://bioconductor.org/checkResults/3.23/bioc-LATEST/report.tgz'
#> 
#> adding rname 'https://bioconductor.org/packages/3.23/bioc/VIEWS'
#> 
```
