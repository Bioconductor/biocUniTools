# Flatten raw universe data.frame by matching R, OS, and arch information from `_jobs` with `_binaries`

Adds additional columns to `_jobs_r_xy`, `_binaries_r_xy`, `_jobs_type`,
`_jobs_arch`, `_jobs_os_`, `_binaries_os_`

## Usage

``` r
get_uni_df(raw_df)
```

## Arguments

- raw_df:

  raw data.frame from R Universe API

## Value

data.frame

## Examples

``` r
raw_df <- get_raw_uni_df("bioc")
get_uni_df(raw_df)
#> # A tibble: 24,576 × 250
#>    Package  Title Version Author Maintainer Description URL   BugReports License
#>    <chr>    <chr> <chr>   <chr>  <chr>      <chr>       <chr> <chr>      <chr>  
#>  1 CBN2Path "CBN… 1.1.7   Willi… "William … "CBN2Path … "htt… https://g… MIT + …
#>  2 CBN2Path "CBN… 1.1.7   Willi… "William … "CBN2Path … "htt… https://g… MIT + …
#>  3 CBN2Path "CBN… 1.1.7   Willi… "William … "CBN2Path … "htt… https://g… MIT + …
#>  4 CBN2Path "CBN… 1.1.7   Willi… "William … "CBN2Path … "htt… https://g… MIT + …
#>  5 CBN2Path "CBN… 1.1.7   Willi… "William … "CBN2Path … "htt… https://g… MIT + …
#>  6 CBN2Path "CBN… 1.1.7   Willi… "William … "CBN2Path … "htt… https://g… MIT + …
#>  7 CBN2Path "CBN… 1.1.7   Willi… "William … "CBN2Path … "htt… https://g… MIT + …
#>  8 CBN2Path "CBN… 1.1.7   Willi… "William … "CBN2Path … "htt… https://g… MIT + …
#>  9 CBN2Path "CBN… 1.1.7   Willi… "William … "CBN2Path … "htt… https://g… MIT + …
#> 10 CBN2Path "CBN… 1.1.7   Willi… "William … "CBN2Path … "htt… https://g… MIT + …
#> # ℹ 24,566 more rows
#> # ℹ 241 more variables: Encoding <chr>, Roxygen <chr>, RoxygenNote <chr>,
#> #   `Config/testthat/edition` <chr>, biocViews <chr>, VignetteBuilder <chr>,
#> #   SystemRequirements <chr>, OS_type <chr>,
#> #   `Config/Bioconductor/UnsupportedPlatforms` <chr>,
#> #   `Config/pak/sysreqs` <chr>, Repository <chr>, `Date/Publication` <chr>,
#> #   RemoteUrl <chr>, RemoteRef <chr>, RemoteSha <chr>, …
```
