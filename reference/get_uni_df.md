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
#> # A tibble: 25,222 × 250
#>    Package Type    Title       Version `Authors@R` Description License biocViews
#>    <chr>   <chr>   <chr>       <chr>   <chr>       <chr>       <chr>   <chr>    
#>  1 hammers Package "Utilities… 0.99.11 "person('A… "hammers i… MIT + … "Softwar…
#>  2 hammers Package "Utilities… 0.99.11 "person('A… "hammers i… MIT + … "Softwar…
#>  3 hammers Package "Utilities… 0.99.11 "person('A… "hammers i… MIT + … "Softwar…
#>  4 hammers Package "Utilities… 0.99.11 "person('A… "hammers i… MIT + … "Softwar…
#>  5 hammers Package "Utilities… 0.99.11 "person('A… "hammers i… MIT + … "Softwar…
#>  6 hammers Package "Utilities… 0.99.11 "person('A… "hammers i… MIT + … "Softwar…
#>  7 hammers Package "Utilities… 0.99.11 "person('A… "hammers i… MIT + … "Softwar…
#>  8 hammers Package "Utilities… 0.99.11 "person('A… "hammers i… MIT + … "Softwar…
#>  9 Aerith  NA      "visualiza… 0.99.12 "person(\"… "Visualisa… GPL-3   "Proteom…
#> 10 Aerith  NA      "visualiza… 0.99.12 "person(\"… "Visualisa… GPL-3   "Proteom…
#> # ℹ 25,212 more rows
#> # ℹ 242 more variables: Encoding <chr>, RoxygenNote <chr>,
#> #   VignetteBuilder <chr>, URL <chr>, BugReports <chr>,
#> #   `Config/testthat/edition` <chr>, `Config/pak/sysreqs` <chr>,
#> #   Repository <chr>, `Date/Publication` <chr>, RemoteUrl <chr>,
#> #   RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>, Author <chr>,
#> #   Maintainer <chr>, MD5sum <chr>, `_user` <chr>, `_type` <chr>, …
```
