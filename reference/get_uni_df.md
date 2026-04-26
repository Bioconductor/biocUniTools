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
#> # A tibble: 25,690 × 250
#>    Package  Version Date  Title `Authors@R` Description License URL   BugReports
#>    <chr>    <chr>   <chr> <chr> <chr>       <chr>       <chr>   <chr> <chr>     
#>  1 graphite 1.57.1  2026… GRAP… "c(\nperso… "Graph obj… AGPL-3  http… https://g…
#>  2 graphite 1.57.1  2026… GRAP… "c(\nperso… "Graph obj… AGPL-3  http… https://g…
#>  3 graphite 1.57.1  2026… GRAP… "c(\nperso… "Graph obj… AGPL-3  http… https://g…
#>  4 graphite 1.57.1  2026… GRAP… "c(\nperso… "Graph obj… AGPL-3  http… https://g…
#>  5 graphite 1.57.1  2026… GRAP… "c(\nperso… "Graph obj… AGPL-3  http… https://g…
#>  6 graphite 1.57.1  2026… GRAP… "c(\nperso… "Graph obj… AGPL-3  http… https://g…
#>  7 graphite 1.57.1  2026… GRAP… "c(\nperso… "Graph obj… AGPL-3  http… https://g…
#>  8 graphite 1.57.1  2026… GRAP… "c(\nperso… "Graph obj… AGPL-3  http… https://g…
#>  9 graphite 1.57.1  2026… GRAP… "c(\nperso… "Graph obj… AGPL-3  http… https://g…
#> 10 amplican 1.33.10 NA    Auto… "c(\nperso… "`amplican… GPL-3   http… https://g…
#> # ℹ 25,680 more rows
#> # ℹ 241 more variables: Collate <chr>, VignetteBuilder <chr>, biocViews <chr>,
#> #   `Config/pak/sysreqs` <chr>, Repository <chr>, `Date/Publication` <chr>,
#> #   RemoteUrl <chr>, RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>,
#> #   Author <chr>, Maintainer <chr>, MD5sum <chr>, `_user` <chr>, `_type` <chr>,
#> #   `_file` <chr>, `_fileid` <chr>, `_filesize` <int>, `_sha256` <chr>,
#> #   `_created` <chr>, `_published` <chr>, `_jobs_job` <dbl>, …
```
