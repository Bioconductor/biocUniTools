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
#> # A tibble: 25,147 × 250
#>    Package Version Date       Title        Description Author Maintainer License
#>    <chr>   <chr>   <chr>      <chr>        <chr>       <chr>  <chr>      <chr>  
#>  1 limma   3.67.3  2026-04-22 Linear Mode… "Data anal… "Gord… Gordon Sm… GPL (>…
#>  2 limma   3.67.3  2026-04-22 Linear Mode… "Data anal… "Gord… Gordon Sm… GPL (>…
#>  3 limma   3.67.3  2026-04-22 Linear Mode… "Data anal… "Gord… Gordon Sm… GPL (>…
#>  4 limma   3.67.3  2026-04-22 Linear Mode… "Data anal… "Gord… Gordon Sm… GPL (>…
#>  5 limma   3.67.3  2026-04-22 Linear Mode… "Data anal… "Gord… Gordon Sm… GPL (>…
#>  6 limma   3.67.3  2026-04-22 Linear Mode… "Data anal… "Gord… Gordon Sm… GPL (>…
#>  7 limma   3.67.3  2026-04-22 Linear Mode… "Data anal… "Gord… Gordon Sm… GPL (>…
#>  8 limma   3.67.3  2026-04-22 Linear Mode… "Data anal… "Gord… Gordon Sm… GPL (>…
#>  9 limma   3.67.3  2026-04-22 Linear Mode… "Data anal… "Gord… Gordon Sm… GPL (>…
#> 10 limma   3.67.3  2026-04-22 Linear Mode… "Data anal… "Gord… Gordon Sm… GPL (>…
#> # ℹ 25,137 more rows
#> # ℹ 242 more variables: VignetteBuilder <chr>, URL <chr>, biocViews <chr>,
#> #   Repository <chr>, `Date/Publication` <chr>, RemoteUrl <chr>,
#> #   RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>, MD5sum <chr>,
#> #   `_user` <chr>, `_type` <chr>, `_file` <chr>, `_fileid` <chr>,
#> #   `_filesize` <int>, `_sha256` <chr>, `_created` <chr>, `_published` <chr>,
#> #   `_jobs_job` <dbl>, `_jobs_time` <int>, `_jobs_config` <chr>, …
```
