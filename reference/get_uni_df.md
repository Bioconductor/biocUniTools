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
#> # A tibble: 24,377 × 250
#>    Package Type    Title  Version Date  `Authors@R` Description License LazyData
#>    <chr>   <chr>   <chr>  <chr>   <chr> <chr>       <chr>       <chr>   <chr>   
#>  1 HIBAG   Package HLA G… 1.47.1  2026… "c(person(… "Imputes H… GPL-3   yes     
#>  2 HIBAG   Package HLA G… 1.47.1  2026… "c(person(… "Imputes H… GPL-3   yes     
#>  3 HIBAG   Package HLA G… 1.47.1  2026… "c(person(… "Imputes H… GPL-3   yes     
#>  4 HIBAG   Package HLA G… 1.47.1  2026… "c(person(… "Imputes H… GPL-3   yes     
#>  5 HIBAG   Package HLA G… 1.47.1  2026… "c(person(… "Imputes H… GPL-3   yes     
#>  6 HIBAG   Package HLA G… 1.47.1  2026… "c(person(… "Imputes H… GPL-3   yes     
#>  7 HIBAG   Package HLA G… 1.47.1  2026… "c(person(… "Imputes H… GPL-3   yes     
#>  8 HIBAG   Package HLA G… 1.47.1  2026… "c(person(… "Imputes H… GPL-3   yes     
#>  9 HIBAG   Package HLA G… 1.47.1  2026… "c(person(… "Imputes H… GPL-3   yes     
#> 10 HIBAG   Package HLA G… 1.47.1  2026… "c(person(… "Imputes H… GPL-3   yes     
#> # ℹ 24,367 more rows
#> # ℹ 241 more variables: VignetteBuilder <chr>, SystemRequirements <chr>,
#> #   ByteCompile <chr>, biocViews <chr>, URL <chr>, `Config/pak/sysreqs` <chr>,
#> #   Repository <chr>, `Date/Publication` <chr>, RemoteUrl <chr>,
#> #   RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>, Author <chr>,
#> #   Maintainer <chr>, MD5sum <chr>, `_user` <chr>, `_type` <chr>,
#> #   `_file` <chr>, `_fileid` <chr>, `_filesize` <int>, `_sha256` <chr>, …
```
