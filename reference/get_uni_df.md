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
#> # A tibble: 25,411 × 250
#>    Package  Type    Title          Version Date  `Authors@R` Description License
#>    <chr>    <chr>   <chr>          <chr>   <chr> <chr>       <chr>       <chr>  
#>  1 SeqArray Package "Data managem… 1.51.10 2026… "c(person(… "Data mana… GPL-3  
#>  2 SeqArray Package "Data managem… 1.51.10 2026… "c(person(… "Data mana… GPL-3  
#>  3 SeqArray Package "Data managem… 1.51.10 2026… "c(person(… "Data mana… GPL-3  
#>  4 SeqArray Package "Data managem… 1.51.10 2026… "c(person(… "Data mana… GPL-3  
#>  5 SeqArray Package "Data managem… 1.51.10 2026… "c(person(… "Data mana… GPL-3  
#>  6 SeqArray Package "Data managem… 1.51.10 2026… "c(person(… "Data mana… GPL-3  
#>  7 SeqArray Package "Data managem… 1.51.10 2026… "c(person(… "Data mana… GPL-3  
#>  8 SeqArray Package "Data managem… 1.51.10 2026… "c(person(… "Data mana… GPL-3  
#>  9 SeqArray Package "Data managem… 1.51.10 2026… "c(person(… "Data mana… GPL-3  
#> 10 SeqArray Package "Data managem… 1.51.10 2026… "c(person(… "Data mana… GPL-3  
#> # ℹ 25,401 more rows
#> # ℹ 242 more variables: VignetteBuilder <chr>, ByteCompile <chr>,
#> #   LazyData <chr>, URL <chr>, BugReports <chr>, biocViews <chr>,
#> #   `Config/pak/sysreqs` <chr>, Repository <chr>, `Date/Publication` <chr>,
#> #   RemoteUrl <chr>, RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>,
#> #   Author <chr>, Maintainer <chr>, MD5sum <chr>, `_user` <chr>, `_type` <chr>,
#> #   `_file` <chr>, `_fileid` <chr>, `_filesize` <int>, `_sha256` <chr>, …
```
