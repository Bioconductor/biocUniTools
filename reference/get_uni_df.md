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
#> # A tibble: 24,566 × 250
#>    Package Type  Title Version `Authors@R` Description License Encoding LazyData
#>    <chr>   <chr> <chr> <chr>   <chr>       <chr>       <chr>   <chr>    <chr>   
#>  1 SMAD    Pack… Stat… 1.27.6  "person(gi… "Assigning… MIT + … UTF-8    false   
#>  2 SMAD    Pack… Stat… 1.27.6  "person(gi… "Assigning… MIT + … UTF-8    false   
#>  3 SMAD    Pack… Stat… 1.27.6  "person(gi… "Assigning… MIT + … UTF-8    false   
#>  4 SMAD    Pack… Stat… 1.27.6  "person(gi… "Assigning… MIT + … UTF-8    false   
#>  5 SMAD    Pack… Stat… 1.27.6  "person(gi… "Assigning… MIT + … UTF-8    false   
#>  6 SMAD    Pack… Stat… 1.27.6  "person(gi… "Assigning… MIT + … UTF-8    false   
#>  7 SMAD    Pack… Stat… 1.27.6  "person(gi… "Assigning… MIT + … UTF-8    false   
#>  8 SMAD    Pack… Stat… 1.27.6  "person(gi… "Assigning… MIT + … UTF-8    false   
#>  9 SMAD    Pack… Stat… 1.27.6  "person(gi… "Assigning… MIT + … UTF-8    false   
#> 10 SMAD    Pack… Stat… 1.27.6  "person(gi… "Assigning… MIT + … UTF-8    false   
#> # ℹ 24,556 more rows
#> # ℹ 241 more variables: RoxygenNote <chr>, URL <chr>, BugReports <chr>,
#> #   VignetteBuilder <chr>, biocViews <chr>, `Config/pak/sysreqs` <chr>,
#> #   Repository <chr>, `Date/Publication` <chr>, RemoteUrl <chr>,
#> #   RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>, Author <chr>,
#> #   Maintainer <chr>, MD5sum <chr>, `_user` <chr>, `_type` <chr>,
#> #   `_file` <chr>, `_fileid` <chr>, `_filesize` <int>, `_sha256` <chr>, …
```
