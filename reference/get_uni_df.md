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
#> # A tibble: 25,247 × 250
#>    Package          Type  Title Version `Authors@R` Description License Encoding
#>    <chr>            <chr> <chr> <chr>   <chr>       <chr>       <chr>   <chr>   
#>  1 Voyager          Pack… From… 1.13.1  "c(person(… "SpatialFe… Artist… UTF-8   
#>  2 Voyager          Pack… From… 1.13.1  "c(person(… "SpatialFe… Artist… UTF-8   
#>  3 Voyager          Pack… From… 1.13.1  "c(person(… "SpatialFe… Artist… UTF-8   
#>  4 Voyager          Pack… From… 1.13.1  "c(person(… "SpatialFe… Artist… UTF-8   
#>  5 Voyager          Pack… From… 1.13.1  "c(person(… "SpatialFe… Artist… UTF-8   
#>  6 Voyager          Pack… From… 1.13.1  "c(person(… "SpatialFe… Artist… UTF-8   
#>  7 Voyager          Pack… From… 1.13.1  "c(person(… "SpatialFe… Artist… UTF-8   
#>  8 Voyager          Pack… From… 1.13.1  "c(person(… "SpatialFe… Artist… UTF-8   
#>  9 SpatialFeatureE… Pack… Inte… 1.13.2  "c(person(… "A new S4 … Artist… UTF-8   
#> 10 SpatialFeatureE… Pack… Inte… 1.13.2  "c(person(… "A new S4 … Artist… UTF-8   
#> # ℹ 25,237 more rows
#> # ℹ 242 more variables: RoxygenNote <chr>, `Config/testthat/edition` <chr>,
#> #   biocViews <chr>, VignetteBuilder <chr>, URL <chr>, BugReports <chr>,
#> #   Collate <chr>, `Config/pak/sysreqs` <chr>, Repository <chr>,
#> #   `Date/Publication` <chr>, RemoteUrl <chr>, RemoteRef <chr>,
#> #   RemoteSha <chr>, NeedsCompilation <chr>, Author <chr>, Maintainer <chr>,
#> #   MD5sum <chr>, `_user` <chr>, `_type` <chr>, `_file` <chr>, …
```
