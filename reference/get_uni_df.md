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
#> # A tibble: 24,439 × 250
#>    Package  Type    Title          Version Date  `Authors@R` Description License
#>    <chr>    <chr>   <chr>          <chr>   <chr> <chr>       <chr>       <chr>  
#>  1 SAIGEgds Package "Scalable Imp… 2.11.4  2026… "c(person(… "Scalable … GPL-3  
#>  2 SAIGEgds Package "Scalable Imp… 2.11.4  2026… "c(person(… "Scalable … GPL-3  
#>  3 SAIGEgds Package "Scalable Imp… 2.11.4  2026… "c(person(… "Scalable … GPL-3  
#>  4 SAIGEgds Package "Scalable Imp… 2.11.4  2026… "c(person(… "Scalable … GPL-3  
#>  5 SAIGEgds Package "Scalable Imp… 2.11.4  2026… "c(person(… "Scalable … GPL-3  
#>  6 SAIGEgds Package "Scalable Imp… 2.11.4  2026… "c(person(… "Scalable … GPL-3  
#>  7 SAIGEgds Package "Scalable Imp… 2.11.4  2026… "c(person(… "Scalable … GPL-3  
#>  8 SAIGEgds Package "Scalable Imp… 2.11.4  2026… "c(person(… "Scalable … GPL-3  
#>  9 SAIGEgds Package "Scalable Imp… 2.11.4  2026… "c(person(… "Scalable … GPL-3  
#> 10 SAIGEgds Package "Scalable Imp… 2.11.4  2026… "c(person(… "Scalable … GPL-3  
#> # ℹ 24,429 more rows
#> # ℹ 242 more variables: SystemRequirements <chr>, VignetteBuilder <chr>,
#> #   ByteCompile <chr>, URL <chr>, biocViews <chr>, `Config/pak/sysreqs` <chr>,
#> #   Repository <chr>, `Date/Publication` <chr>, RemoteUrl <chr>,
#> #   RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>, Author <chr>,
#> #   Maintainer <chr>, MD5sum <chr>, `_user` <chr>, `_type` <chr>,
#> #   `_file` <chr>, `_fileid` <chr>, `_filesize` <int>, `_sha256` <chr>, …
```
