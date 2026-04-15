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
#> # A tibble: 24,425 × 250
#>    Package   Type    Title         Version Date  `Authors@R` Description License
#>    <chr>     <chr>   <chr>         <chr>   <chr> <chr>       <chr>       <chr>  
#>  1 SNPRelate Package "Parallel Co… 1.45.2  2026… "c(person(… "Genome-wi… GPL-3  
#>  2 SNPRelate Package "Parallel Co… 1.45.2  2026… "c(person(… "Genome-wi… GPL-3  
#>  3 SNPRelate Package "Parallel Co… 1.45.2  2026… "c(person(… "Genome-wi… GPL-3  
#>  4 SNPRelate Package "Parallel Co… 1.45.2  2026… "c(person(… "Genome-wi… GPL-3  
#>  5 SNPRelate Package "Parallel Co… 1.45.2  2026… "c(person(… "Genome-wi… GPL-3  
#>  6 SNPRelate Package "Parallel Co… 1.45.2  2026… "c(person(… "Genome-wi… GPL-3  
#>  7 SNPRelate Package "Parallel Co… 1.45.2  2026… "c(person(… "Genome-wi… GPL-3  
#>  8 SNPRelate Package "Parallel Co… 1.45.2  2026… "c(person(… "Genome-wi… GPL-3  
#>  9 SNPRelate Package "Parallel Co… 1.45.2  2026… "c(person(… "Genome-wi… GPL-3  
#> 10 SNPRelate Package "Parallel Co… 1.45.2  2026… "c(person(… "Genome-wi… GPL-3  
#> # ℹ 24,415 more rows
#> # ℹ 242 more variables: VignetteBuilder <chr>, LazyData <chr>, URL <chr>,
#> #   BugReports <chr>, biocViews <chr>, Repository <chr>,
#> #   `Date/Publication` <chr>, RemoteUrl <chr>, RemoteRef <chr>,
#> #   RemoteSha <chr>, NeedsCompilation <chr>, Author <chr>, Maintainer <chr>,
#> #   MD5sum <chr>, `_user` <chr>, `_type` <chr>, `_file` <chr>, `_fileid` <chr>,
#> #   `_filesize` <int>, `_sha256` <chr>, `_created` <chr>, `_published` <chr>, …
```
