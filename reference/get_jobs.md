# Get jobs for an R Universe build

Adds Artifact and JobUrl

## Usage

``` r
get_jobs(uni, uni_os_branch, r_version, bioc_version)
```

## Arguments

- uni:

  character universe name

- uni_os_branch:

  character release or devel

- r_version:

  character

- bioc_version:

  character

## Value

data.frame packages in a universe

## Examples

``` r
get_jobs("bioc", "devel", "4.6.0", "3.23")
#> # A tibble: 12,809 × 252
#>    Package Version Date       Title        Description Author Maintainer License
#>    <chr>   <chr>   <chr>      <chr>        <chr>       <chr>  <chr>      <chr>  
#>  1 ABSSeq  1.65.0  NA         "ABSSeq: a … "Inferring… Wenta… Wentao Ya… GPL (>…
#>  2 ABSSeq  1.65.0  NA         "ABSSeq: a … "Inferring… Wenta… Wentao Ya… GPL (>…
#>  3 ABSSeq  1.65.0  NA         "ABSSeq: a … "Inferring… Wenta… Wentao Ya… GPL (>…
#>  4 ABSSeq  1.65.0  NA         "ABSSeq: a … "Inferring… Wenta… Wentao Ya… GPL (>…
#>  5 ABSSeq  1.65.0  NA         "ABSSeq: a … "Inferring… Wenta… Wentao Ya… GPL (>…
#>  6 ABarray 1.79.0  2006-02-11 "Microarray… "Automated… Yongm… Yongming … GPL    
#>  7 ABarray 1.79.0  2006-02-11 "Microarray… "Automated… Yongm… Yongming … GPL    
#>  8 ABarray 1.79.0  2006-02-11 "Microarray… "Automated… Yongm… Yongming … GPL    
#>  9 ABarray 1.79.0  2006-02-11 "Microarray… "Automated… Yongm… Yongming … GPL    
#> 10 ABarray 1.79.0  2006-02-11 "Microarray… "Automated… Yongm… Yongming … GPL    
#> # ℹ 12,799 more rows
#> # ℹ 244 more variables: VignetteBuilder <chr>, URL <chr>, biocViews <chr>,
#> #   Repository <chr>, `Date/Publication` <chr>, RemoteUrl <chr>,
#> #   RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>, MD5sum <chr>,
#> #   `_user` <chr>, `_type` <chr>, `_file` <chr>, `_fileid` <chr>,
#> #   `_filesize` <int>, `_sha256` <chr>, `_created` <chr>, `_published` <chr>,
#> #   `_jobs_job` <dbl>, `_jobs_time` <int>, `_jobs_config` <chr>, …
```
