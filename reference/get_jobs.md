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
#> # A tibble: 12,880 × 252
#>    Package Version Date   Title `Authors@R` Description License URL   BugReports
#>    <chr>   <chr>   <chr>  <chr> <chr>       <chr>       <chr>   <chr> <chr>     
#>  1 ABSSeq  1.65.0  NA     "ABS… NA          "Inferring… GPL (>… NA    NA        
#>  2 ABSSeq  1.65.0  NA     "ABS… NA          "Inferring… GPL (>… NA    NA        
#>  3 ABSSeq  1.65.0  NA     "ABS… NA          "Inferring… GPL (>… NA    NA        
#>  4 ABSSeq  1.65.0  NA     "ABS… NA          "Inferring… GPL (>… NA    NA        
#>  5 ABSSeq  1.65.0  NA     "ABS… NA          "Inferring… GPL (>… NA    NA        
#>  6 ABarray 1.79.0  2006-… "Mic… NA          "Automated… GPL     NA    NA        
#>  7 ABarray 1.79.0  2006-… "Mic… NA          "Automated… GPL     NA    NA        
#>  8 ABarray 1.79.0  2006-… "Mic… NA          "Automated… GPL     NA    NA        
#>  9 ABarray 1.79.0  2006-… "Mic… NA          "Automated… GPL     NA    NA        
#> 10 ABarray 1.79.0  2006-… "Mic… NA          "Automated… GPL     NA    NA        
#> # ℹ 12,870 more rows
#> # ℹ 243 more variables: Collate <chr>, VignetteBuilder <chr>, biocViews <chr>,
#> #   `Config/pak/sysreqs` <chr>, Repository <chr>, `Date/Publication` <chr>,
#> #   RemoteUrl <chr>, RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>,
#> #   Author <chr>, Maintainer <chr>, MD5sum <chr>, `_user` <chr>, `_type` <chr>,
#> #   `_file` <chr>, `_fileid` <chr>, `_filesize` <int>, `_sha256` <chr>,
#> #   `_created` <chr>, `_published` <chr>, `_jobs_job` <dbl>, …
```
