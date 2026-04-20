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
#> # A tibble: 12,810 × 252
#>    Package Type  Title Version `Authors@R` Description License Encoding LazyData
#>    <chr>   <chr> <chr> <chr>   <chr>       <chr>       <chr>   <chr>    <chr>   
#>  1 ABSSeq  Pack… "ABS… 1.65.0  NA          "Inferring… GPL (>… NA       NA      
#>  2 ABSSeq  Pack… "ABS… 1.65.0  NA          "Inferring… GPL (>… NA       NA      
#>  3 ABSSeq  Pack… "ABS… 1.65.0  NA          "Inferring… GPL (>… NA       NA      
#>  4 ABSSeq  Pack… "ABS… 1.65.0  NA          "Inferring… GPL (>… NA       NA      
#>  5 ABSSeq  Pack… "ABS… 1.65.0  NA          "Inferring… GPL (>… NA       NA      
#>  6 ABarray NA    "Mic… 1.79.0  NA          "Automated… GPL     NA       NA      
#>  7 ABarray NA    "Mic… 1.79.0  NA          "Automated… GPL     NA       NA      
#>  8 ABarray NA    "Mic… 1.79.0  NA          "Automated… GPL     NA       NA      
#>  9 ABarray NA    "Mic… 1.79.0  NA          "Automated… GPL     NA       NA      
#> 10 ABarray NA    "Mic… 1.79.0  NA          "Automated… GPL     NA       NA      
#> # ℹ 12,800 more rows
#> # ℹ 243 more variables: RoxygenNote <chr>, URL <chr>, BugReports <chr>,
#> #   VignetteBuilder <chr>, biocViews <chr>, `Config/pak/sysreqs` <chr>,
#> #   Repository <chr>, `Date/Publication` <chr>, RemoteUrl <chr>,
#> #   RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>, Author <chr>,
#> #   Maintainer <chr>, MD5sum <chr>, `_user` <chr>, `_type` <chr>,
#> #   `_file` <chr>, `_fileid` <chr>, `_filesize` <int>, `_sha256` <chr>, …
```
