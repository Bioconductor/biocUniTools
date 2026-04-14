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
#> # A tibble: 12,730 × 252
#>    Package Type    Title  Version Date  `Authors@R` Description License LazyData
#>    <chr>   <chr>   <chr>  <chr>   <chr> <chr>       <chr>       <chr>   <chr>   
#>  1 ABSSeq  Package "ABSS… 1.65.0  NA    NA          "Inferring… GPL (>… NA      
#>  2 ABSSeq  Package "ABSS… 1.65.0  NA    NA          "Inferring… GPL (>… NA      
#>  3 ABSSeq  Package "ABSS… 1.65.0  NA    NA          "Inferring… GPL (>… NA      
#>  4 ABSSeq  Package "ABSS… 1.65.0  NA    NA          "Inferring… GPL (>… NA      
#>  5 ABSSeq  Package "ABSS… 1.65.0  NA    NA          "Inferring… GPL (>… NA      
#>  6 ABarray NA      "Micr… 1.79.0  2006… NA          "Automated… GPL     NA      
#>  7 ABarray NA      "Micr… 1.79.0  2006… NA          "Automated… GPL     NA      
#>  8 ABarray NA      "Micr… 1.79.0  2006… NA          "Automated… GPL     NA      
#>  9 ABarray NA      "Micr… 1.79.0  2006… NA          "Automated… GPL     NA      
#> 10 ABarray NA      "Micr… 1.79.0  2006… NA          "Automated… GPL     NA      
#> # ℹ 12,720 more rows
#> # ℹ 243 more variables: VignetteBuilder <chr>, SystemRequirements <chr>,
#> #   ByteCompile <chr>, biocViews <chr>, URL <chr>, `Config/pak/sysreqs` <chr>,
#> #   Repository <chr>, `Date/Publication` <chr>, RemoteUrl <chr>,
#> #   RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>, Author <chr>,
#> #   Maintainer <chr>, MD5sum <chr>, `_user` <chr>, `_type` <chr>,
#> #   `_file` <chr>, `_fileid` <chr>, `_filesize` <int>, `_sha256` <chr>, …
```
