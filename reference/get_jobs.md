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
#> # A tibble: 12,806 × 252
#>    Package Type    Title           Version Date  `Authors@R` Description License
#>    <chr>   <chr>   <chr>           <chr>   <chr> <chr>       <chr>       <chr>  
#>  1 ABSSeq  Package "ABSSeq: a new… 1.65.0  NA    NA          "Inferring… GPL (>…
#>  2 ABSSeq  Package "ABSSeq: a new… 1.65.0  NA    NA          "Inferring… GPL (>…
#>  3 ABSSeq  Package "ABSSeq: a new… 1.65.0  NA    NA          "Inferring… GPL (>…
#>  4 ABSSeq  Package "ABSSeq: a new… 1.65.0  NA    NA          "Inferring… GPL (>…
#>  5 ABSSeq  Package "ABSSeq: a new… 1.65.0  NA    NA          "Inferring… GPL (>…
#>  6 ABarray NA      "Microarray QA… 1.79.0  2006… NA          "Automated… GPL    
#>  7 ABarray NA      "Microarray QA… 1.79.0  2006… NA          "Automated… GPL    
#>  8 ABarray NA      "Microarray QA… 1.79.0  2006… NA          "Automated… GPL    
#>  9 ABarray NA      "Microarray QA… 1.79.0  2006… NA          "Automated… GPL    
#> 10 ABarray NA      "Microarray QA… 1.79.0  2006… NA          "Automated… GPL    
#> # ℹ 12,796 more rows
#> # ℹ 244 more variables: VignetteBuilder <chr>, ByteCompile <chr>,
#> #   LazyData <chr>, URL <chr>, BugReports <chr>, biocViews <chr>,
#> #   `Config/pak/sysreqs` <chr>, Repository <chr>, `Date/Publication` <chr>,
#> #   RemoteUrl <chr>, RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>,
#> #   Author <chr>, Maintainer <chr>, MD5sum <chr>, `_user` <chr>, `_type` <chr>,
#> #   `_file` <chr>, `_fileid` <chr>, `_filesize` <int>, `_sha256` <chr>, …
```
