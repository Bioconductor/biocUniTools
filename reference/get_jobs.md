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
#>    Type    Package Title           Version Date  `Authors@R` Description License
#>    <chr>   <chr>   <chr>           <chr>   <chr> <chr>       <chr>       <chr>  
#>  1 Package ABSSeq  "ABSSeq: a new… 1.65.0  NA    NA          "Inferring… GPL (>…
#>  2 Package ABSSeq  "ABSSeq: a new… 1.65.0  NA    NA          "Inferring… GPL (>…
#>  3 Package ABSSeq  "ABSSeq: a new… 1.65.0  NA    NA          "Inferring… GPL (>…
#>  4 Package ABSSeq  "ABSSeq: a new… 1.65.0  NA    NA          "Inferring… GPL (>…
#>  5 Package ABSSeq  "ABSSeq: a new… 1.65.0  NA    NA          "Inferring… GPL (>…
#>  6 NA      ABarray "Microarray QA… 1.79.0  2006… NA          "Automated… GPL    
#>  7 NA      ABarray "Microarray QA… 1.79.0  2006… NA          "Automated… GPL    
#>  8 NA      ABarray "Microarray QA… 1.79.0  2006… NA          "Automated… GPL    
#>  9 NA      ABarray "Microarray QA… 1.79.0  2006… NA          "Automated… GPL    
#> 10 NA      ABarray "Microarray QA… 1.79.0  2006… NA          "Automated… GPL    
#> # ℹ 12,870 more rows
#> # ℹ 244 more variables: VignetteBuilder <chr>, URL <chr>, BugReports <chr>,
#> #   biocViews <chr>, ByteCompile <chr>, DeploySubPath <chr>, Encoding <chr>,
#> #   LazyLoad <chr>, NeedsCompilation <chr>, RoxygenNote <chr>, Roxygen <chr>,
#> #   SwitchrLibrary <chr>, `Config/pak/sysreqs` <chr>, Repository <chr>,
#> #   `Date/Publication` <chr>, RemoteUrl <chr>, RemoteRef <chr>,
#> #   RemoteSha <chr>, Author <chr>, Maintainer <chr>, MD5sum <chr>, …
```
