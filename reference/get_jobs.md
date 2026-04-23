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
#> # A tibble: 12,804 × 252
#>    Package Type    Title       Version `Authors@R` Description License biocViews
#>    <chr>   <chr>   <chr>       <chr>   <chr>       <chr>       <chr>   <chr>    
#>  1 ABSSeq  Package "ABSSeq: a… 1.65.0  NA          "Inferring… GPL (>… Differen…
#>  2 ABSSeq  Package "ABSSeq: a… 1.65.0  NA          "Inferring… GPL (>… Differen…
#>  3 ABSSeq  Package "ABSSeq: a… 1.65.0  NA          "Inferring… GPL (>… Differen…
#>  4 ABSSeq  Package "ABSSeq: a… 1.65.0  NA          "Inferring… GPL (>… Differen…
#>  5 ABSSeq  Package "ABSSeq: a… 1.65.0  NA          "Inferring… GPL (>… Differen…
#>  6 ABarray NA      "Microarra… 1.79.0  NA          "Automated… GPL     Microarr…
#>  7 ABarray NA      "Microarra… 1.79.0  NA          "Automated… GPL     Microarr…
#>  8 ABarray NA      "Microarra… 1.79.0  NA          "Automated… GPL     Microarr…
#>  9 ABarray NA      "Microarra… 1.79.0  NA          "Automated… GPL     Microarr…
#> 10 ABarray NA      "Microarra… 1.79.0  NA          "Automated… GPL     Microarr…
#> # ℹ 12,794 more rows
#> # ℹ 244 more variables: Encoding <chr>, RoxygenNote <chr>,
#> #   VignetteBuilder <chr>, URL <chr>, BugReports <chr>,
#> #   `Config/testthat/edition` <chr>, `Config/pak/sysreqs` <chr>,
#> #   Repository <chr>, `Date/Publication` <chr>, RemoteUrl <chr>,
#> #   RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>, Author <chr>,
#> #   Maintainer <chr>, MD5sum <chr>, `_user` <chr>, `_type` <chr>, …
```
