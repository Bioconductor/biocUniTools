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
#> # A tibble: 12,760 × 252
#>    Package Version Date       `Authors@R` Title    Description License biocViews
#>    <chr>   <chr>   <chr>      <chr>       <chr>    <chr>       <chr>   <chr>    
#>  1 ABSSeq  1.65.0  NA         NA          "ABSSeq… "Inferring… GPL (>… Differen…
#>  2 ABSSeq  1.65.0  NA         NA          "ABSSeq… "Inferring… GPL (>… Differen…
#>  3 ABSSeq  1.65.0  NA         NA          "ABSSeq… "Inferring… GPL (>… Differen…
#>  4 ABSSeq  1.65.0  NA         NA          "ABSSeq… "Inferring… GPL (>… Differen…
#>  5 ABSSeq  1.65.0  NA         NA          "ABSSeq… "Inferring… GPL (>… Differen…
#>  6 ABarray 1.79.0  2006-02-11 NA          "Microa… "Automated… GPL     Microarr…
#>  7 ABarray 1.79.0  2006-02-11 NA          "Microa… "Automated… GPL     Microarr…
#>  8 ABarray 1.79.0  2006-02-11 NA          "Microa… "Automated… GPL     Microarr…
#>  9 ABarray 1.79.0  2006-02-11 NA          "Microa… "Automated… GPL     Microarr…
#> 10 ABarray 1.79.0  2006-02-11 NA          "Microa… "Automated… GPL     Microarr…
#> # ℹ 12,750 more rows
#> # ℹ 244 more variables: SystemRequirements <chr>, URL <chr>, BugReports <chr>,
#> #   VignetteBuilder <chr>, Encoding <chr>, RoxygenNote <chr>,
#> #   `Config/pak/sysreqs` <chr>, Repository <chr>, `Date/Publication` <chr>,
#> #   RemoteUrl <chr>, RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>,
#> #   Author <chr>, Maintainer <chr>, MD5sum <chr>, `_user` <chr>, `_type` <chr>,
#> #   `_file` <chr>, `_fileid` <chr>, `_filesize` <int>, `_sha256` <chr>, …
```
