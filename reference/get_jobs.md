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
#> # A tibble: 12,816 × 252
#>    Package Title  Version Author Maintainer Description URL   BugReports License
#>    <chr>   <chr>  <chr>   <chr>  <chr>      <chr>       <chr> <chr>      <chr>  
#>  1 ABSSeq  "ABSS… 1.65.0  Wenta… Wentao Ya… "Inferring… NA    NA         GPL (>…
#>  2 ABSSeq  "ABSS… 1.65.0  Wenta… Wentao Ya… "Inferring… NA    NA         GPL (>…
#>  3 ABSSeq  "ABSS… 1.65.0  Wenta… Wentao Ya… "Inferring… NA    NA         GPL (>…
#>  4 ABSSeq  "ABSS… 1.65.0  Wenta… Wentao Ya… "Inferring… NA    NA         GPL (>…
#>  5 ABSSeq  "ABSS… 1.65.0  Wenta… Wentao Ya… "Inferring… NA    NA         GPL (>…
#>  6 ABarray "Micr… 1.79.0  Yongm… Yongming … "Automated… NA    NA         GPL    
#>  7 ABarray "Micr… 1.79.0  Yongm… Yongming … "Automated… NA    NA         GPL    
#>  8 ABarray "Micr… 1.79.0  Yongm… Yongming … "Automated… NA    NA         GPL    
#>  9 ABarray "Micr… 1.79.0  Yongm… Yongming … "Automated… NA    NA         GPL    
#> 10 ABarray "Micr… 1.79.0  Yongm… Yongming … "Automated… NA    NA         GPL    
#> # ℹ 12,806 more rows
#> # ℹ 243 more variables: Encoding <chr>, Roxygen <chr>, RoxygenNote <chr>,
#> #   `Config/testthat/edition` <chr>, biocViews <chr>, VignetteBuilder <chr>,
#> #   SystemRequirements <chr>, OS_type <chr>,
#> #   `Config/Bioconductor/UnsupportedPlatforms` <chr>,
#> #   `Config/pak/sysreqs` <chr>, Repository <chr>, `Date/Publication` <chr>,
#> #   RemoteUrl <chr>, RemoteRef <chr>, RemoteSha <chr>, …
```
