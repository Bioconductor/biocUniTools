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
#> # A tibble: 25,753 × 250
#>    Type    Package Title           Version Date  `Authors@R` Description License
#>    <chr>   <chr>   <chr>           <chr>   <chr> <chr>       <chr>       <chr>  
#>  1 Package gDRcore "Processing fu… 1.9.8   2026… "c(\nperso… "This pack… Artist…
#>  2 Package gDRcore "Processing fu… 1.9.8   2026… "c(\nperso… "This pack… Artist…
#>  3 Package gDRcore "Processing fu… 1.9.8   2026… "c(\nperso… "This pack… Artist…
#>  4 Package gDRcore "Processing fu… 1.9.8   2026… "c(\nperso… "This pack… Artist…
#>  5 Package gDRcore "Processing fu… 1.9.8   2026… "c(\nperso… "This pack… Artist…
#>  6 Package gDRcore "Processing fu… 1.9.8   2026… "c(\nperso… "This pack… Artist…
#>  7 Package gDRcore "Processing fu… 1.9.8   2026… "c(\nperso… "This pack… Artist…
#>  8 Package gDRcore "Processing fu… 1.9.8   2026… "c(\nperso… "This pack… Artist…
#>  9 Package gDRcore "Processing fu… 1.9.8   2026… "c(\nperso… "This pack… Artist…
#> 10 Package gDRcore "Processing fu… 1.9.8   2026… "c(\nperso… "This pack… Artist…
#> # ℹ 25,743 more rows
#> # ℹ 242 more variables: VignetteBuilder <chr>, URL <chr>, BugReports <chr>,
#> #   biocViews <chr>, ByteCompile <chr>, DeploySubPath <chr>, Encoding <chr>,
#> #   LazyLoad <chr>, NeedsCompilation <chr>, RoxygenNote <chr>, Roxygen <chr>,
#> #   SwitchrLibrary <chr>, `Config/pak/sysreqs` <chr>, Repository <chr>,
#> #   `Date/Publication` <chr>, RemoteUrl <chr>, RemoteRef <chr>,
#> #   RemoteSha <chr>, Author <chr>, Maintainer <chr>, MD5sum <chr>, …
```
