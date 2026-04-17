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
#> # A tibble: 24,460 × 250
#>    Package  Version Date       `Authors@R`   Title Description License biocViews
#>    <chr>    <chr>   <chr>      <chr>         <chr> <chr>       <chr>   <chr>    
#>  1 scrapper 1.5.18  2026-04-17 "person(\"Aa… Bind… "Implement… MIT + … "Normali…
#>  2 scrapper 1.5.18  2026-04-17 "person(\"Aa… Bind… "Implement… MIT + … "Normali…
#>  3 scrapper 1.5.18  2026-04-17 "person(\"Aa… Bind… "Implement… MIT + … "Normali…
#>  4 scrapper 1.5.18  2026-04-17 "person(\"Aa… Bind… "Implement… MIT + … "Normali…
#>  5 scrapper 1.5.18  2026-04-17 "person(\"Aa… Bind… "Implement… MIT + … "Normali…
#>  6 scrapper 1.5.18  2026-04-17 "person(\"Aa… Bind… "Implement… MIT + … "Normali…
#>  7 scrapper 1.5.18  2026-04-17 "person(\"Aa… Bind… "Implement… MIT + … "Normali…
#>  8 scrapper 1.5.18  2026-04-17 "person(\"Aa… Bind… "Implement… MIT + … "Normali…
#>  9 scrapper 1.5.18  2026-04-17 "person(\"Aa… Bind… "Implement… MIT + … "Normali…
#> 10 scrapper 1.5.18  2026-04-17 "person(\"Aa… Bind… "Implement… MIT + … "Normali…
#> # ℹ 24,450 more rows
#> # ℹ 242 more variables: SystemRequirements <chr>, URL <chr>, BugReports <chr>,
#> #   VignetteBuilder <chr>, Encoding <chr>, RoxygenNote <chr>,
#> #   `Config/pak/sysreqs` <chr>, Repository <chr>, `Date/Publication` <chr>,
#> #   RemoteUrl <chr>, RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>,
#> #   Author <chr>, Maintainer <chr>, MD5sum <chr>, `_user` <chr>, `_type` <chr>,
#> #   `_file` <chr>, `_fileid` <chr>, `_filesize` <int>, `_sha256` <chr>, …
```
