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
#> # A tibble: 24,463 × 250
#>    Package        Title Version `Authors@R` Description License URL   BugReports
#>    <chr>          <chr> <chr>   <chr>       <chr>       <chr>   <chr> <chr>     
#>  1 tidyexposomics Inte… 0.99.15 "c(\nperso… "The tidye… MIT + … http… https://g…
#>  2 tidyexposomics Inte… 0.99.15 "c(\nperso… "The tidye… MIT + … http… https://g…
#>  3 tidyexposomics Inte… 0.99.15 "c(\nperso… "The tidye… MIT + … http… https://g…
#>  4 tidyexposomics Inte… 0.99.15 "c(\nperso… "The tidye… MIT + … http… https://g…
#>  5 tidyexposomics Inte… 0.99.15 "c(\nperso… "The tidye… MIT + … http… https://g…
#>  6 tidyexposomics Inte… 0.99.15 "c(\nperso… "The tidye… MIT + … http… https://g…
#>  7 tidyexposomics Inte… 0.99.15 "c(\nperso… "The tidye… MIT + … http… https://g…
#>  8 tidyexposomics Inte… 0.99.15 "c(\nperso… "The tidye… MIT + … http… https://g…
#>  9 betterChromVAR Impr… 0.99.37 "person(\"… "A much fa… GPL (>… http… https://g…
#> 10 betterChromVAR Impr… 0.99.37 "person(\"… "A much fa… GPL (>… http… https://g…
#> # ℹ 24,453 more rows
#> # ℹ 242 more variables: Encoding <chr>, Roxygen <chr>, RoxygenNote <chr>,
#> #   VignetteBuilder <chr>, biocViews <chr>, `Config/testthat/edition` <chr>,
#> #   `Config/pak/sysreqs` <chr>, Repository <chr>, `Date/Publication` <chr>,
#> #   RemoteUrl <chr>, RemoteRef <chr>, RemoteSha <chr>, NeedsCompilation <chr>,
#> #   Author <chr>, Maintainer <chr>, MD5sum <chr>, `_user` <chr>, `_type` <chr>,
#> #   `_file` <chr>, `_fileid` <chr>, `_filesize` <int>, `_sha256` <chr>, …
```
