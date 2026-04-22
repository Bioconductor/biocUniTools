# biocUniTools

biocUniTools aims to make it easier to work with Bioconductor package
data in R Universe. It can retrieve raw data for a universe or attempt
to match jobs to binaries by OS and arch. The package also contains
small helper functions to manage repository clean up when maintaining a
package repository that’s replenished with new binaries from R Universe.

## R Universe API

Check a package like [lefser](https://bioc.r-universe.dev/lefser) in R
Universe. The R Universe API has the same data available
programmatically at https://bioc.r-universe.dev/api/packages/lefser.

Data comes from the `DESCRIPTION` file and GitHub. In particular, we’re
interested in the `_jobs`, `_binaries`, and `_buildurl` when considering
whether to propagate a binary created in R Universe. Notice it has jobs
for `bioc-checks`, builds on multiple versions of R, and the
`_jobs_config` doesn’t exactly match a `_binaries`’s `os`.

## lefser in Bioconductor 3.23 via the R Universe API

This is a truncated version of parts of the API.

``` json
{
  "_id": "69cf91f20c54e422bf1d50de",
  "Package": "lefser",
  "Type": "Package",
  "Title": "R implementation of the LEfSE method for microbiome biomarker\ndiscovery",
  "_jobs": [
    {
      "job": 69829927166,
      "time": 244,
      "config": "bioc-checks",
      "r": "4.5.3",
      "check": "NOTE",
      "artifact": "6257774022"
    },
    {
      "job": 69829927169,
      "time": 379,
      "config": "linux-devel-x86_64",
      "r": "4.6.0",
      "check": "OK",
      "artifact": "6257796829"
    },
    {
      "job": 69829927167,
      "time": 449,
      "config": "linux-release-x86_64",
      "r": "4.5.3",
      "check": "OK",
      "artifact": "6257808198"
    },
    {
      "job": 69829927184,
      "time": 252,
      "config": "macos-oldrel-arm64",
      "r": "4.5.3",
      "check": "ERROR",
      "artifact": "6258002602"
    },
    {
      "job": 69829927171,
      "time": 282,
      "config": "macos-release-arm64",
      "r": "4.6.0",
      "check": "ERROR",
      "artifact": "6258007467"
    },
    {
      "job": 69829465206,
      "time": 327,
      "config": "source",
      "r": "4.5.3",
      "check": "OK",
      "artifact": "6257731498"
    },
    {
      "job": 69829927173,
      "time": 230,
      "config": "wasm-release",
      "r": "4.5.1",
      "check": "OK",
      "artifact": "6257771337"
    },
    {
      "job": 69829927228,
      "time": 615,
      "config": "windows-devel",
      "r": "4.7.0",
      "check": "ERROR",
      "artifact": "6257837183"
    },
    {
      "job": 69829927188,
      "time": 517,
      "config": "windows-oldrel",
      "r": "4.5.3",
      "check": "ERROR",
      "artifact": "6257820249"
    },
    {
      "job": 69829927225,
      "time": 643,
      "config": "windows-release",
      "r": "4.6.0",
      "check": "ERROR",
      "artifact": "6257841770"
    }
  ],
  "_bioccheck": {
    "error": 0,
    "warning": 0,
    "note": 8
  },
  "_buildurl": "https://github.com/r-universe/bioc/actions/runs/23941765469",
  "_binaries": [
    {
      "r": "4.6.0",
      "os": "linux",
      "version": "1.21.7",
      "date": "2026-04-03T09:45:48.000Z",
      "distro": "noble",
      "commit": "333fd2411085100ddc70fdbffbf1cbebb735df8d",
      "fileid": "0523834cf0d6f2942ec27c4cebd0ce8fd20f317967eb7c69672abf0642a95426",
      "status": "success",
      "check": "OK",
      "buildurl": "https://github.com/r-universe/bioc/actions/runs/23941765469"
    },
    {
      "r": "4.5.3",
      "os": "linux",
      "version": "1.21.7",
      "date": "2026-04-03T09:46:50.000Z",
      "distro": "noble",
      "commit": "333fd2411085100ddc70fdbffbf1cbebb735df8d",
      "fileid": "2906aa666beaae1e887c356eb68fcf5cca2eabff42aefea05399215fade84680",
      "status": "success",
      "check": "OK",
      "buildurl": "https://github.com/r-universe/bioc/actions/runs/23941765469"
    },
    {
      "r": "4.5.3",
      "os": "mac",
      "version": "1.21.7",
      "date": "2026-04-03T10:06:49.000Z",
      "commit": "333fd2411085100ddc70fdbffbf1cbebb735df8d",
      "fileid": "d9e157f9bb0160ad67bbb54a911a6c921e7c4e1f25c856afd6554571603d4991",
      "status": "failure",
      "check": "ERROR",
      "buildurl": "https://github.com/r-universe/bioc/actions/runs/23941765469"
    },
    {
      "r": "4.6.0",
      "os": "mac",
      "version": "1.21.7",
      "date": "2026-04-03T10:07:02.000Z",
      "commit": "333fd2411085100ddc70fdbffbf1cbebb735df8d",
      "fileid": "e4083909ae44d1d8317c445f939e547b5f1d314bb0d0e68982ae31eaee9e4fbe",
      "status": "failure",
      "check": "ERROR",
      "buildurl": "https://github.com/r-universe/bioc/actions/runs/23941765469"
    }
  ]
}
```

Binaries that work on any arch of an OS are universal binaries.
Universal binaries are built on the x86_64 linux whereas for macosx,
it’s built on arm64. However, if your package must compile C code, it
needs a binary that’s arch-specific so the R Universe API will include
additional fields for these binaries. This information is important to
construct the repository path. Note: the `arch` field in `rtracklayer`’s
`_binaries` for macosx.

``` json
}
  "_binaries": [
    {
      "r": "4.6.0",
      "os": "mac",
      "version": "1.71.3",
      "date": "2026-03-16T06:33:54.000Z",
      "arch": "aarch64",
      "commit": "d9b23f963c621e4aa8e5e52c6e4da50d5d213e1a",
      "fileid": "ac6b65936a87f30994737a041f03c22f0d85c82b0a22b3a31b3e01c98ab3d5e1",
      "status": "success",
      "check": "WARNING",
      "buildurl": "https://github.com/r-universe/bioc/actions/runs/23130760393"
    }
  ]
}
```

## Using biocUniTools

Let’s imagine we want to create a repository of R Universe binaries for
macosx arm64. We’ll use `biocUniTools` to align `_jobs` and `_binaries`
and filter for candidate packages that match our criteria and show to
clean up our repository for future maintenance.

Get corresponding R Universe information for a Bioconductor branch.

``` r
bioc_info <- get_uni_for_bioc_version("3.23")
bioc_info
```

    $bioc_version
    [1] "3.23"

    $bioc_branch
    [1] "devel"

    $ru_uni
    [1] "bioc"

    $r_version
    [1] "4.6.0"

If we wanted to create a report of each package’s status, we could use
`get_jobs`, which attempts to align jobs and binaries where possible.
This function creates a few extra columns to help sort differences in
these data points, a `JobUrl`to the GitHub Action job, and `Artifact`,
the url for the binary in R Universe.

``` r
jobs <- get_jobs(bioc_info$ru_uni, bioc_info$bioc_branch, bioc_info$r_version,
                 bioc_info$bioc_version)

jobs |>
    dplyr::filter(Package %in% c("lefser", "rtracklayer")) |>
    dplyr::select(Package, `_jobs_r_xy`, `_jobs_type`, `_jobs_arch`,
                  `_binaries_arch`, `_jobs_os_`, `_binaries_os_`,
                  `_jobs_check`, `_binaries_check`)
```

    # A tibble: 12 × 9
       Package   `_jobs_r_xy` `_jobs_type` `_jobs_arch` `_binaries_arch` `_jobs_os_`
       <chr>     <chr>        <chr>        <chr>        <chr>            <chr>
     1 lefser    4.6          binary       x86_64       <NA>             linux
     2 lefser    4.6          binary       arm64        <NA>             mac
     3 lefser    4.6          binary       x86_64       <NA>             win
     4 lefser    4.5          bioc-checks  <NA>         <NA>             linux
     5 lefser    4.5          source       <NA>         <NA>             linux
     6 rtrackla… 4.6          binary       arm64        aarch64          linux
     7 rtrackla… 4.6          binary       x86_64       x86_64           linux
     8 rtrackla… 4.6          binary       arm64        aarch64          mac
     9 rtrackla… 4.6          binary       x86_64       x86_64           mac
    10 rtrackla… 4.6          binary       x86_64       x86_64           win
    11 rtrackla… 4.5          bioc-checks  <NA>         <NA>             linux
    12 rtrackla… 4.5          source       <NA>         <NA>             linux
    # ℹ 3 more variables: `_binaries_os_` <chr>, `_jobs_check` <chr>,
    #   `_binaries_check` <chr>

Use `get_candidates` to find candidates binaries if their vignettes have
passed check and the commit is the same in the BBS. We could use
[`curl::multi_download`](https://jeroen.r-universe.dev/curl/reference/multi_download.html)
to download all binaries into our repository. ::: column-screen-inset

``` r
candidates <- get_candidates(bioc_info$ru_uni, bioc_info$bioc_branch,
                             bioc_info$r_version, bioc_info$bioc_version,
                             "macosx", "arm64", vignettes = TRUE,
                             commit = TRUE)
candidates |>
    dplyr::filter(Package %in% c("lefser", "rtracklayer")) |>
    dplyr::select(Package, `_jobs_r_xy`, `_jobs_type`, `_jobs_arch`,
                  `_jobs_os_`, `_jobs_check`, `_binaries_check`, JobUrl,
                  Artifact)
```

          Package _jobs_r_xy _jobs_type _jobs_arch _jobs_os_ _jobs_check
    1 rtracklayer        4.6     binary      arm64       mac     WARNING
      _binaries_check
    1         WARNING
                                                                           JobUrl
    1 https://github.com/r-universe/bioc/actions/runs/24439351234/job/71401219263
                                                                                    Artifact
    1 https://bioc.r-universe.dev/bin/macosx/sonoma-arm64/contrib/4.6/rtracklayer_1.71.3.tgz

:::

`biocUniTools` also has `remove_old_binaries` to clean up older versions
of binaries after updating the repository with R Universe binaries. See
`inst/scripts/harvest.R` for a more complete example.

``` r
os <- "macosx"
arch <- "arm64"
repo_root <- "~/tmp/PACKAGES/3.23/bioc"
# The R 4.6 macosx path uses sonoma for arm64
repo_path <- get_repository_path(repo_root, bioc_info$r_version, os, "sonoma",
                                 arch)
downloaded <- curl::multi_download(candidates$Artifact, destfiles = repo_path)
remove_old_binaries(repo_root, bioc_info$r_version, os, arch)
```

``` r
sessionInfo()
```

    R version 4.5.3 (2026-03-11)
    Platform: x86_64-pc-linux-gnu
    Running under: Ubuntu 24.04.4 LTS

    Matrix products: default
    BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3
    LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0

    locale:
     [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8
     [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8
     [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C
    [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C

    time zone: UTC
    tzcode source: system (glibc)

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base

    other attached packages:
    [1] reactable_0.4.5     biocUniTools_0.0.99 dplyr_1.2.1

    loaded via a namespace (and not attached):
     [1] xfun_0.57           httr2_1.2.2         htmlwidgets_1.6.4
     [4] gh_1.5.0            Biobase_2.70.0      tzdb_0.5.0
     [7] vctrs_0.7.3         tools_4.5.3         bitops_1.0-9
    [10] generics_0.1.4      stats4_4.5.3        curl_7.1.0
    [13] RUnit_0.4.33.1      tibble_3.3.1        RSQLite_2.4.6
    [16] blob_1.3.0          pkgconfig_2.0.3     dbplyr_2.5.2
    [19] graph_1.88.1        lifecycle_1.0.5     compiler_4.5.3
    [22] stringr_1.6.0       biocViews_1.78.2    htmltools_0.5.9
    [25] RCurl_1.98-1.18     yaml_2.3.12         pillar_1.11.1
    [28] tidyr_1.3.2         DT_0.34.0           cachem_1.1.0
    [31] rvest_1.0.5         tidyselect_1.2.1    digest_0.6.39
    [34] stringi_1.8.7       purrr_1.2.2         fastmap_1.2.0
    [37] cli_3.6.6           magrittr_2.0.5      RBGL_1.86.0
    [40] XML_3.99-0.23       utf8_1.2.6          readr_2.2.0
    [43] withr_3.0.2         filelock_1.0.3      rappdirs_0.3.4
    [46] bit64_4.8.0         lubridate_1.9.5     timechange_0.4.0
    [49] rmarkdown_2.31      httr_1.4.8          igraph_2.3.0
    [52] bit_4.6.0           otel_0.2.0          hms_1.1.4
    [55] memoise_2.0.1       evaluate_1.0.5      knitr_1.51
    [58] BiocFileCache_3.0.0 rlang_1.2.0         BiocPkgTools_1.28.3
    [61] glue_1.8.1          DBI_1.3.0           BiocManager_1.30.27
    [64] xml2_1.5.2          BiocGenerics_0.56.0 jsonlite_2.0.0
    [67] R6_2.6.1           
