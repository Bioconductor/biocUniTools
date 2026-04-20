# Filter by arch if it has arch-specific and universal binaries

Some builds can have arch-specific and universal binaries. This function
filters specifically for data observed in R Universe, which could change
in the future.

- mac universal binaries: `_jobs_arch` == 'arm64' &
  !is.na(`_binaries_arch`)

- linux universal binaries: `_jobs_arch` == 'x86_64' &
  !is.na(`_binaries_arch`)

## Usage

``` r
filter_by_arch(df, os, arch = NA)
```

## Arguments

- df:

  data.frame from get_jobs()

- os:

  character

- arch:

  character

## Value

data.frame

## Examples

``` r
df <- get_jobs("bioc", "devel", "4.6.0", "3.23", "macosx", "arm64")
#> Error in get_jobs("bioc", "devel", "4.6.0", "3.23", "macosx", "arm64"): unused arguments ("macosx", "arm64")
filter_by_arch(df, "macosx", "arm64")
#> function (x, df1, df2, ncp, log = FALSE) 
#> {
#>     if (missing(ncp)) 
#>         .Call(C_df, x, df1, df2, log)
#>     else .Call(C_dnf, x, df1, df2, ncp, log)
#> }
#> <bytecode: 0x5591e7014820>
#> <environment: namespace:stats>
```
