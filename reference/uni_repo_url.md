# Get universe URL for an os and R version

Get universe URL for an os and R version

## Usage

``` r
uni_repo_url(uni, r_version, os, macosx_name = NA, arch = NA)
```

## Arguments

- uni:

  character universe name

- r_version:

  character

- os:

  character

- macosx_name:

  big-sur or sonoma

- arch:

  character x86_64 or arm64

## Value

character

## Examples

``` r
uni_repo_url("bioc", "4.5.3", "windows")
#> [1] "https://bioc.r-universe.dev/bin/windows/contrib/4.5"
```
