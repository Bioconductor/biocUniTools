# Get macosx repo subpath by R version

Get macosx repo subpath by R version

## Usage

``` r
get_macosx_subpath(r_version, arch)
```

## Arguments

- r_version:

  R X.Y character

- arch:

  character x86_64 or arm64

## Value

character

## Examples

``` r
get_macosx_subpath("4.6", "arm64")
#> [1] "sonoma-arm64"

```
