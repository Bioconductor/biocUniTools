# Construct binary file name

Construct binary file name

## Usage

``` r
uni_pkg_file(pkg, os, version)
```

## Arguments

- pkg:

  character package name

- os:

  character OS name

- version:

  character package version

## Value

character file with extension

## Examples

``` r
uni_pkg_file("bedbaser", "windows", "1.0.12")
#> [1] "bedbaser_1.0.12.zip"
```
