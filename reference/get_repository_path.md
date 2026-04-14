# Get repo path

Get repo path

## Usage

``` r
get_repository_path(repo_root, r_version, os, macosx_name = NA, arch = NA)
```

## Arguments

- r_version:

  character

- os:

  character

- macosx_name:

  big-sur or sonoma

- arch:

  character x86_64 or arm64

- bioc_version:

  character

## Value

character

## Examples

``` r
get_repository_path("/home/biocpush/PACKAGES/3.22/bioc", "4.6.0", "windows")
#> [1] "/home/biocpush/PACKAGES/3.22/bioc/bin/windows/contrib/4.6"
```
