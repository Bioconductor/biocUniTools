# Remove old binaries if a new binary exists

Remove old binaries if a new binary exists

## Usage

``` r
remove_old_binaries(
  repo_root,
  r_version,
  os,
  macosx_name = NA,
  arch = NA,
  test = TRUE
)
```

## Arguments

- repo_root:

  path that includes repo type–e.g., "/home/biocpush/PACKAGES/3.22/bioc"

- r_version:

  R x.y.z version

- os:

  name, full or abbreviation

- macosx_name:

  big-sur or sonoma

- arch:

  x86_64 or arm64

- test:

  logical (default TRUE) don't remove, only print packages marked for
  removal

## Value

vector of the full path of binaries removed

## Examples

``` r
remove_old_binaries("/home/biocpush/PACKAGES/3.22/bioc", "4.6.0", "windows")
#> Error in data.frame(file = files, latest = NA): arguments imply differing number of rows: 0, 1
```
