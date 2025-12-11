test_that("r_xy_ver gives x.y", {
    expect_equal(r_xy_ver("4.6.3"), "4.6")
    expect_equal(r_xy_ver("40.623.33333"), "40.623")
})

test_that("is_greater_than returns TRUE if LHS version greater", {
    expect_true(!is_greater_than("pkg_10.2.4.tar.gz",
                                 "pkg_12.3.0.tar.gz"))
    expect_true(is_greater_than("pkg_2.4.4.tar.gz",
                                "pkg_2.3.0.tar.gz"))
    expect_true(is_greater_than("pkg_2.4.4.tgz",
                                "pkg_2.3.0.tgz"))
})

test_that("uni_pkg_file makes correct file format", {
    expect_equal(uni_pkg_file("pkg", "windows", "1.2.3"),
                 "pkg_1.2.3.zip")
    expect_equal(uni_pkg_file("pkg", "macosx", "1.2.3"),
                 "pkg_1.2.3.tgz")
    expect_equal(uni_pkg_file("pkg", "linux", "1.2.3"),
                 "pkg_1.2.3.tar.gz")
})

test_that("uni_repo_url gets the universe url", {
    macx86 <- "https://bioc.r-universe.dev/bin/macosx/big-sur-x86_64/contrib/4.6/"
    macx64 <- "https://bioc.r-universe.dev/bin/macosx/big-sur-arm64/contrib/4.6/"
    win <- "https://bioc.r-universe.dev/bin/windows/contrib/4.6/"
    expect_equal(uni_repo_url("bioc", "4.6.0", "macosx", "x86_64"), macx86)
    expect_equal(uni_repo_url("bioc", "4.6.0", "macosx", "arm64"), macx64)
    expect_equal(uni_repo_url("bioc", "4.6.0", "windows"), win)
})


test_that("get_binary_os gets abbreviation", {
    expect_equal(get_binary_os("windows"), "win")
    expect_equal(get_binary_os("macosx"), "mac")
})

test_that("get_uni_pkgs gets R Universe data", {
    # We'll check the path of file for windows
    pkgs <- get_uni_pkgs("bioc", "devel", "4.6.0", "3.23", "windows")
    pkg <- pkgs |>
        dplyr::filter(Package == "UCSC.utils")
    version <- pkg |>
        dplyr::pull(Version)
    url <- paste0(uni_repo_url("bioc", "4.6.0", "windows"),
                  uni_pkg_file("UCSC.utils", "windows", version))
    expect_equal(pkg |> dplyr::pull(Url), url)

    # We'll check the path of a file for mac x86_64
    pkgs <- get_uni_pkgs("bioc-release", "release", "4.5.0", "3.22", "macosx",
                         "x86_64")
    pkg <- pkgs |>
        dplyr::filter(Package == "Biobase")
    version <- pkg |>
        dplyr::pull(Version)
    url <- paste0(uni_repo_url("bioc-release", "4.5.0", "macosx", "x86_64"),
                  uni_pkg_file("Biobase", "macosx", version))
    expect_equal(pkg |> dplyr::pull(Url), url)
})

test_that("get_uni_for_bioc_version gets correct universe", {
    bioc_ru_info <- get_uni_for_bioc_version("3.22")
    expect_equal(bioc_ru_info$r_version, "4.5.0")
    expect_equal(bioc_ru_info$bioc_branch, "release")
    expect_equal(bioc_ru_info$ru_uni, "bioc-release")
})
