 test_that("r_xy_ver gives x.y", {
     expect_equal(r_xy_ver("4.6.3"), "4.6")
     expect_equal(r_xy_ver("40.623.33333"), "40.623")
 })
 
 test_that("is_greater_than returns TRUE if LHS version greater", {
     expect_true(!is_greater_than("10.2.4", "12.3.0"))
     expect_true(is_greater_than("2.4.4", "2.3.0"))
     expect_true(is_greater_than("2.3.4", "2.3.0"))
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
     macx86 <- "https://bioc.r-universe.dev/bin/macosx/sonoma-x86_64/contrib/4.6"
     macx64 <- "https://bioc.r-universe.dev/bin/macosx/big-sur-arm64/contrib/4.6"
     win <- "https://bioc.r-universe.dev/bin/windows/contrib/4.6"
     expect_equal(uni_repo_url("bioc", "4.6.0", "macosx", "sonoma", "x86_64"), macx86)
     expect_equal(uni_repo_url("bioc", "4.6.0", "macosx", "big-sur", "arm64"), macx64)
     expect_equal(uni_repo_url("bioc", "4.6.0", "windows"), win)
 })
 
 
 test_that("get_binary_os gets abbreviation", {
     expect_equal(get_binary_os("windows"), "win")
     expect_equal(get_binary_os("macosx"), "mac")
 })
 
 test_that("get_comparable_pkgs gets R Universe data", {
     # We'll check the path of file for windows
     pkgs <- get_comparable_pkgs("bioc", "devel", "4.6.0", "3.23", "windows")
     pkg <- pkgs |>
         dplyr::filter(Package == "UCSC.utils")
     version <- pkg |>
         dplyr::pull(Version)
     url <- file.path(uni_repo_url("bioc", "4.6.0", "windows"),
                      uni_pkg_file("UCSC.utils", "windows", version))
     expect_equal(pkg |> dplyr::pull(Url), url)
 
     # We'll check the path of a file for mac x86_64
     pkgs <- get_comparable_pkgs("bioc-release", "release", "4.5.0", "3.22",
                                 "macosx", "big-sur", "x86_64")
     pkg <- pkgs |>
         dplyr::filter(Package == "Biobase")
     version <- pkg |>
         dplyr::pull(Version)
     url <- file.path(uni_repo_url("bioc-release", "4.5.0", "macosx", "big-sur",
                                   "x86_64"),
                      uni_pkg_file("Biobase", "macosx", version))
     expect_equal(pkg |> dplyr::pull(Url), url)
 })
 
 test_that("get_uni_for_bioc_version gets correct universe", {
     bioc_ru_info <- get_uni_for_bioc_version("3.22")
     expect_equal(bioc_ru_info$r_version, "4.5.0")
     expect_equal(bioc_ru_info$bioc_branch, "release")
     expect_equal(bioc_ru_info$ru_uni, "bioc-release")
 })

test_that("remove_binaries only removes older binaries", {
    # Set up test repo
    repo <- tempdir()
    repo_path <- get_repository_path(repo, "4.6.0", "linux")
    pkgs <- c("mytestpkg_1.2.3.tar.gz",
              "mytestpkg_1.3.0.tar.gz",
              "another_testpkg_2.3.0.tar.gz",
              "another_testpkg_2.3.4.tar.gz",
              "a_testpkg_1.0.14.tar.gz")
    if (!dir.exists(repo_path))
        dir.create(repo_path, recursive = TRUE)
    for (pkg in pkgs) {
        if (!pkg %in% list.files(repo_path))
            file.create(file.path(repo_path, pkg))
    }
    removed_ex <- c(file.path(repo_path, "mytestpkg_1.2.3.tar.gz"),
                    file.path(repo_path, "another_testpkg_2.3.0.tar.gz"))
    removed <- suppressWarnings(remove_old_binaries(repo, "4.6.0", "linux",
                                                    test = TRUE))
    expect_contains(removed, removed_ex)
})
