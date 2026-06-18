devel <- biocUniTools::uni_for_bioc("devel")
release <- biocUniTools::uni_for_bioc("release")

test_that("r_xy_ver gives x.y", {
    expect_equal(r_xy_ver("4.6.3"), "4.6")
    expect_equal(r_xy_ver("40.623.33333"), "40.623")
})

test_that("r_xy_ver handles NA", {
    expect_equal(r_xy_ver(NA), NA_character_)
    expect_equal(r_xy_ver(c("4.6.0", NA)), c("4.6", NA))
})

test_that("uni_pkg_file makes correct file format", {
    expect_equal(uni_pkg_file("pkg", "windows", "1.2.3"),
                 "pkg_1.2.3.zip")
    expect_equal(uni_pkg_file("pkg", "macosx", "1.2.3"),
                 "pkg_1.2.3.tgz")
    expect_equal(uni_pkg_file("pkg", "linux", "1.2.3"),
                 "pkg_1.2.3.tar.gz")
})

test_that("get_binary_os gets abbreviation", {
    expect_equal(get_binary_os("windows"), "win")
    expect_equal(get_binary_os("macosx"), "mac")
    # linux should pass through unchanged
    expect_equal(get_binary_os("linux"), "linux")
    expect_equal(get_binary_os("aarch64-linux"), "aarch64-linux")
})

test_that("uni_repo_url gets the universe url", {
    expect_error(uni_repo_url("bioc", "4.6.0", "macosx", "ventura", "x86_64"),
                 "macosx_name must be big-sur or sonoma")
    expect_equal(uni_repo_url("bioc", "4.5.0", "macosx", "big-sur", "arm64"),
                 "https://bioc.r-universe.dev/bin/macosx/big-sur-arm64/contrib/4.5")
    expect_equal(uni_repo_url("bioc", "4.6.0", "macosx", "sonoma", "arm64"),
                 "https://bioc.r-universe.dev/bin/macosx/sonoma-arm64/contrib/4.6")
    expect_equal(uni_repo_url("bioc", "4.6.0", "windows"),
                 "https://bioc.r-universe.dev/bin/windows/contrib/4.6")
})

test_that("get_macosx_subpath returns correct paths", {
    expect_equal(get_macosx_subpath("4.6", "arm64"), "sonoma-arm64")
    expect_equal(get_macosx_subpath("4.6", "x86_64"), "big-sur-x86_64")
    expect_equal(get_macosx_subpath("4.5", "x86_64"), "big-sur-x86_64")
    expect_equal(get_macosx_subpath("4.5", "arm64"), "big-sur-arm64")
})

test_that("get_repository_path creates good paths", {
    expect_error(get_repository_path("reporoot", "4.6", "macos",
                                     macosx_name = "sonoma", arch = "x86_64"),
                 "sonoma binaries are for arch == arm64 and >= R 4.6")
    expect_error(get_repository_path("reporoot", "4.5", "macos",
                                     macosx_name = "sonoma", arch = "arm64"),
                 "sonoma binaries are for arch == arm64 and >= R 4.6")
    expect_error(get_repository_path("reporoot", "4.5", "macos"),
                 "arch must not be NA for os == macosx")
    expect_error(get_repository_path("reporoot", "4.6", "macosx",
                                     macosx_name = "sonoma"),
                 "arch must not be NA for os == macosx")
    expect_error(get_repository_path("reporoot", "4.5", "macos",
                                     macosx_name = "catalina", arch = "x86_64"),
                 "macosx_name must be big-sur or sonoma")
    expect_error(get_repository_path("reporoot", "4.5", "macos",
                                     macosx_name = "big-sur", arch = "arm32"),
                 "arch must be x86_64 or arm64")
    expect_equal(get_repository_path("reporoot", "4.5", "linux"),
                 "reporoot/src/contrib")
    expect_equal(get_repository_path("reporoot", "4.6", "windows"),
                 "reporoot/bin/windows/contrib/4.6")
    expect_equal(get_repository_path("reporoot", "4.5", "macos",
                                     macosx_name = "big-sur", arch = "arm64"),
                 "reporoot/bin/macosx/big-sur-arm64/contrib/4.5")
    expect_equal(get_repository_path("reporoot", "4.6", "macos",
                                     macosx_name = "big-sur", arch = "x86_64"),
                 "reporoot/bin/macosx/big-sur-x86_64/contrib/4.6")
    expect_equal(get_repository_path("reporoot", "4.6", "macos",
                                     macosx_name = "sonoma", arch = "arm64"),
                 "reporoot/bin/macosx/sonoma-arm64/contrib/4.6")
})

test_that("is_unsupported_platforms filters os-arch", {
    expect_true(is_unsupported_platform("windows, macosx-arm64", "win", "x86_64"))
    expect_true(is_unsupported_platform("windows, macosx-arm64", "mac", "arm64"))
    expect_true(!is_unsupported_platform("windows, macosx-arm64", "mac", "x86_64"))
    expect_true(is_unsupported_platform("macosx", "mac", "arm64"))
    expect_true(is_unsupported_platform("wasm-enscripten", "wasm", "enscripten"))

    # NA arch: OS-only entry still matches, arch-specific entry does not
    expect_true(is_unsupported_platform("macosx", "mac", NA))
    expect_false(is_unsupported_platform("macosx-arm64", "mac", NA))

    # NA os always FALSE
    expect_false(is_unsupported_platform("windows", NA, NA))

    expect_false(is_unsupported_platform("wasm", "win", "x86_64"))
    expect_false(is_unsupported_platform("enscripten", "mac", "arm64"))
    expect_false(is_unsupported_platform(NA, "win", "x86_64"))

    # abbreviations work
    expect_true(is_unsupported_platform("win", "win", "x86_64"))
    expect_true(is_unsupported_platform("mac", "mac", "arm64"))

    # whitespace tolerance
    expect_true(is_unsupported_platform("windows , macosx-arm64", "win", "x86_64"))

    # linux
    expect_false(is_unsupported_platform("linux", "lin", "x86_64"))
    expect_false(is_unsupported_platform("linux", "win", "x86_64"))
    expect_true(is_unsupported_platform("aarch64-linux-gnu", "linux", "arm64"))
})

test_that("get_candidates gets R Universe data", {
    # windows
    pkgs <- get_candidates("bioc", "devel", "4.6.0", "3.23", "windows")
    expect_gt(nrow(pkgs), 0)
    
    pkg <- pkgs |>
        dplyr::filter(!is.na(Artifact)) |>
        dplyr::slice(1)
    version <- dplyr::pull(pkg, Version)
    package <- dplyr::pull(pkg, Package)
    url <- file.path(uni_repo_url("bioc", "4.6.0", "windows"),
                     uni_pkg_file(package, "windows", version))
    expect_equal(dplyr::pull(pkg, Artifact), url)
    
    # macosx x86_64
    pkgs <- get_candidates("bioc-release", "release", "4.5.0", "3.22",
                           "macosx", arch = "x86_64")
    expect_gt(nrow(pkgs), 0)
    
    pkg <- pkgs |>
        dplyr::filter(!is.na(Artifact)) |>
        dplyr::slice(1)
    version <- dplyr::pull(pkg, Version)
    package <- dplyr::pull(pkg, Package)
    url <- file.path(uni_repo_url("bioc-release", "4.5.0", "macosx",
                                  macosx_name = "big-sur", arch = "x86_64"),
                     uni_pkg_file(package, "macosx", version))
    expect_equal(dplyr::pull(pkg, Artifact), url)
})

test_that("get_candidates filters on check status", {
    # without commit filter, check status is not filtered
    candidates <- get_candidates("bioc", "devel", devel$r_version,
                                 devel$bioc_version, "windows")
    expect_gt(nrow(candidates), 0)
    
    # with commit filter, check status is filtered
    candidates_commit <- get_candidates("bioc", "devel", devel$r_version,
                                        devel$bioc_version, "windows",
                                        commit = TRUE)
    expect_true(all(candidates_commit$`_binaries_check` %in% c("NOTE", "WARNING", "OK")))
    expect_true(all(candidates_commit$`_jobs_check` %in% c("NOTE", "WARNING", "OK")))
    expect_true(all(candidates_commit$`_binaries_status` == "success"))
    
    # restrict to OK only
    ok_only <- get_candidates("bioc", "devel", devel$r_version,
                              devel$bioc_version, "windows", commit = TRUE,
                              check = "OK")
    expect_true(all(ok_only$`_binaries_check` == "OK"))
    expect_true(all(ok_only$`_jobs_check` == "OK"))
})

test_that("get_candidates commit filter works", {
    without_commit <- get_candidates("bioc", "devel", devel$r_version,
                                     devel$bioc_version, "windows",
                                     commit = FALSE)
    with_commit    <- get_candidates("bioc", "devel", devel$r_version,
                                     devel$bioc_version, "windows",
                                     commit = TRUE)

    # commit filter should only reduce or maintain row count
    expect_lte(nrow(with_commit), nrow(without_commit))

    # commit-filtered rows should all have matching commits
    if (nrow(with_commit) > 0)
        expect_true(all(with_commit$BBS_commit ==
                            substr(with_commit$RemoteSha, 1, 7)))
})

test_that("get_candidates for sonoma includes arm64 and universal binaries", {
    arm_candidates <- get_candidates("bioc", "devel", devel$r_version,
                                     devel$bioc_version, "macosx",
                                     arch = "arm64", commit = TRUE)
    expect_true("BiocVersion" %in% arm_candidates$Package)
    expect_true("rtracklayer" %in% arm_candidates$Package)
})

test_that("uni_for_bioc gets correct universe", {
    bioc_ru_info <- uni_for_bioc("release")
    expect_equal(bioc_ru_info$r_version, release$r_version)
    expect_equal(bioc_ru_info$bioc_branch, "release")
    expect_equal(bioc_ru_info$ru_uni, "bioc-release")
})

test_that("remove_old_binaries only removes older binaries", {
    repo <- tempdir()
    repo_path <- get_repository_path(repo, "4.6.0", "linux")
    
    pkgs <- c("mytestpkg_1.2.3.tar.gz",
              "mytestpkg_1.3.0.tar.gz",
              "another_testpkg_2.3.0.tar.gz",
              "another_testpkg_2.3.4.tar.gz",
              "a_testpkg_1.0.14.tar.gz")
    
    if (!dir.exists(repo_path))
        dir.create(repo_path, recursive = TRUE)
    for (pkg in pkgs)
        file.create(file.path(repo_path, pkg))
    
    removed <- expect_no_warning(
        remove_old_binaries(repo, "4.6.0", "linux", test = TRUE)
    )
    
    # older versions should be marked for removal
    expect_contains(removed, file.path(repo_path, "mytestpkg_1.2.3.tar.gz"))
    expect_contains(removed, file.path(repo_path, "another_testpkg_2.3.0.tar.gz"))
    
    # newer versions should not be removed
    expect_false(file.path(repo_path, "mytestpkg_1.3.0.tar.gz") %in% removed)
    expect_false(file.path(repo_path, "another_testpkg_2.3.4.tar.gz") %in% removed)
    
    # single version packages should never be removed
    expect_false(file.path(repo_path, "a_testpkg_1.0.14.tar.gz") %in% removed)
})

# Claude generated tests for filter_by_arch
make_mock_df <- function(jobs_os, jobs_arch, binaries_arch) {
    data.frame(
        Package = paste0("pkg", seq_along(jobs_os)),
        `_jobs_os_` = jobs_os,
        `_jobs_arch` = jobs_arch,
        `_binaries_arch` = binaries_arch,
        check.names = FALSE
    )
}

test_that("filter_by_arch filters mac x86_64 correctly", {
    df <- make_mock_df(
        jobs_os =       c("mac",    "mac",   "mac"),
        jobs_arch =     c("x86_64", "arm64", "arm64"),
        binaries_arch = c("x86_64", NA,      "aarch64")
    )
    result <- filter_by_arch(df, "mac", "x86_64")
    expect_equal(nrow(result), 2)
    expect_contains(result$Package, c("pkg1", "pkg2"))
    expect_false("pkg3" %in% result$Package)
})

test_that("filter_by_arch mac x86_64 drops non-arm64 universal binaries", {
    df <- make_mock_df(
        jobs_os =       c("mac"),
        jobs_arch =     c("x86_64"),
        binaries_arch = c(NA)
    )
    result <- filter_by_arch(df, "mac", "x86_64")
    expect_equal(nrow(result), 0)
})

test_that("filter_by_arch filters mac arm64 correctly", {
    df <- make_mock_df(
        jobs_os =       c("mac",     "mac",   "mac"),
        jobs_arch =     c("arm64",   "arm64", "x86_64"),
        binaries_arch = c("aarch64", NA,      "x86_64")
    )
    result <- filter_by_arch(df, "mac", "arm64")
    expect_equal(nrow(result), 2)
    expect_contains(result$Package, c("pkg1", "pkg2"))
    expect_false("pkg3" %in% result$Package)
})

test_that("filter_by_arch filters linux x86_64 correctly", {
    df <- make_mock_df(
        jobs_os =       c("linux",  "linux",  "linux"),
        jobs_arch =     c("x86_64", "x86_64", "arm64"),
        binaries_arch = c("x86_64", NA,       "aarch64")
    )
    result <- filter_by_arch(df, "linux", "x86_64")
    expect_equal(nrow(result), 2)
    expect_contains(result$Package, c("pkg1", "pkg2"))
    expect_false("pkg3" %in% result$Package)
})

test_that("filter_by_arch linux x86_64 drops non-x86_64 universal binaries", {
    df <- make_mock_df(
        jobs_os =       c("linux"),
        jobs_arch =     c("arm64"),
        binaries_arch = c(NA)
    )
    result <- filter_by_arch(df, "linux", "x86_64")
    expect_equal(nrow(result), 0)
})

test_that("filter_by_arch filters linux arm64 correctly", {
    df <- make_mock_df(
        jobs_os =       c("linux",    "linux",  "linux"),
        jobs_arch =     c("arm64",    "x86_64", "x86_64"),
        binaries_arch = c("aarch64",  NA,       "x86_64")
    )
    result <- filter_by_arch(df, "linux", "arm64")
    expect_equal(nrow(result), 2)
    expect_contains(result$Package, c("pkg1", "pkg2"))
    expect_false("pkg3" %in% result$Package)
})

test_that("filter_by_arch linux arm64 drops non-x86_64 universal binaries", {
    df <- make_mock_df(
        jobs_os =       c("linux"),
        jobs_arch =     c("arm64"),
        binaries_arch = c(NA)
    )
    result <- filter_by_arch(df, "linux", "arm64")
    expect_equal(nrow(result), 0)
})

test_that("filter_by_arch treats linux NA arch same as x86_64", {
    df <- make_mock_df(
        jobs_os =       c("linux",  "linux",  "linux"),
        jobs_arch =     c("x86_64", "x86_64", "arm64"),
        binaries_arch = c("x86_64", NA,       "aarch64")
    )
    result_na  <- filter_by_arch(df, "linux", NA)
    result_x86 <- filter_by_arch(df, "linux", "x86_64")
    expect_equal(result_na, result_x86)
})

test_that("filter_by_arch drops rows from other OS", {
    df <- make_mock_df(
        jobs_os =       c("mac",     "linux",  "win"),
        jobs_arch =     c("arm64",   "x86_64", "x86_64"),
        binaries_arch = c("aarch64", "x86_64", NA)
    )
    result <- filter_by_arch(df, "mac", "arm64")
    expect_equal(nrow(result), 1)
    expect_equal(result$Package, "pkg1")
})

test_that("filter_by_arch returns empty df when no matches", {
    df <- make_mock_df(
        jobs_os =       c("mac",    "mac"),
        jobs_arch =     c("x86_64", "x86_64"),
        binaries_arch = c("x86_64", NA)
    )
    result <- filter_by_arch(df, "mac", "arm64")
    expect_equal(nrow(result), 0)
})

test_that("filter_by_arch passes all rows for windows", {
    df <- make_mock_df(
        jobs_os =       c("win",    "win"),
        jobs_arch =     c("x86_64", "x86_64"),
        binaries_arch = c(NA,       NA)
    )
    result <- filter_by_arch(df, "win", NA)
    expect_equal(nrow(result), 2)
})

test_that("get_candidates vignette filter works", {
    # vignettes = TRUE (default) should filter on source check
    with_vignettes <- get_candidates("bioc", release$bioc_branch, release$r_version,
                                     release$bioc_version, "windows",
                                     vignettes = TRUE)
    without_vignettes <- get_candidates("bioc", release$bioc_branch,
                                        release$r_version, release$bioc_version,
                                        "windows", vignettes = FALSE)
    
    # vignette filter should only reduce or maintain row count
    expect_lte(nrow(with_vignettes), nrow(without_vignettes))
    
    # all rows should have passing vignette check
    expect_true(all(with_vignettes$`Vignettes Check` %in% c("NOTE", "WARNING", "OK")))
})

test_that("get_candidates vignette check respects check argument", {
    ok_only <- get_candidates("bioc", release$bioc_branch, release$r_version,
                              release$bioc_version, "windows", vignettes = TRUE,
                              check = "OK")
    expect_true(all(ok_only$`Vignettes Check` == "OK"))
})

test_that("get_candidates without vignettes has no Vignettes Check column", {
    without_vignettes <- get_candidates("bioc", release$bioc_branch,
                                        release$r_version, release$bioc_version,
                                        "windows", vignettes = FALSE)
    expect_false("Vignettes Check" %in% names(without_vignettes))
})
