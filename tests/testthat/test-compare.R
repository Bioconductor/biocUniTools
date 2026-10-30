test_that("normalize_result puts both systems on one scale", {
    expect_equal(normalize_result(c("OK", "NOTE", "WARNING", "WARNINGS",
                                    "ERROR", "FAIL", "TIMEOUT", NA)),
                 c("OK", "OK", "WARNING", "WARNING", "ERROR", "ERROR",
                   "TIMEOUT", NA))
})

bbs <- data.frame(Package = c("a", "b", "c"), Version = c("1.0", "1.0", "1.0"),
                  type = "software", bioc_version = "3.24", node = "n1",
                  commit = c("abc1234", "abc1234", "abc1234"),
                  build = c("OK", "OK", "ERROR"),
                  check = c("WARNINGS", "OK", "skipped"),
                  build_bin = NA)
uni <- data.frame(Package = c("a", "b", "d"), Version = c("1.0", "1.1", "1.0"),
                  type = "software", bioc_version = "3.24", universe = "bioc",
                  commit = c("abc1234", "def5678", "abc1234"),
                  source_build = c("OK", "ERROR", "OK"),
                  check = c("WARNING", "NOTE", "OK"))

test_that("compare_builds compares R CMD check for software", {
    x <- compare_builds("devel", "software", bbs = bbs, uni = uni,
                        downloads = NULL)
    expect_equal(x$Package, c("a", "b", "c", "d"))
    expect_equal(x$where, c("both", "both", "bbs only", "universe only"))
    expect_equal(x$commit_match, c(TRUE, FALSE, NA, NA))
    expect_equal(x$result_match, c(TRUE, TRUE, NA, NA))
})

test_that("compare_builds compares R CMD build for workflows", {
    x <- compare_builds("devel", "workflows", bbs = bbs, uni = uni,
                        downloads = NULL)
    expect_equal(x$bbs_result[1:2], c("OK", "OK"))
    expect_equal(x$uni_result[1:2], c("OK", "ERROR"))
    expect_equal(x$result_match[1:2], c(TRUE, FALSE))
})

test_that("compare_summary counts matches and differences", {
    uni$check_diverged <- c(FALSE, TRUE, FALSE)
    s <- compare_summary(compare_builds("devel", "software", bbs = bbs,
                                        uni = uni, downloads = NULL))
    expect_equal(nrow(s), 1)
    expect_equal(s$packages, 4)
    expect_equal(c(s$both, s$bbs_only, s$universe_only), c(2, 1, 1))
    expect_equal(c(s$commits_match, s$commits_differ), c(1, 1))
    expect_equal(c(s$results_match, s$results_differ), c(2, 0))
    expect_equal(s$check_diverged, 1)
})
