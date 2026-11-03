cmp <- data.frame(
    Package = c("a", "b", "c", "d"), type = "software", branch = "devel",
    bbs_deprecated = c(FALSE, TRUE, FALSE, FALSE),
    bbs_url = "https://bbs/", bbs_node = "n1",
    bbs_install = "OK", bbs_build = c("OK", "ERROR", "OK", "OK"),
    bbs_check = c("OK", "skipped", "TIMEOUT", "WARNINGS"),
    uni_status = c("success", "success", "success", "failure"),
    uni_source_build = c("OK", "OK", "OK", "OK"),
    uni_check = c("NOTE", "FAIL", "OK", "WARNING"),
    uni_source_url = "s", uni_check_url = "c", uni_build_url = "b",
    downloads_month = c(10, 5, 20, 1))

test_that("compare_failing keeps packages failing in either system", {
    f <- compare_failing(cmp)
    # sorted by downloads; "a" passes everywhere
    expect_equal(f$Package, c("c", "b", "d"))
    expect_equal(f$bbs_failure, c("check: TIMEOUT", "build: ERROR", NA))
    expect_equal(f$bbs_failure_url[2], "https://bbs/n1-buildsrc.html")
    expect_equal(f$uni_failure, c(NA, "check: FAIL", "status: failure"))
    expect_equal(f$deprecated, c(FALSE, TRUE, FALSE))
})

test_that("compare_failing combines a list of comparisons", {
    f <- compare_failing(list(cmp, transform(cmp, type = "books")))
    expect_equal(nrow(f), 6)
})
