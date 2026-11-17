test_that(".badge keeps each row's result when there are no links", {
    b <- .badge(c("ERROR", "WARNING", NA))
    expect_match(b[1], "ERROR")
    expect_match(b[2], "WARNING")
    expect_false(grepl("badge", b[3]))
})

test_that(".badge links each result to its own url", {
    b <- .badge(c("OK", "ERROR"), c("https://a", NA))
    expect_match(b[1], 'href="https://a"')
    expect_false(grepl("href", b[2]))
})

# Comparison of a with the same commit and warnings, b with a different commit
# and a timeout, and c with a different commit and a matching result
report_cmp <- function() {
    cmp <- data.frame(
        Package = c("a", "b", "c"), type = "software", branch = "devel",
        bioc_version = "3.24", where = "both",
        commit_match = c(TRUE, FALSE, FALSE),
        bbs_result = c("WARNINGS", "TIMEOUT", "OK"),
        uni_result = c("OK", "ERROR", "NOTE"),
        uni_check_diverged = FALSE)
    cmp$result_match <- .result_match(cmp)
    cmp
}

test_that("compare_bars counts results with each setting", {
    cmp <- report_cmp()
    text <- function(...) as.character(compare_bars(cmp, ...))
    expect_match(text(), "Differ: 2")
    expect_match(text(warning_ok = TRUE), "Differ: 1")
    expect_match(text(warning_ok = TRUE, timeout_error = TRUE), "Same: 3")
    # Only a has the same commit
    expect_match(text(same_commit = TRUE),
                 "1 packages in both R Universe and the BBS with the same commit; 1 differ")
})

test_that("compare_controls makes bars for every setting", {
    html <- as.character(compare_controls(report_cmp(), "t1"))
    for (v in c("w0t0c0", "w1t0c0", "w0t1c1", "w1t1c1"))
        expect_match(html, sprintf('data-variant="%s"', v))
    # Only the default bars are shown
    shown <- regmatches(html, gregexpr('class="compare-variant"[^>]*>', html))[[1]]
    expect_equal(sum(!grepl("hidden", shown)), 1)
    expect_match(shown[!grepl("hidden", shown)], "w0t0c0")
    expect_match(html, 'data-table="t1"')
})

test_that("compare_table has a result column for every setting", {
    skip_if_not_installed("reactable")
    cmp <- report_cmp()
    cmp[c("bbs_commit", "uni_commit", "bbs_install", "bbs_build", "bbs_check",
          "uni_status", "uni_source_build", "uni_check", "uni_binary_check",
          "uni_bioccheck", "bbs_url", "bbs_node", "uni_build_url",
          "uni_source_url", "uni_check_url", "uni_bioccheck_url")] <- NA_character_
    cmp$bbs_deprecated <- FALSE
    t <- compare_table(cmp, element_id = "t1")
    data <- jsonlite::fromJSON(t$x$tag$attribs$data)
    expect_equal(t$x$tag$attribs$elementId, "t1")
    expect_equal(data$filter_w0t0, c("no", "no", "yes"))
    expect_equal(data$filter_w1t1, c("yes", "yes", "yes"))
    expect_equal(data$filter_commit, c("yes", "no", "no"))
})
