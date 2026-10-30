test_that(".uni_bioc_type maps R Universe repositories to types", {
    raw_df <- data.frame(Package = c("a", "b", "c"))
    raw_df$`_bioc` <- list(
        data.frame(branch = c("devel", "release"),
                   repository = c("data/experiment", "bioc")),
        data.frame(branch = "devel", version = "1.0"),
        NULL)
    expect_equal(.uni_bioc_type(raw_df, "devel"),
                 c("data-experiment", NA, NA))
    expect_equal(.uni_bioc_type(raw_df, "release"),
                 c("software", NA, NA))
})

test_that(".uni_revdeps finds strong and suggested reverse dependencies", {
    jobs <- data.frame(Package = c("a", "b", "c"))
    jobs$`_dependencies` <- list(
        NULL,
        data.frame(package = c("a", "x"), role = c("Imports", "Depends")),
        data.frame(package = c("a", "b"), role = c("Suggests", "LinkingTo")))
    r <- .uni_revdeps(jobs)
    expect_equal(r$revdeps[r$Package == "a"][[1]], "b")
    expect_equal(r$revdeps_suggests[r$Package == "a"][[1]], "c")
    expect_equal(r$revdeps[r$Package == "b"][[1]], "c")
    # packages outside the universe are ignored
    expect_false("x" %in% r$Package)
})

test_that(".recent_commits counts commits in the last weeks", {
    updates <- list(data.frame(week = c("2026-40", "2026-30", "2025-01"),
                               n = c(2L, 3L, 5L)),
                    NULL)
    expect_equal(.recent_commits(updates, weeks = 12L,
                                 today = as.Date("2026-10-30")),
                 c(2L, 0L))
    expect_equal(.recent_commits(updates, weeks = 20L,
                                 today = as.Date("2026-10-30")),
                 c(5L, 0L))
})
