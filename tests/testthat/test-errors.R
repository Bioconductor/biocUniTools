test_that("parse_uni_log finds a failing test", {
    log <- c("##[error]Check: tests, Result: ERROR",
             "── Failure ('test-a.R:1:1'): works ────",
             "Expected `x` to equal 1.",
             "Backtrace:",
             "  1. f()")
    p <- parse_uni_log(log)
    expect_equal(p$step, "Tests")
    expect_equal(p$cause, "Test expectation failed")
    expect_equal(p$excerpt, c("── Failure ('test-a.R:1:1'): works",
                              "Expected `x` to equal 1."))
})

test_that("parse_uni_log finds a failing vignette chunk", {
    log <- c("Quitting from a.Rmd:1-5 [load]", "~~~~~~", "<error/rlang_error>",
             "Error:", "! failed to load resource", "---", "Backtrace:",
             "##[error]Installation OK but failed to build vignettes")
    p <- parse_uni_log(log, source = TRUE)
    expect_equal(p$step, "Vignettes")
    expect_equal(p$cause, "Hub resource")
    expect_equal(p$excerpt, c("Quitting from a.Rmd:1-5 [load]",
                              "! failed to load resource"))
})

test_that("parse_uni_log matches the excerpt before the rest of the log", {
    log <- c("Warning: cannot open URL 'x'",
             "##[error]Check: examples, Result: ERROR",
             "Error in g(): bowtie2-build: command not found",
             "Execution halted",
             "Error: cannot open URL 'x'")
    expect_equal(parse_uni_log(log)$cause, "External tool")
})

test_that("parse_uni_log uses Other when no rule matches", {
    p <- parse_uni_log(c("##[error]Check: examples, Result: ERROR",
                         "Error in f(): something odd"))
    expect_equal(p$cause, "Other")
    expect_equal(p$excerpt, "Error in f(): something odd")
})

test_that("uni_error_rules can be overridden", {
    withr::local_options(biocUniTools.error_rules =
                             data.frame(cause = "Odd", pattern = "odd"))
    expect_equal(parse_uni_log("Error: odd")$cause, "Odd")
})

test_that("uni_errors stops downloading after repeated failures", {
    failing <- data.frame(
        Package = c("a", "b", "c"), type = "software", branch = "devel",
        uni_failure = "check: ERROR",
        uni_failure_url = sprintf(
            "https://github.com/r-universe/bioc/actions/runs/1/job/%d", 1:3))
    calls <- 0
    local_mocked_bindings(uni_job_log = function(...) {
        calls <<- calls + 1
        NULL
    })
    expect_message(e <- uni_errors(failing, delay = 0, max_failures = 2),
                   "Stopped downloading")
    expect_equal(calls, 2)
    expect_true(all(is.na(e$error_cause)))
})

test_that("uni_errors downloads at most max_downloads logs", {
    failing <- data.frame(
        Package = c("a", "b", "c"), type = "software", branch = "devel",
        uni_failure = "check: ERROR",
        uni_failure_url = sprintf(
            "https://github.com/r-universe/bioc/actions/runs/1/job/%d", 1:3))
    calls <- 0
    local_mocked_bindings(uni_job_log = function(...) {
        calls <<- calls + 1
        "Error: there is no package called 'x'"
    })
    expect_message(e <- uni_errors(failing, delay = 0, max_downloads = 2),
                   "skipped downloading 1")
    expect_equal(calls, 2)
    expect_equal(e$error_cause, c("Missing package", "Missing package", NA))
    expect_equal(attr(e, "max_downloads"), 2)
    expect_equal(attr(e, "skipped"), 1)
    expect_equal(attr(e, "failed"), 0)
})
