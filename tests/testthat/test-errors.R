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
