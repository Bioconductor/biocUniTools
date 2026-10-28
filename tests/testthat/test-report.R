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
