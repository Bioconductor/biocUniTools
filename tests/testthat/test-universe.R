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
