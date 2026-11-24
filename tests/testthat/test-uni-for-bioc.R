# A config.yaml where the website is still on 3.23 but R Universe moved on
local_bioc_config <- function(r_versions = list("3.24" = "4.6", "3.25" = "4.7"),
                              env = parent.frame()) {
    file <- withr::local_tempfile(fileext = ".yaml", .local_envir = env)
    yaml::write_yaml(list(r_universe_release_version = "3.24",
                          r_universe_devel_version = "3.25",
                          release_version = "3.23",
                          devel_version = "3.24",
                          r_version_associated_with_release = "4.6.0",
                          r_version_associated_with_devel = "4.6.0",
                          r_ver_for_bioc_ver = r_versions), file)
    withr::local_options(biocUniTools.config_url = file, .local_envir = env)
}

test_that("uni_for_bioc uses the versions R Universe builds", {
    local_bioc_config()
    expect_equal(uni_for_bioc("release"),
                 list(bioc_version = "3.24", bioc_branch = "release",
                      universe = "bioc-release", r_version = "4.6"))
    expect_equal(uni_for_bioc("devel"),
                 list(bioc_version = "3.25", bioc_branch = "devel",
                      universe = "bioc", r_version = "4.7"))
})

test_that("uni_for_bioc accepts version numbers", {
    local_bioc_config()
    expect_equal(uni_for_bioc("3.24")$universe, "bioc-release")
    expect_equal(uni_for_bioc("3.25")$universe, "bioc")
    expect_error(uni_for_bioc("3.23"), "builds Bioconductor 3.24 \\(release\\)")
})

test_that("uni_for_bioc stops without an R version for the Bioconductor version", {
    local_bioc_config(r_versions = list("3.24" = "4.6"))
    expect_error(uni_for_bioc("devel"), "no R version for Bioconductor 3.25")
})
