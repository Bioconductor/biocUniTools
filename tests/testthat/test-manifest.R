test_that("bioc_git_branch maps branches", {
    expect_equal(bioc_git_branch(list(bioc_branch = "devel",
                                      bioc_version = "3.24")), "devel")
    expect_equal(bioc_git_branch(list(bioc_branch = "release",
                                      bioc_version = "3.23")), "RELEASE_3_23")
})

test_that("manifest_url can be overridden", {
    withr::local_options(biocUniTools.manifest_url = "x/{git_branch}/{type}")
    expect_equal(manifest_url(), "x/{git_branch}/{type}")
})

test_that("bioc_manifest reads package names from a local manifest", {
    dir <- withr::local_tempdir()
    dir.create(file.path(dir, "devel"))
    writeLines(c("## comment", "Package: a", "", "Package: b"),
               file.path(dir, "devel", "workflows.txt"))
    withr::local_options(
        biocUniTools.manifest_url = file.path(dir, "{git_branch}/{type}.txt"))
    expect_equal(bioc_manifest("devel", "workflows"), c("a", "b"))
})
