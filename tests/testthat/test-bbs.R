test_that("bbs_report_url fills in version and type", {
    withr::local_options(biocUniTools.bbs_url = "x/{version}/{type}")
    local_mocked_bindings(uni_for_bioc = function(branch)
        list(bioc_branch = branch, bioc_version = "3.24"))
    expect_equal(bbs_report_url("devel", "data-experiment"),
                 "x/3.24/data-experiment")
    expect_equal(bbs_report_url("devel", "software"), "x/3.24/bioc")
})

test_that(".read_bbs_status_db parses package, node, stage, and result", {
    dir <- withr::local_tempdir()
    writeLines(c("a#n1#install: OK", "a#n1#checksrc: WARNINGS",
                 "b.c#n1#buildsrc: ERROR"),
               file.path(dir, "BUILD_STATUS_DB.txt"))
    db <- .read_bbs_status_db(dir)
    expect_equal(db$Package, c("a", "a", "b.c"))
    expect_equal(db$stage, c("install", "checksrc", "buildsrc"))
    expect_equal(db$result, c("OK", "WARNINGS", "ERROR"))
})

test_that(".bbs_node_info reads node details", {
    dir <- withr::local_tempdir()
    writeLines(c("<td>Hostname:&nbsp;</td><td>n1</td>",
                 "<td>OS:&nbsp;</td><td>Linux&nbsp;(Ubuntu&nbsp;24.04.4&nbsp;LTS)</td>",
                 "<td>Arch:&nbsp;</td><td>x86_64</td>",
                 "<td>Platform:&nbsp;</td><td>x86_64-linux-gnu</td>",
                 "<td>R&nbsp;version:&nbsp;</td><td>4.6.1 (2026-06-24) -- \"Happy Hop\"</td>",
                 "<td>R environment variables:</td>"),
               file.path(dir, "n1-NodeInfo.html"))
    info <- .bbs_node_info(dir, "n1")
    expect_equal(info$os, "Linux (Ubuntu 24.04.4 LTS)")
    expect_equal(info$arch, "x86_64")
    expect_equal(info$platform, "x86_64-linux-gnu")
    expect_equal(info$r_version, "4.6.1 (2026-06-24) -- \"Happy Hop\"")
})

# A build report folder with some of its files
local_bbs_report <- function(files = c("BUILD_STATUS_DB.txt", "n1-NodeInfo.html"),
                             env = parent.frame()) {
    dir <- withr::local_tempdir(.local_envir = env)
    if ("BUILD_STATUS_DB.txt" %in% files)
        writeLines(c("a#n1#install: OK", "a#n1#buildsrc: OK",
                     "a#n1#checksrc: WARNINGS"),
                   file.path(dir, "BUILD_STATUS_DB.txt"))
    if ("n1-NodeInfo.html" %in% files)
        writeLines(c("<td>OS:&nbsp;</td><td>Linux</td>",
                     "<td>Arch:&nbsp;</td><td>x86_64</td>",
                     "<td>Platform:&nbsp;</td><td>x86_64-linux-gnu</td>",
                     "<td>R&nbsp;version:&nbsp;</td><td>4.7.0</td>",
                     "<td>R environment variables:</td>"),
                   file.path(dir, "n1-NodeInfo.html"))
    withr::local_options(biocUniTools.bbs_url = dir, .local_envir = env)
    local_mocked_bindings(uni_for_bioc = function(branch)
        list(bioc_branch = branch, bioc_version = "3.25"), .env = env)
    dir
}

test_that("bbs_status reports a missing build report and returns no rows", {
    local_bbs_report(files = character())
    expect_message(x <- bbs_status("devel", "workflows"),
                   "BBS build status is not available")
    expect_equal(nrow(x), 0)
    expect_true(all(c("Package", "commit", "check", "deprecated") %in% names(x)))
    expect_message(expect_null(bbs_nodes("devel", "workflows")),
                   "not available")
})

test_that("bbs_status keeps results when commits and deprecations are missing", {
    local_bbs_report()
    expect_message(x <- bbs_status("devel"), "report.tgz is not available")
    expect_equal(x$Package, "a")
    expect_equal(x$check, "WARNINGS")
    expect_true(is.na(x$commit))
    expect_true(is.na(x$deprecated))
    expect_equal(attr(x, "node_info")$node, "n1")
})

test_that("bbs_status reports a missing node and returns no rows", {
    local_bbs_report()
    expect_message(x <- bbs_status("devel", platform = "aarch64-linux-gnu"),
                   "Expected 1 BBS node for platform 'aarch64-linux-gnu'")
    expect_equal(nrow(x), 0)
})
