test_that("bbs_report_url fills in version and type", {
    withr::local_options(biocUniTools.bbs_url = "x/{version}/{type}")
    bu <- uni_for_bioc("devel")
    expect_equal(bbs_report_url("devel", "data-experiment"),
                 paste0("x/", bu$bioc_version, "/data-experiment"))
    expect_equal(bbs_report_url("devel", "software"),
                 paste0("x/", bu$bioc_version, "/bioc"))
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
