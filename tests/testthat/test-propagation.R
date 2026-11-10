# "a" passes everywhere; "b" fails the linux check; "d" is a data package
uni_df <- data.frame(
    Package = c(rep(c("a", "b"), each = 3), "d"),
    Version = c(rep(c("1.0.0", "2.0.0"), each = 3), "3.0.0"),
    RemoteSha = c(rep(c("aaaaaaa1", "bbbbbbb1"), each = 3), "ddddddd1"),
    job_type = c(rep(c("source", "binary", "binary"), 2), "source"),
    job_os = c(rep(c("linux", "linux", "win"), 2), "linux"),
    job_arch = c(rep(c(NA, "x86_64", "x86_64"), 2), NA),
    job_r_xy = "4.6",
    jobs_check = c("OK", "NOTE", "OK", "OK", "ERROR", "OK", "OK"),
    binary_os = c(rep(c(NA, "linux", "win"), 2), NA),
    binary_r_xy = c(rep(c(NA, "4.6", "4.6"), 2), NA),
    binaries_version = c(NA, "1.0.0", "1.0.0", NA, "2.0.0", "2.0.0", NA),
    binaries_arch = NA_character_,
    binaries_distro = c(rep(c(NA, "resolute", NA), 2), NA),
    binaries_check = c(rep(c(NA, "OK", "OK"), 2), NA),
    binaries_status = c(rep(c(NA, "success", "success"), 2), NA),
    job_id = 1:7,
    `_buildurl` = "https://github.com/r-universe/bioc/actions/runs/1",
    `Config/Bioconductor/UnsupportedPlatforms` = NA_character_,
    check.names = FALSE)
jobs <- get_jobs(uni_df, "4.6.1", "bioc")
bbs <- data.frame(Package = c("a", "b", "d"),
                  Version = c("1.0.0", "2.0.0", "3.0.0"),
                  BBS_commit = c("aaaaaaa", "bbbbbbb", "zzzzzzz"),
                  Deprecated = c(FALSE, TRUE, FALSE))

test_that("propagation_checks reports the rules a package fails", {
    x <- propagation_checks(list(), type = "source", commit = TRUE,
                            jobs = jobs, bbs = bbs)
    expect_equal(x$Package, c("a", "b", "d"))
    expect_equal(x$propagates, c(TRUE, FALSE, FALSE))
    expect_equal(x$reason, c("", "linux check failed",
                             "linux check failed; commit differs from BBS"))
    # "b" links to its failing linux job; "d" has no linux job
    expect_equal(x$failure_url,
                 c(NA, "https://github.com/r-universe/bioc/actions/runs/1/job/5",
                   NA))
})

test_that("propagation_status marks binaries of data packages not applicable", {
    local_mocked_bindings(
        uni_for_bioc = function(branch) list(bioc_branch = branch),
        bioc_manifest = function(branch, type)
            switch(type, software = c("a", "b"), "data-experiment" = "d"))
    targets <- propagation_targets()[1:2, ]
    st <- propagation_status("devel", types = c("software", "data-experiment"),
                             jobs = jobs, bbs = bbs, targets = targets)
    expect_equal(st$Package, c("a", "b", "d"))
    expect_equal(st$source, c(TRUE, FALSE, FALSE))
    expect_equal(st$windows_x86_64, c(TRUE, TRUE, NA))
    expect_equal(st$deprecated, c(FALSE, TRUE, FALSE))
    expect_equal(st$uni_commit, c("aaaaaaa", "bbbbbbb", "ddddddd"))
    expect_equal(st$bbs_commit, c("aaaaaaa", "bbbbbbb", "zzzzzzz"))
    expect_equal(st$source_artifact,
                 c("https://bioc.r-universe.dev/src/contrib/a_1.0.0.tar.gz",
                   NA, NA))
    skip_if_not_installed("reactable")
    expect_s3_class(propagation_table(st), "reactable")
})
