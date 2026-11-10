test_that("uni_repo_url uses R Universe's linux and source layouts", {
    expect_equal(uni_repo_url("bioc", "4.6.1", "linux", "x86_64",
                              distro = "resolute"),
                 "https://bioc.r-universe.dev/bin/linux/resolute-x86_64/4.6/src/contrib")
    expect_equal(uni_repo_url("bioc", "4.6.1", type = "source"),
                 "https://bioc.r-universe.dev/src/contrib")
    expect_equal(uni_repo_url("bioc", "4.6.1", "linux", "x86_64"),
                 NA_character_)
    expect_equal(uni_repo_url("bioc", "4.6.1", "windows", "x86_64"),
                 "https://bioc.r-universe.dev/bin/windows/gcc-x86_64/contrib/4.6")
})

# Two packages: "a" passes everywhere, "b" fails the linux check
uni_df <- data.frame(
    Package = rep(c("a", "b"), each = 3),
    Version = rep(c("1.0.0", "2.0.0"), each = 3),
    RemoteSha = rep(c("aaaaaaa1", "bbbbbbb1"), each = 3),
    job_type = rep(c("source", "binary", "binary"), 2),
    job_os = rep(c("linux", "linux", "win"), 2),
    job_arch = rep(c(NA, "x86_64", "x86_64"), 2),
    job_r_xy = "4.6",
    jobs_check = c("OK", "NOTE", "OK", "OK", "ERROR", "OK"),
    binary_os = rep(c(NA, "linux", "win"), 2),
    binary_r_xy = rep(c(NA, "4.6", "4.6"), 2),
    binaries_version = rep(c(NA, "1.0.0", NA, "2.0.0"), c(1, 2, 1, 2)),
    binaries_arch = NA_character_,
    binaries_distro = rep(c(NA, "resolute", NA), 2),
    binaries_check = rep(c(NA, "OK", "OK"), 2),
    binaries_status = rep(c(NA, "success", "success"), 2),
    job_id = 1:6,
    `_buildurl` = "https://github.com/r-universe/bioc/actions/runs/1",
    `Config/Bioconductor/UnsupportedPlatforms` = NA_character_,
    check.names = FALSE)
jobs <- get_jobs(uni_df, "4.6.1", "bioc")

test_that("get_jobs links source and linux binary packages", {
    src <- jobs[jobs$job_type == "source" & jobs$Package == "a", ]
    expect_equal(src$artifact,
                 "https://bioc.r-universe.dev/src/contrib/a_1.0.0.tar.gz")
    lin <- jobs[jobs$job_os == "linux" & jobs$job_type == "binary" &
                    jobs$Package == "a", ]
    expect_equal(lin$artifact,
                 paste0("https://bioc.r-universe.dev/bin/linux/",
                        "resolute-x86_64/4.6/src/contrib/a_1.0.0.tar.gz"))
})

test_that("get_candidates keeps only binaries for type binary", {
    x <- get_candidates(list(), "linux", "x86_64", vignettes = FALSE,
                        jobs = jobs)
    expect_true(all(x$job_type == "binary"))
    expect_equal(x$Package, c("a", "b"))
    # "b" failed its check, so there is nothing to download
    expect_true(is.na(x$artifact[x$Package == "b"]))
})

test_that("get_candidates source needs the source build and linux check", {
    x <- get_candidates(list(), type = "source", jobs = jobs)
    expect_equal(x$Package, "a")
    expect_equal(x$artifact,
                 "https://bioc.r-universe.dev/src/contrib/a_1.0.0.tar.gz")
})

test_that("get_candidates source can require the BBS commit", {
    local_mocked_bindings(.bbs_commits = function(bu)
        data.frame(Package = c("a", "b"), Version = c("1.0.0", "2.0.0"),
                   BBS_commit = c("zzzzzzz", "bbbbbbb"), Deprecated = FALSE))
    x <- get_candidates(list(), type = "source", commit = TRUE, jobs = jobs)
    expect_equal(nrow(x), 0)
})

test_that("mac binaries for both architectures use each target's folder", {
    mac <- data.frame(
        Package = "m", Version = "1.0.0", RemoteSha = "mmmmmmm1",
        job_type = "binary", job_os = "mac", job_arch = "arm64",
        job_r_xy = "4.6", jobs_check = "OK", binary_os = "mac",
        binary_r_xy = "4.6", binaries_version = "1.0.0",
        binaries_arch = NA_character_, binaries_distro = NA_character_,
        binaries_check = "OK", binaries_status = "success", job_id = 9L,
        `_buildurl` = "https://github.com/r-universe/bioc/actions/runs/1",
        `Config/Bioconductor/UnsupportedPlatforms` = NA_character_,
        check.names = FALSE)
    mac_jobs <- get_jobs(mac, "4.6.1", "bioc")
    arm <- get_candidates(list(), "macosx", "arm64", vignettes = FALSE,
                          jobs = mac_jobs)
    x86 <- get_candidates(list(), "macosx", "x86_64", vignettes = FALSE,
                          jobs = mac_jobs)
    expect_equal(arm$artifact, paste0("https://bioc.r-universe.dev/bin/macosx/",
                                      "sonoma-arm64/contrib/4.6/m_1.0.0.tgz"))
    expect_equal(x86$artifact, paste0("https://bioc.r-universe.dev/bin/macosx/",
                                      "big-sur-x86_64/contrib/4.6/m_1.0.0.tgz"))
})
