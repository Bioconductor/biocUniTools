# One package per criterion, each failing only that criterion, plus one
# that passes everything. Jobs are built from a small universe table.
make_jobs <- function() {
    pkgs <- c("pass", "no_source", "no_check", "no_commit", "unsupported",
              "note_only")
    one <- function(pkg, source_check, win_check, unsupported) {
        data.frame(
            Package = pkg, Version = "1.0.0",
            RemoteSha = paste0(substr(pkg, 1, 1), "aaaaaa1"),
            job_type = c("source", "binary", "binary"),
            job_os = c("linux", "linux", "win"),
            job_arch = c(NA, "x86_64", "x86_64"), job_r_xy = "4.6",
            jobs_check = c(source_check, "OK", win_check),
            binary_os = c(NA, "linux", "win"),
            binary_r_xy = c(NA, "4.6", "4.6"),
            binaries_version = c(NA, "1.0.0", "1.0.0"),
            binaries_arch = NA_character_,
            binaries_distro = c(NA, "resolute", NA),
            binaries_check = c(NA, "OK", win_check),
            binaries_status = c(NA, "success", "success"),
            job_id = seq_len(3) + 10 * match(pkg, pkgs),
            `_buildurl` = "https://github.com/r-universe/bioc/actions/runs/1",
            `Config/Bioconductor/UnsupportedPlatforms` = unsupported,
            check.names = FALSE)
    }
    uni_df <- rbind(one("pass", "OK", "OK", NA),
                    one("no_source", "ERROR", "OK", NA),
                    one("no_check", "OK", "ERROR", NA),
                    one("no_commit", "OK", "OK", NA),
                    one("unsupported", "OK", "OK", "windows"),
                    one("note_only", "NOTE", "OK", NA))
    get_jobs(uni_df, "4.6.1", "bioc")
}
jobs <- make_jobs()
# BBS built the same commits, except for no_commit
bbs <- data.frame(Package = unique(jobs$Package), Version = "1.0.0")
bbs$BBS_commit <- paste0(substr(bbs$Package, 1, 1), "aaaaaa")
bbs$BBS_commit[bbs$Package == "no_commit"] <- "zzzzzzz"

win <- function(...)
    sort(get_candidates(list(), "windows", "x86_64", jobs = jobs, bbs = bbs,
                        ...)$Package)

test_that("binary candidates pass every criterion that is on", {
    expect_equal(win(commit = TRUE), c("note_only", "pass"))
})

test_that("vignettes turns only the source build criterion on and off", {
    expect_false("no_source" %in% win(commit = TRUE))
    expect_true("no_source" %in% win(commit = TRUE, vignettes = FALSE))
})

test_that("commit turns the commit criterion on and off", {
    expect_false("no_commit" %in% win(commit = TRUE))
    expect_true("no_commit" %in% win(commit = FALSE))
})

test_that("the binary check applies whether or not commit is on", {
    expect_false("no_check" %in% win(commit = TRUE))
    expect_false("no_check" %in% win(commit = FALSE))
})

test_that("arch is required for windows and macosx binaries", {
    expect_error(get_candidates(list(), "windows", jobs = jobs),
                 "arch must be x86_64 or arm64")
    expect_error(get_candidates(list(), "macosx", jobs = jobs),
                 "arch must be x86_64 or arm64")
})

test_that("unsupported_platforms turns only the platform criterion on and off", {
    expect_false("unsupported" %in% win(commit = TRUE))
    expect_true("unsupported" %in%
                    win(commit = TRUE, unsupported_platforms = FALSE))
})

test_that("check sets which results pass", {
    expect_false("note_only" %in% win(commit = TRUE, check = "OK"))
})

src <- function(...)
    sort(get_candidates(list(), type = "source", jobs = jobs, bbs = bbs,
                        ...)$Package)

test_that("source candidates need the source build, linux check, and commit", {
    # Every package passed its linux check; no_source failed its source build
    expect_equal(src(commit = TRUE),
                 c("no_check", "note_only", "pass", "unsupported"))
    expect_true("no_commit" %in% src(commit = FALSE))
    expect_false("no_source" %in% src(commit = FALSE))
    expect_false("note_only" %in% src(check = "OK"))
})

test_that("source candidates link to the source package", {
    x <- get_candidates(list(), type = "source", jobs = jobs)
    expect_equal(x$artifact[x$Package == "pass"],
                 "https://bioc.r-universe.dev/src/contrib/pass_1.0.0.tar.gz")
})

test_that("get_candidates returns only the jobs' columns", {
    x <- get_candidates(list(), "windows", "x86_64", commit = TRUE,
                        jobs = jobs, bbs = bbs)
    expect_true(all(names(x) %in% names(jobs)))
})
