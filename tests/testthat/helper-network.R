# Tests that download live data from Bioconductor or R Universe call this
# first, so they are skipped on CRAN and without an internet connection.
skip_if_no_network <- function() {
    skip_on_cran()
    skip_if_offline("bioconductor.org")
}

# Download once per test run and reuse
.network_cache <- new.env()
cached <- function(key, value) {
    if (!exists(key, envir = .network_cache, inherits = FALSE))
        assign(key, value, envir = .network_cache)
    get(key, envir = .network_cache)
}

# uni_for_bioc() for a branch, skipping the test without a connection
uni_branch <- function(branch) {
    skip_if_no_network()
    cached(branch, uni_for_bioc(branch))
}

# R Universe jobs for the release branch
release_jobs <- function() {
    release <- uni_branch("release")
    cached("release_jobs",
           get_jobs(get_uni_df(get_raw_uni_df(release$universe)),
                    release$r_version, release$universe))
}
