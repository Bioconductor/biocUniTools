#' Get the Bioconductor package type R Universe reports for each package
#'
#' @param raw_df data.frame from `get_raw_uni_df()`
#' @param bioc_branch character "devel" or "release"
#'
#' @returns character vector, one type per row of `raw_df`, `NA` if unknown
#'
#' @noRd
.uni_bioc_type <- function(raw_df, bioc_branch) {
    types <- bioc_pkg_types()
    repos <- purrr::map_chr(raw_df$`_bioc`, function(bioc) {
        if (!is.data.frame(bioc) || !"repository" %in% names(bioc))
            return(NA_character_)
        repo <- bioc$repository[bioc$branch == bioc_branch]
        if (length(repo) == 0) NA_character_ else repo[1]
    })
    types$type[match(repos, types$universe)]
}

#' Get all R Universe jobs for a Bioconductor branch
#'
#' @description Downloads the universe for `branch` and returns `get_jobs()`
#' output with a `bioc_type` column, the package type R Universe reports.
#'
#' @param branch character "devel" or "release"
#'
#' @returns data.frame
#'
#' @examples
#' \dontrun{
#' jobs <- uni_jobs("release")
#' uni_status("release", "software", jobs = jobs)
#' uni_status("release", "books", jobs = jobs)
#' }
#'
#' @export
uni_jobs <- function(branch) {
    bu <- uni_for_bioc(branch)
    raw_df <- get_raw_uni_df(bu$universe)
    raw_df$bioc_type <- .uni_bioc_type(raw_df, bu$bioc_branch)
    get_jobs(get_uni_df(raw_df), bu$r_version, bu$universe)
}

#' Get R Universe results for Bioconductor packages
#'
#' @description
#' Returns one row per package with the version and commit R Universe built and
#' the result of each job:
#'
#' * `status`: overall R Universe build status
#' * `source_check`: `R CMD check` of the source package, which builds vignettes
#' * `check`: `R CMD check` of the binary job for `os` and `arch`
#' * `binary_check`, `binary_status`: check result and status of the binary
#' * `bioccheck`: result of `BiocCheck`
#'
#' Packages are assigned a type with `bioc_manifest()`. Packages not in any
#' manifest use the type R Universe reports, if any.
#'
#' @param branch character "devel" or "release"
#' @param type character one of `bioc_pkg_types()$type`
#' @param os character (default: "linux")
#' @param arch character (default: "x86_64")
#' @param jobs data.frame from `uni_jobs()`. If `NULL` (default), downloaded.
#'     Pass it to reuse a download for several types.
#'
#' @returns data.frame
#'
#' @examples
#' \dontrun{
#' uni_status("devel", "workflows")
#' }
#'
#' @export
uni_status <- function(branch, type = "software", os = "linux",
                       arch = "x86_64", jobs = NULL) {
    bu <- uni_for_bioc(branch)
    if (is.null(jobs))
        jobs <- uni_jobs(branch)

    manifests <- lapply(stats::setNames(nm = bioc_pkg_types()$type),
                        bioc_manifest, branch = branch)
    in_type <- jobs$Package %in% manifests[[type]]
    in_none <- !jobs$Package %in% unlist(manifests)
    if ("bioc_type" %in% names(jobs))
        in_type <- in_type | (in_none & jobs$bioc_type %in% type)
    jobs <- jobs[in_type, ]

    pkgs <- jobs |>
        dplyr::distinct(Package, .keep_all = TRUE) |>
        dplyr::transmute(
            Package,
            Version = dplyr::coalesce(Version, `_failure.version`),
            type = type,
            bioc_version = bu$bioc_version,
            universe = bu$universe,
            commit = substr(`_commit.id`, 1, 7),
            commit_date = as.POSIXct(`_commit.time`, origin = "1970-01-01"),
            status = dplyr::coalesce(`_status`, "failure"),
            build_url = dplyr::coalesce(`_buildurl`, `_failure.buildurl`))

    # One row per package with a job's check result and url
    job_result <- function(df, check_col, url_col) {
        df <- dplyr::distinct(df, Package, .keep_all = TRUE)
        out <- data.frame(Package = df$Package)
        out[[check_col]] <- df$jobs_check
        out[[url_col]] <- df$job_url
        out
    }
    source <- jobs |>
        dplyr::filter(job_type == "source") |>
        job_result("source_check", "source_url")
    bioccheck <- jobs |>
        dplyr::filter(job_type == "bioc-checks") |>
        job_result("bioccheck", "bioccheck_url")
    binary_jobs <- jobs |>
        dplyr::filter(job_type == "binary", job_os == !!os) |>
        filter_by_arch(os, arch) |>
        dplyr::distinct(Package, .keep_all = TRUE)
    binary <- job_result(binary_jobs, "check", "check_url")
    binary$binary_check <- binary_jobs$binaries_check
    binary$binary_status <- binary_jobs$binaries_status

    pkgs |>
        dplyr::left_join(source, by = "Package") |>
        dplyr::left_join(binary, by = "Package") |>
        dplyr::left_join(bioccheck, by = "Package") |>
        dplyr::select(Package, Version, type, bioc_version, universe, commit,
                      commit_date, status, source_check, check, binary_check,
                      binary_status, bioccheck, build_url, source_url,
                      check_url, bioccheck_url) |>
        dplyr::arrange(Package) |>
        as.data.frame()
}
