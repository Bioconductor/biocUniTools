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

#' Reverse dependencies within a universe
#'
#' @param jobs data.frame from `uni_jobs()`
#'
#' @returns data.frame with columns Package, revdeps (strong: Depends, Imports,
#'     LinkingTo), and revdeps_suggests. Both are list columns of sorted
#'     package names.
#'
#' @noRd
.uni_revdeps <- function(jobs) {
    empty <- data.frame(Package = character(), revdeps = I(list()),
                        revdeps_suggests = I(list()))
    if (!"_dependencies" %in% names(jobs))
        return(empty)
    pkgs <- dplyr::distinct(jobs, Package, .keep_all = TRUE)
    edges <- purrr::map2(pkgs$Package, pkgs$`_dependencies`, function(pkg, deps) {
        if (!is.data.frame(deps) || nrow(deps) == 0)
            return(NULL)
        data.frame(Package = deps$package, user = pkg, role = deps$role)
    })
    edges <- do.call(rbind, edges)
    if (is.null(edges))
        return(empty)
    edges <- edges[edges$Package %in% pkgs$Package, ]
    strong <- edges$role %in% c("Depends", "Imports", "LinkingTo")
    users <- function(e) lapply(split(e$user, e$Package), function(x) sort(unique(x)))
    out <- data.frame(Package = sort(unique(edges$Package)))
    out$revdeps <- I(unname(users(edges[strong, ])[out$Package]))
    out$revdeps_suggests <- I(unname(users(edges[edges$role == "Suggests", ])[out$Package]))
    out$revdeps <- I(lapply(out$revdeps, function(x) if (is.null(x)) character() else x))
    out$revdeps_suggests <- I(lapply(out$revdeps_suggests,
                                     function(x) if (is.null(x)) character() else x))
    out
}

#' Count commits in the last weeks from R Universe's `_updates`
#'
#' @param updates list of data.frames with columns week ("YYYY-WW") and n
#' @param weeks integer number of weeks
#' @param today Date
#'
#' @returns integer vector
#'
#' @noRd
.recent_commits <- function(updates, weeks = 12L, today = Sys.Date()) {
    # Monday of ISO week `w` in `year`: week 1 contains January 4
    monday <- function(year, w) {
        jan4 <- as.Date(sprintf("%d-01-04", year))
        jan4 - (as.integer(format(jan4, "%u")) - 1) + 7 * (w - 1)
    }
    since <- today - 7 * weeks
    vapply(updates, function(u) {
        if (!is.data.frame(u) || nrow(u) == 0)
            return(0L)
        parts <- strsplit(u$week, "-", fixed = TRUE)
        start <- as.Date(vapply(parts, function(x)
            as.numeric(monday(as.integer(x[1]), as.integer(x[2]))), numeric(1)))
        as.integer(sum(u$n[start >= since]))
    }, integer(1))
}

#' Get all R Universe jobs for a Bioconductor branch
#'
#' @description Downloads the universe for `branch` and returns `get_jobs()`
#' output with a `bioc_type` column, the package type R Universe reports.
#'
#' @param branch character "devel" or "release"
#' @param packages character package names. If `NULL` (default), all
#'     packages in the universe, which takes a few minutes.
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
uni_jobs <- function(branch, packages = NULL) {
    bu <- uni_for_bioc(branch)
    raw_df <- get_raw_uni_df(bu$universe, packages)
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
#' * `source_build`: `R CMD build` of the source package, which builds vignettes
#' * `check`: `R CMD check` of the binary job for `os` and `arch`
#' * `binary_check`, `binary_status`: check result and status of the binary
#' * `check_diverged`: `TRUE` if `check` and `binary_check` differ
#' * `bioccheck`: result of `BiocCheck`
#' * `n_revdeps`, `revdeps`: number and names of packages in the universe with
#'   a strong dependency (Depends, Imports, LinkingTo) on the package
#' * `revdeps_suggests`: names of packages in the universe that suggest it
#' * `usedby`: R Universe's count of packages anywhere in R Universe that use
#'   it
#' * `score`: R Universe's package score
#' * `mentions`: R Universe's count of mentions in scientific papers
#' * `commits_12wk`: commits in the last 12 weeks
#'
#' Reverse dependencies are found among the packages in `jobs`, so they are
#' complete only when `jobs` has the whole universe.
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

    revdeps <- .uni_revdeps(jobs)

    manifests <- lapply(stats::setNames(nm = bioc_pkg_types()$type),
                        bioc_manifest, branch = branch)
    in_type <- jobs$Package %in% manifests[[type]]
    in_none <- !jobs$Package %in% unlist(manifests)
    if ("bioc_type" %in% names(jobs))
        in_type <- in_type | (in_none & jobs$bioc_type %in% type)
    jobs <- jobs[in_type, ]

    # Columns R Universe only includes when some package has them
    optional <- c("_failure.version", "_failure.buildurl", "_status",
                  "_buildurl", "_usedby", "_score", "_mentions")
    for (col in setdiff(optional, names(jobs)))
        jobs[[col]] <- NA
    if (!"_updates" %in% names(jobs))
        jobs$`_updates` <- list(NULL)

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
            build_url = dplyr::coalesce(`_buildurl`, `_failure.buildurl`),
            usedby = as.integer(`_usedby`),
            score = as.numeric(`_score`),
            mentions = as.integer(`_mentions`),
            commits_12wk = .recent_commits(`_updates`))

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
        job_result("source_build", "source_url")
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
        dplyr::left_join(revdeps, by = "Package") |>
        dplyr::mutate(
            revdeps = I(lapply(revdeps, function(x) if (is.null(x)) character() else x)),
            revdeps_suggests = I(lapply(revdeps_suggests,
                                        function(x) if (is.null(x)) character() else x)),
            n_revdeps = lengths(revdeps)) |>
        dplyr::mutate(check_diverged = !is.na(check) & !is.na(binary_check) &
                          check != binary_check) |>
        dplyr::select(Package, Version, type, bioc_version, universe, commit,
                      commit_date, status, source_build, check, binary_check,
                      check_diverged, binary_status, bioccheck, n_revdeps,
                      revdeps, revdeps_suggests, usedby, score, mentions,
                      commits_12wk, build_url, source_url, check_url,
                      bioccheck_url) |>
        dplyr::arrange(Package) |>
        as.data.frame()
}
