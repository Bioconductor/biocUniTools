#' Put BBS and R Universe results on a common scale
#'
#' @description
#' BBS and R Universe report results differently. This maps both to one scale
#' so they can be compared:
#'
#' * `OK`, `NOTE` -> `OK`
#' * `WARNING`, `WARNINGS` -> `WARNING`
#' * `ERROR`, `FAIL` -> `ERROR`
#'
#' Anything else (e.g., `TIMEOUT`, `skipped`, `CANCELLED`) is kept as is.
#'
#' @param result character
#'
#' @returns character
#'
#' @examples
#' normalize_result(c("NOTE", "WARNINGS", "FAIL", "TIMEOUT"))
#'
#' @export
normalize_result <- function(result) {
    dplyr::case_match(result,
                      c("OK", "NOTE") ~ "OK",
                      c("WARNING", "WARNINGS") ~ "WARNING",
                      c("ERROR", "FAIL") ~ "ERROR",
                      .default = result)
}

#' Compare BBS and R Universe builds
#'
#' @description
#' Joins `bbs_status()` and `uni_status()` by package. Columns from each system
#' are prefixed with `bbs_` or `uni_`. Adds:
#'
#' * `where`: "both", "bbs only", or "universe only"
#' * `version_match`, `commit_match`: whether both systems built the same
#'   version and commit
#' * `bbs_result`, `uni_result`: the results compared. For workflows, BBS
#'   `R CMD build` and R Universe's source `R CMD build`. Otherwise, `R CMD
#'   check` on both.
#' * `result_match`: whether the results agree after `normalize_result()`
#' * `downloads_12mo`, `downloads_month`, `download_rank`, `ranked`: from
#'   `bioc_downloads()`
#'
#' `NA` means there is nothing to compare. Attributes `bbs_node_info`,
#' `bbs_report_url`, and `universe_url` describe where the results come from.
#'
#' @param branch character "devel" or "release"
#' @param type character one of `bioc_pkg_types()$type`
#' @param bbs data.frame from `bbs_status()`. If `NULL` (default), downloaded.
#' @param uni data.frame from `uni_status()`. If `NULL` (default), downloaded.
#' @param jobs data.frame from `uni_jobs()`, used when `uni` is `NULL`.
#' @param downloads data.frame from `bioc_downloads()`, downloaded by default.
#'     `NULL` to leave out download statistics.
#'
#' @returns data.frame
#'
#' @examples
#' \dontrun{
#' compare_builds("devel", "books")
#' }
#'
#' @export
compare_builds <- function(branch, type = "software", bbs = NULL, uni = NULL,
                           jobs = NULL,
                           downloads = bioc_downloads(branch, type)) {
    if (is.null(bbs))
        bbs <- bbs_status(branch, type)
    if (is.null(uni))
        uni <- uni_status(branch, type, jobs = jobs)
    if (is.null(downloads))
        downloads <- data.frame(Package = character())

    bioc_version <- stats::na.omit(c(bbs$bioc_version, uni$bioc_version))[1]
    universe <- stats::na.omit(uni$universe)[1]
    bbs_attrs <- attributes(bbs)[c("node_info", "report_url")]
    bbs <- bbs |>
        dplyr::select(-type, -bioc_version, -build_bin) |>
        dplyr::rename_with(~ paste0("bbs_", tolower(.x)), -Package)
    uni <- uni |>
        dplyr::select(-type, -bioc_version) |>
        dplyr::rename_with(~ paste0("uni_", tolower(.x)), -Package)

    workflows <- type == "workflows"
    dplyr::full_join(bbs, uni, by = "Package") |>
        dplyr::left_join(downloads, by = "Package") |>
        dplyr::mutate(
            type = type,
            branch = branch,
            bioc_version = bioc_version,
            where = dplyr::case_when(
                is.na(uni_universe) ~ "bbs only",
                is.na(bbs_node) ~ "universe only",
                .default = "both"),
            version_match = bbs_version == uni_version,
            commit_match = substr(bbs_commit, 1, 7) == substr(uni_commit, 1, 7),
            bbs_result = if (workflows) bbs_build else bbs_check,
            uni_result = if (workflows) uni_source_build else uni_check,
            result_match = normalize_result(bbs_result) ==
                normalize_result(uni_result)) |>
        dplyr::relocate(type, branch, bioc_version, where, version_match, commit_match,
                        bbs_result, uni_result, result_match,
                        .after = Package) |>
        dplyr::arrange(Package) |>
        as.data.frame() |>
        structure(bbs_node_info = bbs_attrs$node_info,
                  bbs_report_url = bbs_attrs$report_url,
                  universe_url = if (!is.na(universe))
                      paste0("https://", universe, ".r-universe.dev"))
}

#' Summarize a comparison
#'
#' @param cmp data.frame from `compare_builds()`
#'
#' @returns data.frame with one row per branch and type
#'
#' @examples
#' \dontrun{
#' compare_summary(compare_builds("devel", "books"))
#' }
#'
#' @export
compare_summary <- function(cmp) {
    cmp |>
        dplyr::group_by(branch, type) |>
        dplyr::summarize(
            packages = dplyr::n(),
            both = sum(where == "both"),
            bbs_only = sum(where == "bbs only"),
            universe_only = sum(where == "universe only"),
            commits_match = sum(commit_match, na.rm = TRUE),
            commits_differ = sum(!commit_match, na.rm = TRUE),
            results_match = sum(result_match, na.rm = TRUE),
            results_differ = sum(!result_match, na.rm = TRUE),
            check_diverged = sum(uni_check_diverged, na.rm = TRUE),
            .groups = "drop") |>
        as.data.frame()
}

#' Read a comparison report configuration
#'
#' @param file character path to a YAML file with `branches`, `types`, and
#'     `data_dir`. Optional: `platform` (BBS), `os` and `arch` (R Universe).
#'
#' @returns list
#'
#' @noRd
.read_compare_config <- function(file) {
    config <- yaml::read_yaml(file)
    defaults <- list(platform = "x86_64-linux-gnu", os = "linux",
                     arch = "x86_64")
    utils::modifyList(defaults, config)
}

#' Compare builds for every branch and type in a configuration
#'
#' @description
#' Saves one `compare-<branch>-<type>.rds` per branch and type and a
#' `summary.rds` in `data_dir`. Reports read these files, so the data is
#' downloaded once.
#'
#' @param config character path to a YAML configuration. See
#'     `reports/config.yml` on the `reports` branch of the biocUniTools
#'     repository.
#'
#' @returns data.frame summary, invisibly
#'
#' @examples
#' \dontrun{
#' write_comparisons("reports/config.yml")
#' }
#'
#' @export
write_comparisons <- function(config) {
    config_dir <- dirname(config)
    config <- .read_compare_config(config)
    data_dir <- file.path(config_dir, config$data_dir)
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

    summaries <- list()
    for (branch in config$branches) {
        jobs <- uni_jobs(branch)
        for (type in config$types) {
            bbs <- bbs_status(branch, type, platform = config$platform)
            uni <- uni_status(branch, type, os = config$os, arch = config$arch,
                              jobs = jobs)
            cmp <- compare_builds(branch, type, bbs = bbs, uni = uni)
            attr(cmp, "created") <- Sys.time()
            saveRDS(cmp, file.path(data_dir,
                                   sprintf("compare-%s-%s.rds", branch, type)))
            summaries[[paste(branch, type)]] <- compare_summary(cmp)
        }
    }
    summary <- do.call(rbind, unname(summaries))
    attr(summary, "created") <- Sys.time()
    saveRDS(summary, file.path(data_dir, "summary.rds"))
    invisible(summary)
}
