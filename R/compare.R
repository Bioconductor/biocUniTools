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
#'   `R CMD build` and R Universe's source `R CMD check`. Otherwise, `R CMD
#'   check` on both.
#' * `result_match`: whether the results agree after `normalize_result()`
#'
#' `NA` means there is nothing to compare.
#'
#' @param branch character "devel" or "release"
#' @param type character one of `bioc_pkg_types()$type`
#' @param bbs data.frame from `bbs_status()`. If `NULL` (default), downloaded.
#' @param uni data.frame from `uni_status()`. If `NULL` (default), downloaded.
#' @param jobs data.frame from `uni_jobs()`, used when `uni` is `NULL`.
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
                           jobs = NULL) {
    if (is.null(bbs))
        bbs <- bbs_status(branch, type)
    if (is.null(uni))
        uni <- uni_status(branch, type, jobs = jobs)

    bbs <- bbs |>
        dplyr::select(-type, -bioc_version, -build_bin) |>
        dplyr::rename_with(~ paste0("bbs_", tolower(.x)), -Package)
    uni <- uni |>
        dplyr::select(-type, -bioc_version) |>
        dplyr::rename_with(~ paste0("uni_", tolower(.x)), -Package)

    workflows <- type == "workflows"
    dplyr::full_join(bbs, uni, by = "Package") |>
        dplyr::mutate(
            type = type,
            branch = branch,
            where = dplyr::case_when(
                is.na(uni_universe) ~ "bbs only",
                is.na(bbs_node) ~ "universe only",
                .default = "both"),
            version_match = bbs_version == uni_version,
            commit_match = substr(bbs_commit, 1, 7) == substr(uni_commit, 1, 7),
            bbs_result = if (workflows) bbs_build else bbs_check,
            uni_result = if (workflows) uni_source_check else uni_check,
            result_match = normalize_result(bbs_result) ==
                normalize_result(uni_result)) |>
        dplyr::relocate(type, branch, where, version_match, commit_match,
                        bbs_result, uni_result, result_match,
                        .after = Package) |>
        dplyr::arrange(Package) |>
        as.data.frame()
}
