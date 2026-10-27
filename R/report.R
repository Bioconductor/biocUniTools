#' Show a result as a colored badge, optionally linked
#'
#' @param result character
#' @param url character or `NA`
#'
#' @returns character HTML
#'
#' @noRd
.badge <- function(result, url = NA_character_) {
    level <- tolower(normalize_result(result))
    level[is.na(level)] <- "none"
    level[!level %in% c("ok", "warning", "error", "none")] <- "other"
    label <- ifelse(is.na(result), "—", htmltools::htmlEscape(result))
    badge <- sprintf('<span class="badge-result badge-%s">%s</span>', level,
                     label)
    ifelse(is.na(url), badge,
           sprintf('<a href="%s" target="_blank">%s</a>', url, badge))
}

#' Show a logical as a check mark or cross
#'
#' @noRd
.mark <- function(x) {
    dplyr::case_when(x ~ '<span class="mark-yes">✓</span>',
                     !x ~ '<span class="mark-no">✗</span>',
                     .default = '<span class="mark-none">—</span>')
}

#' Interactive table of a comparison
#'
#' @param cmp data.frame from `compare_builds()`
#'
#' @returns reactable htmlwidget
#'
#' @examples
#' \dontrun{
#' compare_table(compare_builds("devel", "books"))
#' }
#'
#' @export
compare_table <- function(cmp) {
    if (!requireNamespace("reactable", quietly = TRUE))
        stop("Install 'reactable' to use compare_table()")
    workflows <- identical(unique(cmp$type), "workflows")
    df <- data.frame(
        Package = cmp$Package,
        where = cmp$where,
        commit_match = .mark(cmp$commit_match),
        result_match = .mark(cmp$result_match),
        bbs_commit = cmp$bbs_commit,
        uni_commit = substr(cmp$uni_commit, 1, 7),
        bbs_install = .badge(cmp$bbs_install),
        bbs_build = .badge(cmp$bbs_build),
        bbs_check = .badge(cmp$bbs_check),
        uni_status = .badge(cmp$uni_status, cmp$uni_build_url),
        uni_source_check = .badge(cmp$uni_source_check, cmp$uni_source_url),
        uni_check = .badge(cmp$uni_check, cmp$uni_check_url),
        uni_binary_check = .badge(cmp$uni_binary_check),
        uni_bioccheck = .badge(cmp$uni_bioccheck, cmp$uni_bioccheck_url),
        deprecated = ifelse(cmp$bbs_deprecated %in% TRUE, "deprecated", ""))
    df$Package <- ifelse(is.na(cmp$bbs_url), df$Package,
                         sprintf('<a href="%s" target="_blank">%s</a>',
                                 cmp$bbs_url, df$Package))

    html <- function(name, ...)
        reactable::colDef(name = name, html = TRUE, ...)
    compared <- list(style = list(background = "var(--compared-bg)"))
    columns <- list(
        Package = html("Package", minWidth = 180, sticky = "left"),
        where = reactable::colDef(name = "Built by", minWidth = 120),
        commit_match = html("Same commit", align = "center"),
        result_match = html("Same result", align = "center"),
        bbs_commit = reactable::colDef(name = "Commit"),
        bbs_install = html("INSTALL"),
        bbs_build = do.call(html, c(list("build"),
                                    if (workflows) compared)),
        bbs_check = do.call(html, c(list("check"),
                                    if (!workflows) compared)),
        uni_commit = reactable::colDef(name = "Commit"),
        uni_status = html("Status"),
        uni_source_check = do.call(html, c(list("Source check"),
                                           if (workflows) compared)),
        uni_check = do.call(html, c(list("Check"), if (!workflows) compared)),
        uni_binary_check = html("Binary check"),
        uni_bioccheck = html("BiocCheck"),
        deprecated = reactable::colDef(name = "", minWidth = 100))

    reactable::reactable(
        df[, names(columns)],
        columns = columns,
        columnGroups = list(
            reactable::colGroup("Comparison",
                                c("where", "commit_match", "result_match")),
            reactable::colGroup("BBS", c("bbs_commit", "bbs_install",
                                         "bbs_build", "bbs_check")),
            reactable::colGroup("R Universe",
                                c("uni_commit", "uni_status",
                                  "uni_source_check", "uni_check",
                                  "uni_binary_check", "uni_bioccheck"))),
        searchable = TRUE, filterable = TRUE, compact = TRUE, striped = TRUE,
        highlight = TRUE, showPageSizeOptions = TRUE, defaultPageSize = 50,
        pageSizeOptions = c(25, 50, 100, 500),
        defaultColDef = reactable::colDef(minWidth = 90,
                                          headerClass = "rt-header"),
        class = "compare-table")
}

#' Interactive table of comparison summaries
#'
#' @param summary data.frame from `compare_summary()`, one row per branch and
#'     type
#' @param link_fmt character `sprintf()` format with 2 `%s` for branch and
#'     type, used to link each row to its page. `NULL` for no links.
#'
#' @returns reactable htmlwidget
#'
#' @examples
#' \dontrun{
#' s <- compare_summary(compare_builds("devel", "books"))
#' summary_table(s)
#' }
#'
#' @export
summary_table <- function(summary, link_fmt = "%s-%s.html") {
    if (!requireNamespace("reactable", quietly = TRUE))
        stop("Install 'reactable' to use summary_table()")
    df <- summary
    if (!is.null(link_fmt))
        df$type <- sprintf('<a href="%s">%s</a>',
                           sprintf(link_fmt, df$branch, df$type), df$type)
    pct <- function(n, d) ifelse(d > 0, sprintf("%.0f%%", 100 * n / d), "—")
    df$commits_pct <- pct(df$commits_match, df$commits_match + df$commits_differ)
    df$results_pct <- pct(df$results_match, df$results_match + df$results_differ)

    num <- function(name, ...) reactable::colDef(name = name, align = "right",
                                                 ...)
    flag <- function(name) num(name, style = function(value)
        if (value > 0) list(color = "var(--bad)", fontWeight = 600))
    columns <- list(
        branch = reactable::colDef(name = "Branch"),
        type = reactable::colDef(name = "Type", html = TRUE, minWidth = 140),
        packages = num("Packages"),
        both = num("Both"),
        bbs_only = flag("BBS only"),
        universe_only = flag("R Universe only"),
        commits_pct = num("Same commit"),
        commits_differ = flag("Commit differs"),
        results_pct = num("Same result"),
        results_differ = flag("Result differs"),
        check_diverged = flag("Check ≠ binary check"))

    reactable::reactable(df[, names(columns)], columns = columns,
                         compact = TRUE, highlight = TRUE, pagination = FALSE,
                         class = "summary-table")
}
