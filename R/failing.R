#' Results that count as failing
#'
#' @noRd
.FAILING <- c("ERROR", "TIMEOUT")

#' First failing result in a set of stages
#'
#' @param results named list of character vectors, one per stage, in order
#' @param urls named list of character vectors of links, same names
#'
#' @returns data.frame with columns failure (e.g., "check: ERROR") and url,
#'     `NA` where nothing failed
#'
#' @noRd
.first_failure <- function(results, urls) {
    n <- length(results[[1]])
    failure <- rep(NA_character_, n)
    url <- rep(NA_character_, n)
    for (stage in names(results)) {
        failed <- is.na(failure) &
            normalize_result(results[[stage]]) %in% .FAILING
        failure[failed] <- paste0(stage, ": ", results[[stage]][failed])
        url[failed] <- urls[[stage]][failed]
    }
    data.frame(failure = failure, url = url)
}

#' Packages failing in BBS or R Universe
#'
#' @description
#' A package fails in BBS when install, build, or check is ERROR or TIMEOUT,
#' and in R Universe when its source build or check is ERROR, FAIL, or TIMEOUT
#' or its status is failure. Each system's first failing stage is reported.
#'
#' @param cmp data.frame from `compare_builds()`, or a list of them to combine
#'     package types
#'
#' @returns data.frame with columns Package, type, branch, deprecated,
#'     bbs_failure, bbs_failure_url, uni_failure, uni_failure_url,
#'     downloads_month, n_revdeps, revdeps, revdeps_suggests, mentions, and
#'     commits_12wk, sorted by downloads
#'
#' @examples
#' \dontrun{
#' compare_failing(compare_builds("devel", "books"))
#' }
#'
#' @export
compare_failing <- function(cmp) {
    if (!is.data.frame(cmp))
        return(do.call(rbind, lapply(cmp, compare_failing)))
    col <- function(name, default = NA)
        if (name %in% names(cmp)) cmp[[name]] else rep(default, nrow(cmp))
    stage_url <- function(stage)
        ifelse(is.na(cmp$bbs_url), NA_character_,
               paste0(cmp$bbs_url, cmp$bbs_node, "-", stage, ".html"))
    bbs <- .first_failure(
        list(install = cmp$bbs_install, build = cmp$bbs_build,
             check = cmp$bbs_check),
        list(install = stage_url("install"), build = stage_url("buildsrc"),
             check = stage_url("checksrc")))
    uni <- .first_failure(
        list(`source build` = cmp$uni_source_build, check = cmp$uni_check),
        list(`source build` = cmp$uni_source_url, check = cmp$uni_check_url))
    status_failed <- is.na(uni$failure) & cmp$uni_status %in% "failure"
    uni$failure[status_failed] <- "status: failure"
    uni$url[status_failed] <- cmp$uni_build_url[status_failed]

    out <- data.frame(
        Package = cmp$Package,
        type = cmp$type,
        branch = cmp$branch,
        deprecated = cmp$bbs_deprecated %in% TRUE,
        bbs_failure = bbs$failure,
        bbs_failure_url = bbs$url,
        uni_failure = uni$failure,
        uni_failure_url = uni$url,
        downloads_month = col("downloads_month"),
        n_revdeps = col("uni_n_revdeps"),
        mentions = col("uni_mentions"),
        commits_12wk = col("uni_commits_12wk"))
    empty <- I(rep(list(character()), nrow(cmp)))
    out$revdeps <- if ("uni_revdeps" %in% names(cmp)) cmp$uni_revdeps else empty
    out$revdeps_suggests <- if ("uni_revdeps_suggests" %in% names(cmp))
        cmp$uni_revdeps_suggests else empty
    out <- out[!is.na(out$bbs_failure) | !is.na(out$uni_failure), ]
    out[order(-out$downloads_month, out$Package, na.last = TRUE), ,
        drop = FALSE] |>
        `rownames<-`(NULL)
}

#' Interactive table of failing packages
#'
#' @param failing data.frame from `compare_failing()`
#'
#' @returns reactable htmlwidget
#'
#' @examples
#' \dontrun{
#' failing_table(compare_failing(compare_builds("devel", "books")))
#' }
#'
#' @export
failing_table <- function(failing) {
    if (!requireNamespace("reactable", quietly = TRUE))
        stop("Install 'reactable' to use failing_table()")
    types <- bioc_pkg_types()
    # e.g., "check: ERROR" is shown as "check" and an ERROR badge
    failure <- function(x, url) {
        stage <- htmltools::htmlEscape(sub(": .*$", "", x))
        badge <- .badge(sub("^.*: ", "", x), url)
        ifelse(is.na(x), '<span class="text-body-secondary">&#8212;</span>',
               paste(stage, badge))
    }
    df <- data.frame(
        Package = sprintf('<a href="%s" target="_blank">%s</a>',
                          mapply(.landing_url, failing$Package, failing$type,
                                 failing$branch),
                          failing$Package),
        type = types$label[match(failing$type, types$type)],
        deprecated = ifelse(failing$deprecated,
                            paste0('<span class="badge rounded-pill fw-normal ',
                                   'bg-secondary-subtle text-secondary-emphasis ',
                                   'border border-secondary-subtle">',
                                   'Deprecated</span>'),
                            ""),
        bbs_failure = failure(failing$bbs_failure, failing$bbs_failure_url),
        uni_failure = failure(failing$uni_failure, failing$uni_failure_url),
        downloads_month = round(failing$downloads_month),
        n_revdeps = failing$n_revdeps,
        mentions = failing$mentions,
        commits_12wk = failing$commits_12wk)

    html <- function(name, ...)
        reactable::colDef(name = name, html = TRUE, ...)
    num <- function(name, ...)
        reactable::colDef(name = name, align = "right", sortNALast = TRUE,
                          na = "\u2014",
                          format = reactable::colFormat(separators = TRUE),
                          ...)
    reactable::reactable(
        df,
        columns = list(
            Package = html("Package", minWidth = 160),
            type = reactable::colDef(name = "Type", minWidth = 120),
            deprecated = html("Deprecated", minWidth = 110),
            bbs_failure = html("BBS", minWidth = 150),
            uni_failure = html("R Universe", minWidth = 170),
            downloads_month = num("Downloads / month"),
            n_revdeps = num("Strong reverse deps"),
            mentions = num("Mentions"),
            commits_12wk = num("Commits (12 weeks)")),
        details = .revdeps_details(failing$revdeps, failing$revdeps_suggests),
        searchable = TRUE, filterable = TRUE, compact = TRUE, striped = TRUE,
        highlight = TRUE, showPageSizeOptions = TRUE, defaultPageSize = 50,
        pageSizeOptions = c(25, 50, 100, 500),
        class = "compare-table")
}
