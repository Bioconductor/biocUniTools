#' Show a result as a Bootstrap badge, optionally linked
#'
#' @description Uses Bootstrap's subtle colors: OK and NOTE are green,
#' WARNING is yellow, ERROR is red, and
#' anything else (e.g., TIMEOUT, skipped) is gray. R Universe's overall status
#' "success" is green and "failure" is red.
#'
#' @param result character
#' @param url character or `NA`
#'
#' @returns character HTML
#'
#' @noRd
.badge <- function(result, url = NA_character_) {
    url <- rep_len(url, length(result))
    level <- normalize_result(result)
    level <- dplyr::case_match(level,
                               c("OK", "success") ~ "success",
                               "WARNING" ~ "warning",
                               c("ERROR", "failure") ~ "danger",
                               NA ~ NA,
                               .default = "secondary")
    badge <- sprintf(paste0('<span class="badge rounded-pill fw-normal ',
                            'bg-%1$s-subtle text-%1$s-emphasis ',
                            'border border-%1$s-subtle">%2$s</span>'),
                     level, htmltools::htmlEscape(result))
    badge <- ifelse(is.na(url), badge,
                    sprintf('<a href="%s" target="_blank">%s</a>', url, badge))
    ifelse(is.na(result), '<span class="text-body-secondary">&#8212;</span>',
           badge)
}

#' Link to a package's page on bioconductor.org
#'
#' @param pkg character
#' @param type character one of `bioc_pkg_types()$type`
#' @param branch character "devel" or "release"
#'
#' @returns character URL
#'
#' @noRd
.landing_url <- function(pkg, type, branch) {
    if (type == "books")
        return(sprintf("https://bioconductor.org/books/%s/%s/", branch, pkg))
    sprintf("https://bioconductor.org/packages/%s/%s/html/%s.html", branch,
            .type_as(type, "universe"), pkg)
}

#' Link to the BBS report page of a package's stage, e.g., checksrc
#'
#' @param cmp data.frame from `compare_builds()`
#' @param result character stage result
#' @param stage character "install", "buildsrc", or "checksrc"
#'
#' @returns character URL, `NA` if there is no page
#'
#' @noRd
.bbs_stage_url <- function(cmp, result, stage) {
    ifelse(is.na(cmp$bbs_url) | is.na(result) | result == "skipped",
           NA_character_,
           paste0(cmp$bbs_url, cmp$bbs_node, "-", stage, ".html"))
}

#' Expandable row listing a package's reverse dependencies
#'
#' @param cmp data.frame from `compare_builds()`
#'
#' @returns function for reactable's `details`, or `NULL` if there are no
#'     reverse dependencies
#'
#' @noRd
.revdeps_details <- function(cmp) {
    if (!"uni_revdeps" %in% names(cmp))
        return(NULL)
    strong <- cmp$uni_revdeps
    suggests <- cmp$uni_revdeps_suggests
    if (all(lengths(strong) == 0 & lengths(suggests) == 0))
        return(NULL)
    item <- function(label, pkgs)
        if (length(pkgs))
            htmltools::p(htmltools::strong(sprintf("%s (%d): ", label,
                                                   length(pkgs))),
                         paste(pkgs, collapse = ", "))
    function(index) {
        if (length(strong[[index]]) == 0 && length(suggests[[index]]) == 0)
            return(NULL)
        htmltools::div(
            class = "revdeps-details",
            item("Strong reverse dependencies", strong[[index]]),
            item("Suggested by", suggests[[index]]))
    }
}

#' Show a logical as a Bootstrap Icons check mark or cross with text
#'
#' @noRd
.mark <- function(x) {
    dplyr::case_when(
        x ~ paste0('<span class="text-success-emphasis">',
                   '<i class="bi bi-check-lg" aria-hidden="true"></i> Yes</span>'),
        !x ~ paste0('<span class="text-danger-emphasis">',
                    '<i class="bi bi-x-lg" aria-hidden="true"></i> No</span>'),
        .default = '<span class="text-body-secondary">&#8212;</span>')
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
        bbs_install = .badge(cmp$bbs_install,
                             .bbs_stage_url(cmp, cmp$bbs_install, "install")),
        bbs_build = .badge(cmp$bbs_build,
                           .bbs_stage_url(cmp, cmp$bbs_build, "buildsrc")),
        bbs_check = .badge(cmp$bbs_check,
                           .bbs_stage_url(cmp, cmp$bbs_check, "checksrc")),
        uni_status = .badge(cmp$uni_status, cmp$uni_build_url),
        uni_source_build = .badge(cmp$uni_source_build, cmp$uni_source_url),
        uni_check = .badge(cmp$uni_check, cmp$uni_check_url),
        uni_binary_check = .badge(cmp$uni_binary_check),
        uni_bioccheck = .badge(cmp$uni_bioccheck, cmp$uni_bioccheck_url),
        deprecated = ifelse(cmp$bbs_deprecated %in% TRUE,
                            paste0('<span class="badge rounded-pill fw-normal ',
                                   'bg-secondary-subtle text-secondary-emphasis ',
                                   'border border-secondary-subtle">',
                                   'Deprecated</span>'),
                            ""))
    # Importance columns, NA when compare_builds() did not add them
    col <- function(name) if (name %in% names(cmp)) cmp[[name]] else
        rep(NA, nrow(cmp))
    df$download_rank <- col("download_rank")
    df$downloads_month <- round(col("downloads_month"))
    df$n_revdeps <- col("uni_n_revdeps")
    df$usedby <- col("uni_usedby")
    df$mentions <- col("uni_mentions")
    df$score <- round(col("uni_score"), 1)
    df$commits_12wk <- col("uni_commits_12wk")
    ranked <- stats::na.omit(col("ranked"))[1]
    df$Package <- sprintf('<a href="%s" target="_blank">%s</a>',
                          .landing_url(cmp$Package, cmp$type[1],
                                       cmp$branch[1]),
                          cmp$Package)

    html <- function(name, ...)
        reactable::colDef(name = name, html = TRUE, ...)
    compared <- list(class = "compared")
    num <- function(name, ...)
        reactable::colDef(name = name, align = "right", sortNALast = TRUE,
                          na = "\u2014", format = reactable::colFormat(separators = TRUE),
                          ...)
    columns <- list(
        Package = html("Package", minWidth = 180, sticky = "left"),
        where = reactable::colDef(name = "Built by", minWidth = 120),
        commit_match = html("Same commit"),
        result_match = html("Same result"),
        deprecated = html("Deprecated"),
        bbs_commit = reactable::colDef(name = "Commit"),
        bbs_install = html("Install"),
        bbs_build = do.call(html, c(list("Build"), if (workflows) compared)),
        bbs_check = do.call(html, c(list("Check"), if (!workflows) compared)),
        uni_commit = reactable::colDef(name = "Commit"),
        uni_status = html("Status"),
        uni_source_build = do.call(html, c(list("Source build"),
                                           if (workflows) compared)),
        uni_check = do.call(html, c(list("Check"), if (!workflows) compared)),
        uni_binary_check = html("Binary check"),
        uni_bioccheck = html("BiocCheck"),
        download_rank = num("Download rank", cell = function(value)
            if (is.na(value)) "\u2014" else
                paste(format(value, big.mark = ","), "/",
                      format(ranked, big.mark = ","))),
        downloads_month = num("Downloads / month"),
        n_revdeps = num("Strong reverse deps"),
        usedby = num("Used by (R Universe)"),
        mentions = num("Mentions"),
        score = num("Score"),
        commits_12wk = num("Commits (12 weeks)"))

    reactable::reactable(
        df[, names(columns)],
        columns = columns,
        columnGroups = list(
            reactable::colGroup("Comparison",
                                c("where", "commit_match", "result_match")),
            reactable::colGroup("BBS", c("deprecated", "bbs_commit",
                                         "bbs_install",
                                         "bbs_build", "bbs_check")),
            reactable::colGroup("R Universe",
                                c("uni_commit", "uni_status",
                                  "uni_source_build", "uni_check",
                                  "uni_binary_check", "uni_bioccheck")),
            reactable::colGroup("Importance",
                                c("download_rank", "downloads_month",
                                  "n_revdeps", "usedby", "mentions", "score",
                                  "commits_12wk"))),
        details = .revdeps_details(cmp),
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
    types <- bioc_pkg_types()
    label <- types$label[match(df$type, types$type)]
    df$type <- if (is.null(link_fmt)) label else
        sprintf('<a href="%s">%s</a>', sprintf(link_fmt, df$branch, df$type),
                label)
    pct <- function(n, d) ifelse(d > 0, sprintf("%.0f%%", 100 * n / d), "&#8212;")
    df$commits_pct <- pct(df$commits_match, df$commits_match + df$commits_differ)
    df$results_pct <- pct(df$results_match, df$results_match + df$results_differ)

    num <- function(name, ...) reactable::colDef(name = name, align = "right",
                                                 ...)
    flag <- function(name) num(name, style = function(value)
        if (value > 0) list(color = "var(--bs-danger-text-emphasis)", fontWeight = 600))
    columns <- list(
        branch = reactable::colDef(name = "Branch"),
        type = reactable::colDef(name = "Type", html = TRUE, minWidth = 140),
        packages = num("Packages"),
        both = num("Both"),
        bbs_only = flag("BBS only"),
        universe_only = flag("R Universe only"),
        commits_pct = num("Same commit", html = TRUE),
        commits_differ = flag("Commit differs"),
        results_pct = num("Same result", html = TRUE),
        results_differ = flag("Result differs"),
        check_diverged = flag("Check differs from binary check"))

    reactable::reactable(df[, names(columns)], columns = columns,
                         compact = TRUE, highlight = TRUE, pagination = FALSE,
                         class = "summary-table")
}

#' Title and update time of a comparison page
#'
#' @param cmp data.frame from `compare_builds()`
#'
#' @returns htmltools tags
#'
#' @examples
#' \dontrun{
#' compare_header(compare_builds("devel", "books"))
#' }
#'
#' @export
compare_header <- function(cmp) {
    types <- bioc_pkg_types()
    label <- types$label[types$type == cmp$type[1]]
    created <- attr(cmp, "created")
    htmltools::tagList(
        htmltools::h1(sprintf("Bioconductor %s %s (%s)", label,
                              cmp$bioc_version[1], cmp$branch[1])),
        if (!is.null(created))
            htmltools::p(class = "text-body-secondary",
                         paste("Updated", format(created, "%Y-%m-%d %H:%M %Z"))))
}

#' Where a comparison's results come from, in a collapsed section
#'
#' @param cmp data.frame from `compare_builds()`
#'
#' @returns htmltools tags
#'
#' @examples
#' \dontrun{
#' compare_details(compare_builds("devel", "books"))
#' }
#'
#' @export
compare_details <- function(cmp) {
    node <- attr(cmp, "bbs_node_info")
    uni_url <- attr(cmp, "universe_url")
    link <- function(url, text)
        if (is.null(url)) NULL else htmltools::a(href = url, text)
    rows <- list(
        `R Universe` = link(uni_url, sub("^https://", "", uni_url)),
        BBS = link(attr(cmp, "bbs_report_url"), "Build report"),
        Hostname = node$node,
        OS = node$os,
        Arch = node$arch,
        `R version` = node$r_version)
    rows <- Filter(Negate(is.null), rows)
    items <- lapply(names(rows), function(name)
        list(htmltools::tags$dt(paste0(name, ":")),
             htmltools::tags$dd(rows[[name]])))
    htmltools::tags$details(
        htmltools::tags$summary("Build details"),
        htmltools::tags$dl(class = "compare-details", items))
}

#' Bars showing how many packages match in both systems
#'
#' @description For packages built by both systems, one bar each for commits
#' and results: same (light teal), differ (dark teal), and not compared (gray).
#'
#' @param cmp data.frame from `compare_builds()`
#'
#' @returns htmltools tags
#'
#' @examples
#' \dontrun{
#' compare_bars(compare_builds("devel", "books"))
#' }
#'
#' @export
compare_bars <- function(cmp) {
    s <- compare_summary(cmp)
    fmt <- function(x) format(x, big.mark = ",")
    result <- if (cmp$type[1] == "workflows")
        "Results (R CMD build)" else
        "Results (R CMD check)"
    segment <- function(n, total, class, label) {
        if (n == 0) return(NULL)
        width <- sprintf("width: %.2f%%", 100 * n / total)
        htmltools::div(class = "progress", role = "progressbar",
                       `aria-label` = label, style = width,
                       title = sprintf("%s: %s", label, fmt(n)),
                       htmltools::div(class = paste("progress-bar", class)))
    }
    bar <- function(name, same, differ) {
        unknown <- s$both - same - differ
        htmltools::div(
            class = "mb-3",
            htmltools::div(
                class = "d-flex justify-content-between",
                htmltools::span(class = "fw-semibold", name),
                htmltools::span(sprintf(
                    "%s packages in both R Universe and the BBS; %s differ",
                    fmt(s$both), fmt(differ)))),
            htmltools::div(
                class = "progress-stacked compare-bar",
                segment(same, s$both, "bar-same", "Same"),
                segment(differ, s$both, "bar-differ", "Differ"),
                segment(unknown, s$both, "bar-unknown",
                        "Not compared")))
    }
    only <- sprintf("BBS only: %s. R Universe only: %s.",
                    fmt(s$bbs_only), fmt(s$universe_only))
    diverged <- sprintf("R Universe check differs from binary check: %s.",
                        fmt(s$check_diverged))
    htmltools::tagList(
        bar("Commits", s$commits_match, s$commits_differ),
        bar(result, s$results_match, s$results_differ),
        htmltools::p(class = "small text-body-secondary",
                     paste(only, diverged)))
}
