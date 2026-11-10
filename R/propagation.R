#' Targets packages propagate to
#'
#' @description The source package and the binaries Bioconductor distributes.
#' Bioconductor only distributes binaries of software packages, so `types`
#' lists the package types each target applies to.
#'
#' @returns data.frame with columns id, label, type, os, arch, and types (a
#'     list column)
#'
#' @examples
#' propagation_targets()
#'
#' @export
propagation_targets <- function() {
    data.frame(
        id = c("source", "windows_x86_64", "macosx_arm64", "macosx_x86_64"),
        label = c("Source", "Windows x86_64", "macOS arm64", "macOS x86_64"),
        type = c("source", "binary", "binary", "binary"),
        os = c(NA, "windows", "macosx", "macosx"),
        arch = c(NA, "x86_64", "arm64", "x86_64"),
        types = I(c(list(bioc_pkg_types()$type), rep(list("software"), 3))))
}

#' Which Bioconductor packages propagate from R Universe
#'
#' @description
#' Applies `propagation_checks()` with the rules `harvest.R` uses, requiring
#' the same commit as BBS, for each of `targets` and every package of `types`
#' in the branch's manifest.
#'
#' @param branch character "devel" or "release"
#' @param types character package types, from `bioc_pkg_types()$type`
#' @param jobs data.frame from `uni_jobs()`. If `NULL` (default), downloaded.
#' @param bbs data.frame of BBS commits with columns Package, Version,
#'     BBS_commit, and Deprecated. If `NULL` (default), read with
#'     `bbs_status()`.
#' @param targets data.frame from `propagation_targets()`
#'
#' @returns data.frame with one row per package and columns Package, type,
#'     Version, deprecated, uni_commit and bbs_commit (the commits R Universe
#'     and BBS built), and, for each target id, a logical column whether it
#'     propagates, `<id>_reason`, why not, `<id>_artifact`, the file to
#'     download if it propagates, and `<id>_failure_url`, the R Universe job
#'     that failed if it does not. A package with no job for a
#'     target does not propagate, with reason "not built". Targets that do not
#'     apply to a package's type are `NA`.
#'
#' @examples
#' \dontrun{
#' propagation_status("devel", types = "books")
#' }
#'
#' @export
propagation_status <- function(branch, types = bioc_pkg_types()$type,
                               jobs = NULL, bbs = NULL,
                               targets = propagation_targets()) {
    bu <- uni_for_bioc(branch)
    if (is.null(jobs))
        jobs <- uni_jobs(branch)
    if (is.null(bbs))
        bbs <- .bbs_commits(bu)

    pkgs <- do.call(rbind, lapply(types, function(type)
        data.frame(Package = bioc_manifest(branch, type), type = type)))
    pkgs$Version <- jobs$Version[match(pkgs$Package, jobs$Package)]
    pkgs$deprecated <- bbs$Deprecated[match(pkgs$Package, bbs$Package)] %in%
        TRUE
    pkgs$uni_commit <- substr(jobs$RemoteSha[match(pkgs$Package, jobs$Package)],
                              1, 7)
    pkgs$bbs_commit <- bbs$BBS_commit[match(pkgs$Package, bbs$Package)]

    for (i in seq_len(nrow(targets))) {
        checks <- propagation_checks(bu, os = targets$os[i],
                                     arch = targets$arch[i],
                                     type = targets$type[i], commit = TRUE,
                                     jobs = jobs, bbs = bbs)
        j <- match(pkgs$Package, checks$Package)
        id <- targets$id[i]
        applies <- pkgs$type %in% targets$types[[i]]
        pkgs[[id]] <- ifelse(applies, !is.na(j) & checks$propagates[j] %in% TRUE,
                             NA)
        pkgs[[paste0(id, "_reason")]] <- ifelse(
            !applies, NA_character_,
            ifelse(is.na(j), "not built", checks$reason[j]))
        pkgs[[paste0(id, "_artifact")]] <- ifelse(pkgs[[id]] %in% TRUE,
                                                  checks$artifact[j],
                                                  NA_character_)
        pkgs[[paste0(id, "_failure_url")]] <- ifelse(pkgs[[id]] %in% FALSE,
                                                     checks$failure_url[j],
                                                     NA_character_)
    }
    pkgs <- pkgs[order(pkgs$Package), , drop = FALSE]
    rownames(pkgs) <- NULL
    structure(pkgs, targets = targets, branch = branch)
}

#' Number of packages that propagate to each target
#'
#' @param status data.frame from `propagation_status()`
#'
#' @returns htmltools table with a row per target
#'
#' @examples
#' \dontrun{
#' propagation_summary_table(propagation_status("devel", types = "books"))
#' }
#'
#' @export
propagation_summary_table <- function(status) {
    targets <- attr(status, "targets")
    fmt <- function(x) format(x, big.mark = ",")
    rows <- lapply(seq_len(nrow(targets)), function(i) {
        x <- status[[targets$id[i]]]
        htmltools::tags$tr(
            htmltools::tags$td(targets$label[i]),
            htmltools::tags$td(class = "text-end", fmt(sum(x, na.rm = TRUE))),
            htmltools::tags$td(class = "text-end",
                               fmt(sum(!x, na.rm = TRUE))))
    })
    htmltools::tags$table(
        class = "table table-sm propagation-summary",
        htmltools::tags$thead(htmltools::tags$tr(
            htmltools::tags$th("Target"),
            htmltools::tags$th(class = "text-end", "Propagate"),
            htmltools::tags$th(class = "text-end", "Don't propagate"))),
        htmltools::tags$tbody(rows))
}

#' Interactive table of which packages propagate
#'
#' @param status data.frame from `propagation_status()`
#'
#' @returns reactable htmlwidget
#'
#' @examples
#' \dontrun{
#' propagation_table(propagation_status("devel", types = "books"))
#' }
#'
#' @export
propagation_table <- function(status) {
    if (!requireNamespace("reactable", quietly = TRUE))
        stop("Install 'reactable' to use propagation_table()")
    targets <- attr(status, "targets")
    types <- bioc_pkg_types()
    branch <- attr(status, "branch")
    df <- data.frame(
        Package = sprintf('<a href="%s" target="_blank">%s</a>',
                          mapply(.landing_url, status$Package, status$type,
                                 branch),
                          status$Package),
        type = types$label[match(status$type, types$type)],
        Version = status$Version,
        uni_commit = status$uni_commit,
        bbs_commit = status$bbs_commit,
        deprecated = ifelse(status$deprecated,
                            paste0('<span class="badge rounded-pill fw-normal ',
                                   'bg-secondary-subtle text-secondary-emphasis ',
                                   'border border-secondary-subtle">',
                                   'Deprecated</span>'),
                            ""))
    pill <- function(level, text)
        sprintf(paste0('<span class="badge rounded-pill fw-normal ',
                       'bg-%1$s-subtle text-%1$s-emphasis ',
                       'border border-%1$s-subtle">%2$s</span>'), level, text)
    link <- function(url, html)
        ifelse(is.na(url), html,
               sprintf('<a href="%s" target="_blank">%s</a>', url, html))
    # "Yes" linked to the file, or "No" linked to the failing job, with the
    # reasons
    answer <- function(yes, reason, artifact, failure_url)
        dplyr::case_when(
            is.na(yes) ~ '<span class="text-body-secondary">&#8212;</span>',
            yes ~ link(artifact, pill("success", "Yes")),
            .default = paste0(link(failure_url, pill("danger", "No")),
                              '<br><small class="text-body-secondary">',
                              htmltools::htmlEscape(reason), '</small>'))
    columns <- list(
        Package = reactable::colDef(name = "Package", html = TRUE,
                                    minWidth = 160),
        type = reactable::colDef(name = "Type", minWidth = 120),
        Version = reactable::colDef(name = "Version", minWidth = 90),
        uni_commit = reactable::colDef(name = "R Universe", minWidth = 100),
        bbs_commit = reactable::colDef(name = "BBS", minWidth = 90),
        deprecated = reactable::colDef(name = "Deprecated", html = TRUE,
                                       minWidth = 110))
    for (i in seq_len(nrow(targets))) {
        id <- targets$id[i]
        df[[id]] <- answer(status[[id]], status[[paste0(id, "_reason")]],
                           status[[paste0(id, "_artifact")]],
                           status[[paste0(id, "_failure_url")]])
        columns[[id]] <- reactable::colDef(name = targets$label[i],
                                           html = TRUE, minWidth = 130)
    }
    reactable::reactable(
        df, columns = columns,
        columnGroups = list(
            reactable::colGroup("Commit", c("uni_commit", "bbs_commit")),
            reactable::colGroup("Propagates to", targets$id)),
        searchable = TRUE, filterable = TRUE, compact = TRUE, striped = TRUE,
        highlight = TRUE, showPageSizeOptions = TRUE, defaultPageSize = 50,
        pageSizeOptions = c(25, 50, 100, 500),
        class = "compare-table")
}
