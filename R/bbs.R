#' Default location of Bioconductor build reports
#'
#' @description
#' `{version}` and `{type}` are filled in by `bbs_report_url()`. Override with
#' `options(biocUniTools.bbs_url = "<new template>")`.
#'
#' @noRd
.BBS_URL <- "https://bioconductor.org/checkResults/{version}/{type}-LATEST"

#' Get the URL of a Bioconductor build report
#'
#' @param branch character "devel" or "release"
#' @param type character one of `bioc_pkg_types()$type`
#'
#' @returns character URL
#'
#' @examples
#' bbs_report_url("devel", "software")
#'
#' @export
bbs_report_url <- function(branch, type = "software") {
    bu <- uni_for_bioc(branch)
    url <- getOption("biocUniTools.bbs_url", .BBS_URL)
    url <- sub("{version}", bu$bioc_version, url, fixed = TRUE)
    sub("{type}", .type_as(type, "bbs"), url, fixed = TRUE)
}

#' Read a build report's status database
#'
#' @param report_url character from `bbs_report_url()`
#'
#' @returns data.frame with columns Package, node, stage, result
#'
#' @noRd
.read_bbs_status_db <- function(report_url) {
    lines <- readLines(file.path(report_url, "BUILD_STATUS_DB.txt"),
                       warn = FALSE)
    parts <- stringr::str_match(lines, "^([^#]+)#([^#]+)#([^:]+):\\s*(.*)$")
    data.frame(Package = parts[, 2], node = parts[, 3], stage = parts[, 4],
               result = parts[, 5])
}

#' Get details of a build node from its NodeInfo page
#'
#' @param report_url character from `bbs_report_url()`
#' @param node character node name
#'
#' @returns data.frame with one row and columns node, os, arch, platform,
#'     r_version
#'
#' @noRd
.bbs_node_info <- function(report_url, node) {
    html <- paste(readLines(file.path(report_url,
                                      paste0(node, "-NodeInfo.html")),
                            warn = FALSE),
                  collapse = " ")
    text <- gsub("\\s+", " ", gsub("&nbsp;", " ", gsub("<[^>]*>", " ", html)))
    field <- function(label, next_label)
        trimws(stringr::str_match(
            text, paste0(label, ":\\s*(.*?)\\s*", next_label))[, 2])
    data.frame(node = node,
               os = field("OS", "Arch:"),
               arch = field("Arch", "Platform:"),
               platform = field("Platform", "R version:"),
               r_version = field("R version", "R environment"))
}

#' Get the build nodes of a build report
#'
#' @param branch character "devel" or "release"
#' @param type character one of `bioc_pkg_types()$type`
#'
#' @returns data.frame with columns node, os, arch, platform, r_version, or
#'     `NULL` with a message if the report is not available yet
#'
#' @examples
#' bbs_nodes("devel", "software")
#'
#' @export
bbs_nodes <- function(branch, type = "software") {
    report_url <- bbs_report_url(branch, type)
    status_db <- .bbs_try(.read_bbs_status_db(report_url), "build status",
                          report_url)
    if (is.null(status_db))
        return(NULL)
    nodes <- unique(status_db$node)
    do.call(rbind, lapply(nodes, .bbs_node_info, report_url = report_url))
}

#' Read package metadata from a build report
#'
#' @description Downloads `report.tgz` and reads each package's `info.dcf`,
#' which includes the git commit the build used.
#'
#' @param report_url character from `bbs_report_url()`
#'
#' @returns data.frame with columns Package, Version, Maintainer, commit,
#'     commit_date
#'
#' @noRd
.read_bbs_info <- function(report_url) {
    tgz <- tempfile(fileext = ".tgz")
    exdir <- tempfile()
    on.exit(unlink(c(tgz, exdir), recursive = TRUE))
    httr2::request(file.path(report_url, "report.tgz")) |>
        httr2::req_perform(path = tgz)
    utils::untar(tgz, files = "*/raw-results/info.dcf", exdir = exdir,
                 extras = "--wildcards")
    files <- list.files(exdir, pattern = "^info\\.dcf$", recursive = TRUE,
                        full.names = TRUE)
    fields <- c("Package", "Version", "Maintainer", "git_last_commit",
                "git_last_commit_date")
    info <- lapply(files, function(f) read.dcf(f, fields = fields))
    info <- as.data.frame(do.call(rbind, info))
    info |>
        dplyr::rename(commit = git_last_commit,
                      commit_date = git_last_commit_date) |>
        dplyr::mutate(commit_date = as.POSIXct(
            sub("\\s*\\(.*$", "", commit_date), format = "%Y-%m-%d %H:%M:%S %z"))
}

#' Read deprecated packages from a build report
#'
#' @param report_url character from `bbs_report_url()`
#'
#' @returns character vector of package names
#'
#' @noRd
.read_bbs_deprecated <- function(report_url) {
    meat <- read.dcf(url(file.path(report_url, "meat-index.dcf")),
                     fields = c("Package", "PackageStatus"))
    meat[which(meat[, "PackageStatus"] == "Deprecated"), "Package"]
}

#' Read part of a build report that may not exist yet
#'
#' @description A new Bioconductor version's reports appear as its builds
#' finish, e.g., after a release, so a file may be missing for a day or more.
#' Reports what is missing with a message and returns `default` instead of
#' failing.
#'
#' @param expr expression that reads the file
#' @param what character name of what is read, for the message
#' @param report_url character from `bbs_report_url()`
#' @param default value to return if it cannot be read
#'
#' @noRd
.bbs_try <- function(expr, what, report_url, default = NULL) {
    # Reading a missing URL warns and then errors, so handle both
    missing <- function(e) {
        message("BBS ", what, " is not available at ", report_url, ": ",
                conditionMessage(e))
        default
    }
    tryCatch(expr, error = missing, warning = missing)
}

#' A bbs_status() result without packages
#'
#' @param report_url character from `bbs_report_url()`
#'
#' @noRd
.bbs_empty <- function(report_url) {
    chr <- character()
    data.frame(Package = chr, Version = chr, type = chr, bioc_version = chr,
               node = chr, Maintainer = chr, deprecated = logical(),
               commit = chr, commit_date = as.POSIXct(chr), install = chr,
               build = chr, check = chr, build_bin = chr, url = chr) |>
        structure(node_info = NULL, report_url = report_url)
}

#' Get Bioconductor Build System (BBS) results for a build node
#'
#' @description
#' Returns one row per package with the version and commit the BBS used and the
#' result of each stage: `R CMD INSTALL` (install), `R CMD build` (build),
#' `R CMD check` (check), and building binaries (build_bin, not done on Linux).
#'
#' @param branch character "devel" or "release"
#' @param type character one of `bioc_pkg_types()$type`
#' @param platform character used to find the node when `node` is `NULL`
#'     (default: "x86_64-linux-gnu")
#' @param node character build node name. If `NULL` (default), the node
#'     building on `platform`.
#'
#' @returns data.frame with attributes `node_info` (see `bbs_nodes()`) and
#'     `report_url`. If the report or the node is not available yet, e.g.,
#'     right after a release, a message says so and the data.frame has no
#'     rows. If a file with commits, deprecated packages, or node details is
#'     missing, a message says so and those columns are `NA`.
#'
#' @examples
#' \dontrun{
#' bbs_status("devel", "workflows")
#' }
#'
#' @export
bbs_status <- function(branch, type = "software",
                       platform = "x86_64-linux-gnu", node = NULL) {
    report_url <- bbs_report_url(branch, type)
    bu <- uni_for_bioc(branch)

    status_db <- .bbs_try(.read_bbs_status_db(report_url), "build status",
                          report_url)
    if (is.null(status_db))
        return(.bbs_empty(report_url))
    nodes <- do.call(rbind, lapply(unique(status_db$node), function(n)
        .bbs_try(.bbs_node_info(report_url, n), paste(n, "node info"),
                 report_url, default = data.frame(node = n, os = NA, arch = NA,
                                                  platform = NA,
                                                  r_version = NA))))
    if (is.null(node)) {
        node <- nodes$node[nodes$platform %in% platform]
        if (length(node) != 1) {
            message("Expected 1 BBS node for platform '", platform, "' at ",
                    report_url, ", found ", length(node))
            return(.bbs_empty(report_url))
        }
    }

    stages <- c(install = "install", buildsrc = "build", checksrc = "check",
                buildbin = "build_bin")
    results <- status_db |>
        dplyr::filter(node == !!node, stage %in% names(stages)) |>
        dplyr::mutate(stage = stages[stage]) |>
        tidyr::pivot_wider(names_from = stage, values_from = result)
    for (s in setdiff(stages, names(results)))
        results[[s]] <- NA_character_

    chr <- character()
    info <- .bbs_try(.read_bbs_info(report_url), "report.tgz", report_url,
                     default = data.frame(Package = chr, Version = chr,
                                          Maintainer = chr, commit = chr,
                                          commit_date = as.POSIXct(chr)))
    deprecated <- .bbs_try(.read_bbs_deprecated(report_url), "meat-index.dcf",
                           report_url, default = NA_character_)

    info |>
        dplyr::right_join(results, by = "Package") |>
        dplyr::mutate(
            type = type,
            bioc_version = bu$bioc_version,
            deprecated = if (anyNA(deprecated)) NA else
                Package %in% deprecated,
            url = file.path(report_url, Package, "")) |>
        dplyr::select(Package, Version, type, bioc_version, node, Maintainer,
                      deprecated, commit, commit_date, install, build, check,
                      build_bin, url) |>
        dplyr::arrange(Package) |>
        as.data.frame() |>
        structure(node_info = nodes[nodes$node == node, ],
                  report_url = report_url)
}
