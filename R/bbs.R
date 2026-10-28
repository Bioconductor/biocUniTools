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
#' @returns data.frame with columns node, os, arch, platform, r_version
#'
#' @examples
#' bbs_nodes("devel", "software")
#'
#' @export
bbs_nodes <- function(branch, type = "software") {
    report_url <- bbs_report_url(branch, type)
    nodes <- unique(.read_bbs_status_db(report_url)$node)
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
#'     `report_url`
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

    status_db <- .read_bbs_status_db(report_url)
    nodes <- do.call(rbind, lapply(unique(status_db$node), .bbs_node_info,
                                   report_url = report_url))
    if (is.null(node)) {
        node <- nodes$node[nodes$platform %in% platform]
        if (length(node) != 1)
            stop("Expected 1 node for platform '", platform, "', found ",
                 length(node))
    }

    stages <- c(install = "install", buildsrc = "build", checksrc = "check",
                buildbin = "build_bin")
    results <- status_db |>
        dplyr::filter(node == !!node, stage %in% names(stages)) |>
        dplyr::mutate(stage = stages[stage]) |>
        tidyr::pivot_wider(names_from = stage, values_from = result)
    for (s in setdiff(stages, names(results)))
        results[[s]] <- NA_character_

    .read_bbs_info(report_url) |>
        dplyr::right_join(results, by = "Package") |>
        dplyr::mutate(
            type = type,
            bioc_version = bu$bioc_version,
            deprecated = Package %in% .read_bbs_deprecated(report_url),
            url = file.path(report_url, Package, "")) |>
        dplyr::select(Package, Version, type, bioc_version, node, Maintainer,
                      deprecated, commit, commit_date, install, build, check,
                      build_bin, url) |>
        dplyr::arrange(Package) |>
        as.data.frame() |>
        structure(node_info = nodes[nodes$node == node, ],
                  report_url = report_url)
}
