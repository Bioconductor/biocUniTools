#' Default location of a Bioconductor manifest file
#'
#' @description
#' `{git_branch}` and `{type}` are filled in by `bioc_manifest()`. Override with
#' `options(biocUniTools.manifest_url = "<new template>")`.
#'
#' @noRd
.MANIFEST_URL <-
    "https://raw.githubusercontent.com/bioc/manifest/{git_branch}/{type}.txt"

#' Get the manifest URL template
#'
#' @description
#' The manifest lists the packages of each type in a Bioconductor branch. Its
#' location can be changed with the `biocUniTools.manifest_url` option. The
#' template must contain `{git_branch}` (e.g., `devel` or `RELEASE_3_23`) and
#' `{type}` (e.g., `software`).
#'
#' @returns character URL template
#'
#' @examples
#' manifest_url()
#' \dontrun{
#' options(biocUniTools.manifest_url =
#'     "https://example.org/manifest/{git_branch}/{type}.txt")
#' }
#'
#' @export
manifest_url <- function() {
    getOption("biocUniTools.manifest_url", .MANIFEST_URL)
}

#' Get the git branch name for a Bioconductor version
#'
#' @param bu result of `uni_for_bioc()`
#'
#' @returns character, e.g., "devel" or "RELEASE_3_23"
#'
#' @examples
#' bioc_git_branch(uni_for_bioc("release"))
#'
#' @export
bioc_git_branch <- function(bu) {
    if (bu$bioc_branch == "devel")
        return("devel")
    paste0("RELEASE_", sub(".", "_", bu$bioc_version, fixed = TRUE))
}

#' Get packages listed in a Bioconductor manifest
#'
#' @param branch character "devel" or "release"
#' @param type character one of `bioc_pkg_types()$type`
#'
#' @returns character vector of package names
#'
#' @examples
#' bioc_manifest("devel", "workflows")
#'
#' @export
bioc_manifest <- function(branch, type = "software") {
    bu <- uni_for_bioc(branch)
    url <- manifest_url()
    url <- sub("{git_branch}", bioc_git_branch(bu), url, fixed = TRUE)
    url <- sub("{type}", .type_as(type, "manifest"), url, fixed = TRUE)
    lines <- readLines(url, warn = FALSE)
    pkgs <- sub("^Package:\\s*", "", grep("^Package:", lines, value = TRUE))
    trimws(pkgs)
}
