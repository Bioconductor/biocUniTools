#' Bioconductor package types
#'
#' @description
#' One row per Bioconductor package type with the name each system uses for it:
#'
#' * `type`: name used by biocUniTools functions
#' * `manifest`: file name (without `.txt`) in the Bioconductor manifest
#' * `bbs`: build report directory, e.g., `bioc` in `bioc-LATEST`
#' * `universe`: `repository` value in R Universe's `_bioc` field
#'
#' @returns data.frame
#'
#' @examples
#' bioc_pkg_types()
#'
#' @export
bioc_pkg_types <- function() {
    data.frame(
        type = c("software", "data-experiment", "workflows", "books"),
        manifest = c("software", "data-experiment", "workflows", "books"),
        bbs = c("bioc", "data-experiment", "workflows", "books"),
        universe = c("bioc", "data/experiment", "workflows", "books")
    )
}

#' Look up a package type's name in another system
#'
#' @param type character one of `bioc_pkg_types()$type`
#' @param system character column of `bioc_pkg_types()`
#'
#' @returns character
#'
#' @noRd
.type_as <- function(type, system) {
    types <- bioc_pkg_types()
    type <- match.arg(type, types$type)
    types[[system]][types$type == type]
}
