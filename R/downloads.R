#' Default location of Bioconductor download scores
#'
#' @description
#' `{dir}` and `{prefix}` are filled in by `bioc_downloads()`. Override with
#' `options(biocUniTools.stats_url = "<new template>")`.
#'
#' @noRd
.STATS_URL <-
    "https://bioconductor.org/packages/stats/{dir}/{prefix}_pkg_scores.tab"

#' Get Bioconductor download statistics and rank
#'
#' @description
#' The download score is the number of distinct IPs that downloaded a package
#' in the last 12 months. The rank is among packages of the same type in the
#' branch, as on bioconductor.org package pages: 1 is the most downloaded.
#'
#' @param branch character "devel" or "release"
#' @param type character one of `bioc_pkg_types()$type`
#'
#' @returns data.frame with columns Package, downloads_12mo, downloads_month
#'     (`downloads_12mo / 12`), download_rank, and ranked (number of packages
#'     ranked). Empty for types without download statistics, such as books.
#'
#' @examples
#' \dontrun{
#' bioc_downloads("release", "workflows")
#' }
#'
#' @export
bioc_downloads <- function(branch, type = "software") {
    out <- data.frame(Package = character(), downloads_12mo = integer(),
                      downloads_month = numeric(), download_rank = integer(),
                      ranked = integer())
    stats <- .type_as(type, "stats")
    if (is.na(stats))
        return(out)
    dir <- if (stats == "experiment") "data-experiment" else stats
    url <- getOption("biocUniTools.stats_url", .STATS_URL)
    url <- sub("{dir}", dir, url, fixed = TRUE)
    url <- sub("{prefix}", stats, url, fixed = TRUE)
    scores <- utils::read.delim(url)
    pkgs <- bioc_manifest(branch, type)
    scores <- scores[scores$Package %in% pkgs, ]
    data.frame(Package = scores$Package,
               downloads_12mo = as.integer(scores$Download_score),
               downloads_month = scores$Download_score / 12,
               download_rank = as.integer(rank(-scores$Download_score,
                                               ties.method = "min")),
               ranked = nrow(scores))
}
