library(curl)
library(dplyr)
library(logger)
library(biocUniTools)


# Usage: Rscript harvest.R [TEST_REMOVAL] [BIOC_VERSION] [OS] [MACOSX_NAME] [ARCH]
# MACOSX_NAME and ARCH are only for macosx
# Example: Rscript harvest.R 3.22 macosx sequoia arm64

msg <- "Usage: Rscript harvest.R [TEST_REMOVAL] [BIOC_VERSION] [OS] [MACOSX_NAME] [ARCH]"
args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 3)
    stop(msg, call. = FALSE)

stopifnot(args[1] %in% c("TRUE", "FALSE"))
TEST_REMOVAL <- as.logical(args[1])

bioc_info <- tryCatch(
    get_uni_for_bioc_version(args[2]),
    error = function(x) {
        stop(args[2], " not a current or valid Bioconductor version")
    })
BIOC_VERSION <- bioc_info$bioc_version

stopifnot(args[3] %in% c("windows", "linux", "macosx"))
OS <- args[3]

if (OS == "macosx" && length(args) != 5) {
    stop(msg, call.=FALSE)
} else if (OS == "macosx" && length(args) == 5) {
    stopifnot(args[4] %in% c("big-sur", "sequoia"))
    MACOSX_NAME <- args[4]
    stopifnot(args[5] %in% c("x86_64", "arm64"))
    ARCH <- args[5]
} else {
    MACOSX_NAME <- NULL
    ARCH <- NULL
}

REPO_ROOT <- file.path("/home/biocpush/PACKAGES", BIOC_VERSION, "bioc")
LOG_PATH <- file.path("/home/biocpush/cron.log", BIOC_VERSION,
                      paste0("harvest-", OS, "-",
                             format(Sys.Date(), format="%Y%m%d"), ".log"))


# set max.print to get all packages
options(max.print = 3000L)

logger::log_appender(logger::appender_file(LOG_PATH))
logger::log_info("{Sys.time()} Start")

repo_path <- get_repository_path(REPO_ROOT, bioc_info$r_version, OS, MACOSX_NAME, ARCH)
binaries <- list.files(repo_path, pattern = ".*._[0-9]+\\.[0-9]+\\.[0-9]+\\..*")
pkgs <- get_uni_pkgs(bioc_info$ru_uni, bioc_info$bioc_branch,
                     bioc_info$r_version, bioc_version = BIOC_VERSION,
                     os = OS, macosx_name = MACOSX_NAME, arch = ARCH)
candidates <- pkgs |>
    dplyr::filter(File != "" & !File %in% binaries)

downloaded <- curl::multi_download(candidates$Url)
logger::log_info("Downloaded {downloaded$success} {downloaded$status_code} {downloaded$destfile}")
removed <- remove_old_binaries(REPO_ROOT, bioc_info$r_version, OS,
                               test = TEST_REMOVAL)
logger::log_info("Removed {removed}")

logger::log_info("{Sys.time()} End")
