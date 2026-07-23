library(curl)
library(dplyr)
library(logger)
library(biocUniTools)


# Usage: Rscript harvest.R [TEST_REMOVAL] [BIOC_VERSION] [OS] [MACOSX_NAME] [ARCH]
# MACOSX_NAME and ARCH are only for macosx
# Example: Rscript harvest.R 3.22 macosx sonoma arm64

msg <- "Usage: Rscript harvest.R [TEST_REMOVAL] [BIOC_VERSION] [OS] [MACOSX_NAME] [ARCH]"
args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 3)
    stop(msg, call. = FALSE)

stopifnot(args[1] %in% c("TRUE", "FALSE"))
TEST_REMOVAL <- as.logical(args[1])

bu <- tryCatch(
    uni_for_bioc(args[2]),
    error = function(x) {
        stop(args[2], " not a current or valid Bioconductor version")
    })
BIOC_VERSION <- bu$bioc_version

stopifnot(args[3] %in% c("windows", "linux", "macosx"))
OS <- args[3]

if (OS == "macosx" && length(args) != 5) {
    stop(msg, call.=FALSE)
} else if (OS == "macosx" && length(args) == 5) {
    stopifnot(args[4] %in% c("big-sur", "sonoma"))
    MACOSX_NAME <- args[4]
    stopifnot(args[5] %in% c("x86_64", "arm64"))
    ARCH <- args[5]
    LOG_FILE_BASE <- paste("harvest", OS, MACOSX_NAME, ARCH, sep = "-")
} else {
    MACOSX_NAME <- NA
    ARCH <- NA
    LOG_FILE_BASE <- paste("harvest", OS, sep = "-")
}

REPO_ROOT <- file.path("/home/biocpush/PACKAGES", BIOC_VERSION, "bioc")
LOG_FILE <- paste0(LOG_FILE_BASE, "-", format(Sys.Date(), format="%Y%m%d"),
                   ".log")
LOG_PATH <- file.path("/home/biocpush/cron.log", BIOC_VERSION, LOG_FILE)

# set max.print to get all packages
options(max.print = 3000L)

logger::log_appender(logger::appender_file(LOG_PATH))
logger::log_info("{Sys.time()} Start")

repo_path <- get_repository_path(REPO_ROOT, bu$r_version, OS, MACOSX_NAME, ARCH)
candidates <- get_candidates(bu, os = OS, arch = ARCH, commit = TRUE)

# Remove any candidates from the list that are currently in the repository
binaries <- list.files(repo_path, pattern = ".*._[0-9]+\\.[0-9]+\\.[0-9]+\\..*")
candidates <- candidates |>
    dplyr::mutate(File = uni_pkg_file(Package, OS, Version)) |>
    dplyr::filter(!File %in% binaries)

# Check for version downgrades against what is currently in the repo
existing_versions <- data.frame(
    File = binaries,
    stringsAsFactors = FALSE
) |>
    dplyr::mutate(
        Package = sub("_[0-9]+\\.[0-9]+.*", "", File),
        Version = sub(".*_(([0-9]+\\.)+[0-9]+)\\..*", "\\1", File)
    )

downgrade_check <- candidates |>
    dplyr::inner_join(existing_versions, by = "Package", suffix = c("_new", "_current")) |>
    dplyr::filter(
        package_version(Version_new) < package_version(Version_current)
    )

if (nrow(downgrade_check) >= 1) {
    for (i in seq_len(nrow(downgrade_check))) {
        logger::log_warn(
            "VERSION DOWNGRADE DETECTED: {downgrade_check$Package[i]} - ",
            "current: {downgrade_check$Version_current[i]}, ",
            "incoming: {downgrade_check$Version_new[i]}. ",
            "Skipping download."
        )
    }
    candidates <- candidates |>
        dplyr::filter(!Package %in% downgrade_check$Package)
}

if (nrow(candidates) >= 1) {
    downloaded <- curl::multi_download(
        candidates$artifact,
        destfiles = file.path(repo_path, candidates$File)
    )
    
    # Log successful downloads
    successful <- downloaded[downloaded$success, ]
    if (nrow(successful) >= 1)
        logger::log_info("Downloaded {nrow(successful)} binaries: {successful$destfile}")
    
    # Log failed downloads
    failed <- downloaded[!downloaded$success, ]
    if (nrow(failed) >= 1) {
        logger::log_warn("Failed to download {nrow(failed)} binaries:")
        for (i in seq_len(nrow(failed))) {
            logger::log_warn("  {failed$url[i]} - status: {failed$status_code[i]}, error: {failed$error[i]}")
        }
        # Remove any partial downloads
        for (f in failed$destfile) {
            if (file.exists(f)) {
                file.remove(f)
                logger::log_info("Removed partial download: {f}")
            }
        }
    }
} else {
    logger::log_info("No new binaries available.")
}

prefix <- ifelse(TEST_REMOVAL, "[TEST] ", "")
removed <- remove_old_binaries(REPO_ROOT, bu$r_version, OS, MACOSX_NAME, ARCH,
                               test = TEST_REMOVAL)
if (length(removed) >= 1) {
    logger::log_info("{prefix}Removed {removed}")
} else {
    logger::log_info("{prefix}No binaries to remove.")
}

logger::log_info("{Sys.time()} End")
