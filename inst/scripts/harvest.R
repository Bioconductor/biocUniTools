library(curl)
library(dplyr)
library(logger)

# Set the following: 
#
# OS <- "windows"
# MACOSX_NAME <- NULL
# ARCH <- NULL
# BIOC_VERSION <- "3.23"
# REPO_ROOT <- file.path("/home/biocpush/PACKAGES", BIOC_VERSION, "bioc")
# REMOVE_OLD_BINARIES_TEST <- TRUE
# LOG_PATH <- file.path("/home/biocpush/cron.log", BIOC_VERSION,
#                       paste0("harvest-", OS,".log"))

OS <- ""
MACOSX_NAME <- NULL
ARCH <- NULL
BIOC_VERSION <- ""
REMOVE_OLD_BINARIES_TEST <- TRUE 
REPO_ROOT <- file.path("/home/biocpush/PACKAGES", BIOC_VERSION, "bioc")
LOG_PATH <- file.path("/home/biocpush/cron.log", BIOC_VERSION,
                      paste0("harvest-", OS,".log"))


# set max.print to get all packages
options(max.print = 3000L)

logger::log_appender(logger::appender_file(LOG_PATH))
logger::log_info((Sys.time(), "Start"))

bioc_info <- get_uni_for_bioc_version(BIOC_VERSION)
repo_path <- get_repository_path(REPO_ROOT, bioc_info$r_version, OS)
binaries <- list.files(repo_path, pattern = ".*._[0-9]+.[0-9]+.[0-9]+..*")
pkgs <- get_uni_pkgs(bioc_info$ru_uni, bioc_info$bioc_branch,
                     bioc_info$r_version, bioc_version = BIOC_VERSION,
                     os = OS, macosx_name = MACOSX_NAME, arch = ARCH)
candidates <- pkgs |>
    dplyr::filter(!File %in% binaries)

downloaded <- curl::multi_download(candidates$Url)
logger::log_info("Download {downloaded}")
removed <- remove_old_binaries(REPO_ROOT, OS, test = REMOVE_OLD_BINARIES_TEST)
logger::log_info("Removed {removed}")

logger::log_info((Sys.time(), "End"))
