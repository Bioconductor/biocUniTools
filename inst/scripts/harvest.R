library(curl)
library(dplyr)
library(readr)

# Set the following: 
#
# OS <- "windows"
# BIOC_VERSION <- "3.23"
# REMOVE_OLD_BINARIES <- TRUE
# REPO_ROOT <- file.path("/home/biocpush/PACKAGES", BIOC_VERSION, "bioc")
# REMOVE_OLD_BINARIES_TEST <- TRUE
# LOG_PATH <- file.path("/home/biocpush/cron.log", BIOC_VERSION,
#                       paste0("harvest-", OS,".log"))

OS <- ""
BIOC_VERSION <- ""
REMOVE_OLD_BINARIES <- FALSE
REPO_ROOT <- file.path("/home/biocpush/PACKAGES", BIOC_VERSION, "bioc")
REMOVE_OLD_BINARIES_TEST <- TRUE
LOG_PATH <- file.path("/home/biocpush/cron.log", BIOC_VERSION,
                      paste0("harvest-", OS,".log"))


# set max.print to get all packages
options(max.print = 3000L)

pkgs <- get_candidate_pkgs_for_bioc_version(OS, BIOC_VERSION)

if (REMOVE_OLD_BINARIES)
    remove_binaries(pkgs, REPO_ROOT, OS, REMOVE_OLD_BINARIES_TEST)

result <- curl::multi_download(pkgs$Url)
readr::write_csv(result, LOG_PATH)
