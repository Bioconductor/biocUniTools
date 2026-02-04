#' Limit for number of records to retrieve using universe package
#' @noRd
.LIMIT <- 3000L

#' Get x.y version of R given x.y.z
#'
#' @examples
#' r_xy_ver("4.5.2")
#'
#' @export
r_xy_ver <- function(version) {
    xyz <- strsplit(version, "\\.")
    paste(xyz[[1]][1], xyz[[1]][2], sep=".")
}

#' Check if the first version is greater the second 
#'
#' @examples
#' is_greater_than("0.10.3", "1.0.20")
#'
#' @export
is_greater_than <- function(version1, version2) {
    parse_version <- function(v) {
        xyz <- stringr::str_extract(v, "[0-9]+\\.[0-9]+\\.[0-9]+")
        if (is.na(xyz)) stop(paste("Invalid version format:", v))
        as.integer(strsplit(xyz, "\\.")[[1]])
    }

    v1 <- parse_version(version1)
    v2 <- parse_version(version2)

    for (i in 1:3) {
        if (v1[i] < v2[i]) return(FALSE)
        if (v1[i] > v2[i]) return(TRUE)
    }
    TRUE
}

#' Construct binary file name
#'
#' @examples
#' uni_pkg_file("bedbaser", "windows", "1.0.12"))
#'
#' @export
uni_pkg_file <- function(pkg, os, version) {
    if (stringr::str_detect(os, "win"))
        ext <- "zip"
    else if (stringr::str_detect(os, "mac"))
        ext <- "tgz"
    else
        ext <- "tar.gz"
    paste0(pkg, "_", version, ".", ext)
}

#' Get macosx repo subpath by R version
#'
#' @param r_version R X.Y character
#' @param arch character x86_64 or arm64
#'
#' @return character
#'
#' @export
get_macosx_subpath <- function(r_version, arch) {
    if (as.double(r_version) >= 4.6 && arch == "arm64")
        subpath <- paste0("sonoma", "-", arch)
    else
        subpath <- paste0("big-sur", "-", arch)
    subpath
}

#' Get repo path
#'
#' @param r_version character
#' @param bioc_version character
#' @param os character
#' @param macosx_name big-sur or sonoma
#' @param arch character x86_64 or arm64
#'
#' @return character
#'
#' @export
get_repository_path <- function(repo_root, r_version, os, macosx_name = NULL,
                                arch = NULL) {
    if (stringr::str_detect(os, "mac")) {
            if (is.null(arch) | is.null(macosx_name))
                stop("macosx_name and arch must not be NULL for os == macosx")
            else if (!stringr::str_detect(macosx_name, "^(big-sur|ventura|sonoma|sequoia)$"))
                stop("macosx_name must be big-sur, sonoma, or sequoia")
            else if (!stringr::str_detect(arch, "^(x86_64|arm64)$"))
                stop("arch must be x86_64 or arm64")
    }

    if (stringr::str_detect(os, "mac"))
        os <- "macosx"
    else if (stringr::str_detect(os, "mac"))
        os <- "windows"

    subpath <- ""
    if (stringr::str_detect(os, "win|mac")) {
        subpath <- file.path("bin", os)
        r_xy <- r_xy_ver(r_version)
        if (stringr::str_detect(os, "mac") && !is.null(arch) &&
            !is.null(macosx_name))
            subpath <- file.path(subpath, get_macosx_subpath(r_xy, arch))
        subpath <- file.path(subpath, "contrib", r_xy)
    } else
        subpath <- file.path("src/contrib")
    subpath
    file.path(repo_root, subpath)
}

#' Get universe URL for an os and R version
#'
#' @examples
#' uni_repo_url("bioc", "4.5.3", "windows"))
#'
#' @export
uni_repo_url <- function(uni, r_version, os, macosx_name = NULL, arch = NULL) {
    universe <- paste0("https://", uni, ".r-universe.dev")
    get_repository_path(universe, r_version, os, macosx_name, arch)
}

#' Get OS abbreviation
#'
#' @return character abbreviation
#' 
#' @examples
#' get_binary_os("windows")
#'
#' @export
get_binary_os <- function(os) {
    if (stringr::str_detect(os, "win") | stringr::str_detect(os, "mac"))
        os <- substr(os, 1, 3)
    os
}

#' Get information about an R Universe build
#'
#' @param uni character universe name
#' @param uni_os_branch character release or devel
#' @param r_version character
#' @param os character
#' @param macosx_name big-sur or sonoma
#' @param arch character x86_64 or arm64
#'
#' @return character abbreviation
#'
#' @examples
#' get_uni_pkgs("bioc", "devel", "4.6.0", "windows")
#'
#' @export
get_uni_pkgs <- function(uni, uni_os_branch, r_version, os, macosx_name = NULL,
                         arch = NULL, verbose = FALSE) {
    r_xy <- r_xy_ver(r_version)
    # if macosx, adjust "macos", the term jobs use in the API
    if (stringr::str_detect(os, "mac"))
        os <- "macos"
    if (stringr::str_detect(os, "linux"))
        arch <- "x86_64"
    ru_info <- universe::universe_all_packages(uni, limit = .LIMIT)

    ru_builds <- data.frame(Package = character(),
                            Version = character(),
                            JobUrl = character(),
                            JobCheck = character(),
                            BinariesCheck = character(),
                            BinariesUrl = character(),
                            BinariesBuildDate = character(),
                            BinariesStatus = character(),
                            RU_commit = character(),
                            MD5sum = character(),
                            RemoteSha = character(),
                            RemoteUrl = character(),
                            Published = character(),
                            File = character(),
                            Url = character())

    uni_repo <- uni_repo_url(uni, r_version, os, macosx_name, arch)

    for (i in seq_along(ru_info)) {
        jobid <- ""
        jobcheck <- ""
        binaries_buildurl <- ""
        binaries_status <- ""
        binaries_check <- ""
        binaries_builddate <- ""
        binaries_version <- ""
        ru_commit <- ""

        for (j in seq_along(ru_info[[i]]$`_jobs`)) {
            job <- ru_info[[i]]$`_jobs`[[j]]
            os_branch <- paste(os, uni_os_branch, sep = "-")
            if (!is.null(arch))
                os_branch <- paste(os_branch, arch, sep = "-")
            if (job$config == os_branch && r_xy_ver(job$r) == r_xy) {
                jobid <- as.character(job$job)
                jobcheck <- job$check
                break
            }
        }

        for (k in seq_along(ru_info[[i]]$`_binaries`)) {
            binary <- ru_info[[i]]$`_binaries`[[k]]
            if (binary$os == get_binary_os(os) && r_xy_ver(binary$r) == r_xy) {
                ru_commit <- as.character(binary$commit)
                binaries_buildurl <- binary$buildurl
                binaries_status <- binary$status
                binaries_check <- binary$check
                binaries_builddate <- binary$date
                binaries_version <- binary$version
                break
            }
        }

        pkg_file <- uni_pkg_file(ru_info[[i]]$Package, os, binaries_version)
        ru_url <- ifelse((binaries_check %in% c("FAIL", "ERROR", "CANCELLED")) |
                         binaries_status != "success", "",
                         file.path(uni_repo, pkg_file))
        ru_url <- as.character(ru_url)
        ru_file <- ifelse((binaries_check %in% c("FAIL", "ERROR", "CANCELLED")) |
                          binaries_status != "success", "", pkg_file)
        ru_file <- as.character(ru_file)
        ru_builds <- tibble::add_row(ru_builds,
                                     Package = ru_info[[i]]$Package,
                                     Version = ru_info[[i]]$Version,
                                     JobUrl = paste0(ru_info[[i]]$`_buildurl`,
                                                     "/job/", jobid),
                                     JobCheck = jobcheck,
                                     BinariesCheck = binaries_check,
                                     BinariesUrl = binaries_buildurl,
                                     RU_commit = substr(ru_commit, 1, 7),
                                     MD5sum = substr(ru_info[[i]]$MD5sum, 1, 7),
                                     RemoteSha = substr(ru_info[[i]]$RemoteSha, 1, 7),
                                     RemoteUrl = ru_info[[i]]$RemoteUrl,
                                     Published = ru_info[[i]]$`_published`,
                                     BinariesBuildDate = binaries_builddate,
                                     BinariesStatus = binaries_status,
                                     Url = ru_url,
                                     File = ru_file)
    }

    ru_builds |>
        dplyr::arrange(Package)
}

#' Get information about comparable builds in R Universe and the BBS
#'
#' Collects information from the BBS for a Bioconductor version and finds
#' information about corresponding package versions in R Universe
#'
#' @param uni character universe name
#' @param uni_os_branch character release or devel
#' @param r_version character
#' @param bioc_version character
#' @param os character
#' @param macosx_name big-sur or sonoma
#' @param arch character x86_64 or arm64
#'
#' @return character abbreviation
#'
#' @examples
#' get_comparable_pkgs("bioc", "devel", "4.6.0", "3.23", "windows")
#'
#' @export
get_comparable_pkgs <- function(uni, uni_os_branch, r_version, bioc_version, os,
                                macosx_name = NULL, arch = NULL,
                                verbose = FALSE) {
    ru_builds <- get_uni_pkgs(uni, uni_os_branch, r_version, os, macosx_name,
                              arch, verbose)
    bbs_info <- BiocPkgTools::biocBuildReport(version = bioc_version,
                                              pkgType = "software")
    bbs_builds <- bbs_info |>
        dplyr::rename(Package = pkg, Version = version,
                      BBS_commit = git_last_commit) |>
        dplyr::select(Package, Version, BBS_commit, Deprecated) |>
        dplyr::distinct()

    builds <- merge(bbs_builds, ru_builds, by = c("Package", "Version"))

    if (verbose) {
        print("Number of Packages")
        print(paste("BBS:", dim(bbs_builds)[1]))
        print(paste("R Universe:", dim(ru_builds)[1]))
        print(paste("Merged:", dim(builds)[1]))
    }

    builds |>
        dplyr::arrange(Package)
}

#' Get packages matching criteria
#' 
#' @param commit logical Check commit hash in RU and BBS?
#' @param check character vector of acceptable R CMD check statuses
#'
#' @return data.frame of filtered candidate packages
#'
#' @examples
#' comparable_pkgs <- get_comparable_pkgs("bioc", "devel", "4.6.0", "windows",
#'                                        "3.23")
#' pkgs <- get_candidates(comparable_pkgs, commit = TRUE)
#'
#' @export
get_candidates <- function(pkgs, commit = FALSE,
                           check = c("NOTE", "WARNING", "OK")) {
    candidates <- pkgs |>
        dplyr::filter(BinariesCheck %in% check,
                      JobCheck %in% check,
                      BinariesStatus == "success")

    if (commit) {
        candidates <- candidates |>
            dplyr::filter(BBS_commit == RU_commit)
    }

    candidates
}

#' Get information about R Universe building a Bioconductor version
#'
#' @param version character Bioconductor version
#'
#' @export
get_uni_for_bioc_version <- function(version) {
    bioc_yaml <- yaml::read_yaml("https://bioconductor.org/config.yaml")
    
    stopifnot(version %in% bioc_yaml$versions)
    bioc_branch <- ifelse(bioc_yaml$devel_version == version, "devel",
                          "release")
    ru_uni <- ifelse(bioc_branch == "devel", "bioc", "bioc-release")
    if (bioc_branch == "devel")
        r_version <- bioc_yaml$r_version_associated_with_devel
    else
        r_version <- bioc_yaml$r_version_associated_with_release

    list(bioc_version = version,
         bioc_branch = bioc_branch,
         ru_uni = ru_uni,
         r_version = r_version)
}

#' Remove old binaries if a new binary exists
#'
#' @param repo_root path that includes repo type--e.g.,
#'     "/home/biocpush/PACKAGES/3.22/bioc"
#' @param os name, full or abbreviation
#' @param macosx_name big-sur or sonoma
#' @param arch x86_64 or arm64
#' @param test logical (default TRUE) don't remove, only print packages marked
#'     for removal
#'
#' @return vector of the full path of binaries removed
#'
#' @examples
#' remove_old_binaries("/home/biocpush/PACKAGES/3.22/bioc", "4.6.0", "windows")
#'
#' @export
remove_old_binaries <- function(repo_root, r_version, os, macosx_name = NULL,
                                arch = NULL, test = TRUE) {
    binaries_path <- get_repository_path(repo_root, r_xy_ver(r_version), os,
                                         macosx_name, arch)
    files <- list.files(binaries_path, pattern=".*._[0-9]+\\.[0-9]+\\.[0-9]+\\..*")
    binaries <- data.frame(file = files,
                           latest = NA)
    binaries <- binaries |>
        dplyr::mutate(pkg = sub("_[^_]*$", "", file),
                      full_path = file.path(binaries_path, file),
                      version = stringr::str_extract(file, "(?<=_)\\d+\\.\\d+\\.\\d+(?=\\.)")) |>
        dplyr::arrange(pkg)

    latest <- NULL
    for (i in seq_len(nrow(binaries))) {

       if (is.null(latest)) {
           latest <- i
           next
       }

       if (binaries$pkg[latest] != binaries$pkg[i]) {
           latest <- i
           binaries$latest[latest] <- TRUE
           next
       }

       if (binaries$pkg[latest] == binaries$pkg[i]) {
           if (is_greater_than(binaries$version[latest],
                               binaries$version[i])) {
               binaries$latest[latest] <- TRUE
               binaries$latest[i] <- FALSE
           } else if (is_greater_than(binaries$version[i],
                                      binaries$version[latest])) {
               binaries$latest[i] <- TRUE
               binaries$latest[latest] <- FALSE
               latest <- i
           }
       }
    }

    if (!is.na(latest)) {
        binaries$latest[latest] <- TRUE
    }

   if (any(is.na(binaries$latest))) {
         warning("Some packages were not properly marked as latest/old")
         print(binaries[is.na(binaries$latest), ])
   }

    old_binaries <- binaries |>
        dplyr::filter(latest == FALSE)

    for (b in old_binaries$full_path) {
        if (!test)
            file.remove(b)
    }
    old_binaries$full_path
}
