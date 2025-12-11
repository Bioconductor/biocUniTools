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

#' Check if the version in a file is greater than another
#'
#' @examples
#' is_greater_than("bedbaser_0.10.3.tar.gz", "bedbaser_1.0.20.tar.gz")
#'
#' @export
is_greater_than <- function(file1, file2) {
    fs <- c()
    for (f in c(file1, file2)) {
        xyz <- stringr::str_extract(f, "[0-9]+.[0-9]+.[0-9]+")
        fs[length(fs)+1] <- strsplit(xyz, "\\.")
    }
  
    for (i in seq(1, 3)) {
        if (as.integer(fs[[1]][i]) < as.integer(fs[[2]][i]))
            return(FALSE)
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
    if (os == "windows")
        ext <- "zip"
    else if (os == "macosx")
        ext <- "tgz"
    else
        ext <- "tar.gz"
    paste0(pkg, "_", version, ".", ext)
}

#' Get universe URL for an os and R version
#'
#' @examples
#' uni_repo_url("bioc", "4.5.3", "windows"))
#'
#' @export
uni_repo_url <- function(uni, r_version, os, arch = NULL) {
    if (os == "macosx" && arch == "x86_64")
        repo_path <- paste(c("bin", os, "big-sur-x86_64"), collapse = "/")
    else if (os == "macosx" && arch == "arm64")
      repo_path <- paste(c("bin", os, "big-sur-arm64"), collapse = "/")
    else if (os == "windows")
        repo_path <- paste(c("bin", os), collapse = "/")
    else
        repo_path <- "src"
    root <- paste0("https://", uni, ".r-universe.dev")
    paste(c(root, repo_path, "contrib", r_xy_ver(r_version), ""), collapse = "/")
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
#' @param arch character x86_64 or arm64
#' 
#' @return character abbreviation
#' 
#' @examples
#' get_uni_pkgs("bioc", "devel", "4.6.0", "3.23", "windows")
#'
#' @export
get_uni_pkgs <- function(uni, uni_os_branch, r_version, bioc_version, os,
                         arch = NA, verbose = FALSE) {
  r_xy <- r_xy_ver(r_version)
  # if macosx, adjust "macos", the term jobs use in the API
  if (stringr::str_detect(os, "mac"))
      os <- "macos"
  ru_info <- universe::universe_all_packages(uni, limit = .LIMIT)
  bbs_info <- BiocPkgTools::biocPkgList(version = bioc_version,
                                        repo="BioCsoft")
  bbs_builds <- bbs_info |>
      dplyr::mutate(BBS_commit = git_last_commit) |>
      dplyr::select(Package, Version, BBS_commit)
  
  ru_builds <- data.frame(Package = "",
                          Version = "",
                          JobUrl = "",
                          JobCheck = "",
                          BinariesCheck = "",
                          BinariesUrl = "",
                          BinariesBuildDate = "",
                          BinariesStatus = "",
                          RU_commit = "",
                          MD5sum = "",
                          RemoteSha = "",
                          RemoteUrl = "",
                          Published = "",
                          File = "",
                          Url = "")
  
  uni_repo <- uni_repo_url(uni, r_version, os, arch)
  
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
      if (!is.na(arch))
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
    ru_url <- ifelse(!(binaries_check %in% c("FAIL", "ERROR")) &
                     binaries_status == "success",
                     paste0(uni_repo, pkg_file), "")
    ru_file <- ifelse(!(binaries_check %in% c("FAIL", "ERROR")) &
                      binaries_status == "success", pkg_file, "")
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
#' @param version logical Check package version in RU and BBS?
#' @param check character vector of acceptable R CMD check statuses
#' 
#' @return data.frame of filtered candidate packages
#' 
#' @examples
#' uni_pkgs <- get_uni_pkgs("bioc", "devel", "4.6.0", "windows", "3.23")
#' pkgs <- get_candiates(uni_pkgs, commit = TRUE, version = TRUE)
#'
#' @export
get_candidates <- function(pkgs, commit = FALSE, version = FALSE,
                           check = c("NOTE", "WARNING", "OK")) {
    candidates <- pkgs |>
        dplyr::filter(BinariesCheck %in% check,
                      JobCheck %in% check,
                      BinariesStatus == "success")
  
    if (commit) {
        candidates <- pkgs |>
            dplyr::filter(BBS_commit == RU_commit)      
    }
    
    if (version) {
      candidates <- pkgs |>
        dplyr::filter(BBS_version == RU_version)      
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
    bioc_branch <- ifelse(bioc_yaml$devel_version == version, "devel", "release")
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
#' @param pkgs Data.Frame of packages that includes new binaries
#' @param repo_root path that includes repo type--e.g.,
#'     "/home/biocpush/PACKAGES/3.22/bioc"
#' @param os name, full or abbreviation
#' @param test logical (default TRUE) don't remove, only print packages marked
#'     for removal
#'
#' @return An invisible \code{NULL}
#'
#' @examples
#' allpkgs <- get_uni_pkgs("bioc", "devel", "3.23", "windows")
#' pkgs <- get_candidates(allpkgs)
#' remove_binaries(pkgs, "/home/biocpush/PACKAGES/3.22/bioc", "windows")
#'
#' @export
remove_binaries <- function(pkgs, repo_root, os, test = TRUE) {
    r_version <- R.Version()
    r_version <- paste0(r_version, strsplit(r_version$minor, "\\.")[[1]][1])
    sub_path <- ifthen(os %in% c("windows", "macosx"), "bin", "src")
    binaries_path <- file.path(repo_root, sub_path, get_binary_os(os),
                               "contrib", r_version)
    binaries <- list.files(binaries_path)
    names(binaries) <- sapply(binaries,
                              function(x) { stringr::str_split_1(x, "_")[1] }) |>
        unname()
    for (b in names(binaries)) {
        if (b %in% pkgs$Package & is_greater_than(pkg$File, binaries[b])) {
            full_path <- tools::file_path_as_absolute(binaries_path,
                                                      binaries[b])
            print(paste("Removing", full_path))
            if (!test)
                file.remove(full_path)
        }
    }
}

#' Wrapper for get_uni_pkgs, pass information for correct R Universe associated
#' with a Bioductor version for a specific OS
#' 
#' @param os character
#' @param version character Bioconductor version
#'
#' @export
get_candidate_pkgs_for_bioc_version <- function(os, version) {
    bioc_info <- get_uni_for_bioc_version(version)
    pkgs <- get_uni_pkgs(bioc_info$ru_uni, bioc_info$bioc_branch,
                         bioc_info$r_version, version, os)
    get_candidates(pkgs, commit = TRUE, version = TRUE)
}
