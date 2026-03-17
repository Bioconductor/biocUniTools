#' Limit for number of records to retrieve using universe package
#' @noRd
.LIMIT <- 3000L

#' Get x.y version of R given x.y.z
#'
#' @param version character
#'
#' @examples
#' r_xy_ver("4.5.2")
#'
#' @export
r_xy_ver <- function(version) {
    x <- stringr::str_split_i(version, "\\.", 1)
    y <- stringr::str_split_i(version, "\\.", 2)
    paste(x, y, sep = ".")
}

#' Check if the first version is greater the second 
#'
#' @param version1 character
#' @param version2 character
#'
#' @returns logical
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
#' @param pkg character package name
#' @param os character OS name
#' @param version character package version
#'
#' @returns character file with extension
#'
#' @examples
#' uni_pkg_file("bedbaser", "windows", "1.0.12")
#'
#' @export
uni_pkg_file <- function(pkg, os, version) {
    ext <- dplyr::case_when(
        stringr::str_detect(os, "win") ~ "zip",
        stringr::str_detect(os, "mac") ~ "tgz",
        .default = "tar.gz"
    )
    paste0(pkg, "_", version, ".", ext)
}

#' Get macosx repo subpath by R version
#'
#' @param r_version R X.Y character
#' @param arch character x86_64 or arm64
#'
#' @returns character
#' 
#' @examples
#' get_macosx_subpath("4.6", "arm64")
#' 
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
#' @returns character
#'
#' @examples
#' get_repository_path("/home/biocpush/PACKAGES/3.22/bioc", "4.6.0", "windows")
#'
#' @export
get_repository_path <- function(repo_root, r_version, os, macosx_name = NULL,
                                arch = NULL) {
    if (stringr::str_detect(os, "mac")) {
        if (is.null(arch) | is.null(macosx_name))
            stop("macosx_name and arch must not be NULL for os == macosx")
        else if (stringr::str_detect(macosx_name, "sonoma") &&
                 (as.numeric(r_xy_ver(r_version)) < 4.6 || 
                  stringr::str_detect(arch, "x86_64")))
            stop("sonoma binaries are for arch == arm64 and >= R 4.6")
        else if (!stringr::str_detect(macosx_name,
                                      "^(big-sur|ventura|sonoma|sequoia)$"))
            stop("macosx_name must be big-sur or sonoma")
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
#' @param uni character universe name
#' @param r_version character
#' @param os character
#' @param macosx_name big-sur or sonoma
#' @param arch character x86_64 or arm64
#'
#' @returns character
#'
#' @examples
#' uni_repo_url("bioc", "4.5.3", "windows")
#'
#' @export
uni_repo_url <- function(uni, r_version, os, macosx_name = NULL, arch = NULL) {
    universe <- paste0("https://", uni, ".r-universe.dev")
    get_repository_path(universe, r_version, os, macosx_name, arch)
}

#' Get OS abbreviation
#'
#' @param os character
#'
#' @returns character abbreviation
#' 
#' @examples
#' get_binary_os("windows")
#'
#' @export
get_binary_os <- function(os) {
    ifelse(stringr::str_detect(os, "win") | stringr::str_detect(os, "mac"),
           substr(os, 1, 3),
           os)
}

#' Get packages in an R Universe
#'
#' @param uni character universe name
#'
#' @returns data.frame packages in a universe
#'
#' @examples
#' get_raw_uni_df("bioc", "devel", "4.6.0")
#'
#' @export
get_raw_uni_df <- function(uni) {
    uni_api_url <- paste0("https://", uni, ".r-universe.dev/api/packages/")
    pkgs <- httr2::request(uni_api_url) |>
        httr2::req_url_query(limit = as.integer(.LIMIT)) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = FALSE)
    jsonlite::fromJSON(jsonlite::toJSON(pkgs, auto_unbox = TRUE), flatten = TRUE)
}

#' Flatten raw universe data.frame by matching R, OS, and arch information from
#'  `_jobs` with `_binaries`
#' 
#' @description
#' Adds additional columns to `_jobs_r_xy`, `_binaries_r_xy`,
#' `_jobs_type`, `_jobs_arch`, `_jobs_os_`, `_binaries_os_`
#' 
#' @param raw_df raw data.frame from R Universe API
#' 
#' @returns data.frame
#' 
#' @examples
#' raw_df <- get_raw_uni_df("bioc", "devel", "4.6.0")
#' get_uni_df(raw_df)
#'
#' @export
get_uni_df <- function(raw_df) {
    uni_df <- raw_df |>
        tidyr::unnest(`_jobs`, names_sep = "_", names_repair = "unique") |>
        dplyr::mutate(
            `_jobs_r_xy` = r_xy_ver(`_jobs_r`),
            `_jobs_type` = dplyr::case_when(
                stringr::str_detect(`_jobs_config`, "bioc-checks") ~ "bioc-checks",
                stringr::str_detect(`_jobs_config`, "source") ~ "source",
                .default = "binary"
            ),
            `_jobs_arch` = dplyr::case_when(
                stringr::str_detect(`_jobs_config`, "arm64") ~ "arm64",
                stringr::str_detect(`_jobs_config`, "x86_64|windows") ~ "x86_64",
                stringr::str_detect(`_jobs_config`, "wasm") ~ "emscripten",
                .default = NA
            ),
            `_jobs_os_` = dplyr::if_else(`_jobs_type` == "binary",
                                         get_binary_os(stringr::str_split_i(`_jobs_config`, "-", 1)),
                                         "linux")
        ) |>
        (\(df) dplyr::bind_rows(
            df |>
                dplyr::filter(`_jobs_type` == "binary") |>
                tidyr::unnest(`_binaries`, names_sep = "_", names_repair = "unique") |>
                dplyr::mutate(`_binaries_os_` = get_binary_os(`_binaries_os`),
                              `_binaries_r_xy` = r_xy_ver(`_binaries_r`)) |>
                dplyr::filter(
                    `_jobs_os_` == `_binaries_os_`,
                    dplyr::case_when(
                        is.na(`_binaries_arch`) ~ TRUE,
                        `_jobs_arch` == "arm64" ~ `_binaries_arch` == "aarch64",
                        .default = `_jobs_arch` == `_binaries_arch`
                    )
                ),
            df |>
                dplyr::filter(`_jobs_type` %in% c("bioc-check", "source")) |>
                dplyr::select(-`_binaries`)
        ))()

    dplyr::filter(uni_df, `_jobs_r_xy` == `_binaries_r_xy`)
}

#' Get binaries for an R Universe build by OS, R version, and arch
#'
#' @param uni character universe name
#' @param uni_os_branch character release or devel
#' @param r_version character
#' @param bioc_version character
#' @param os character
#' @param macosx_name big-sur or sonoma
#' @param arch character x86_64 or arm64
#'
#' @returns data.frame packages in a universe
#'
#' @examples
#' get_binaries_by_os("bioc", "devel", "4.6.0", "3.23", "windows")
#'
#' @export
get_binaries_by_os <- function(uni, uni_os_branch, r_version, bioc_version, os,
                               macosx_name = NULL, arch = NULL) {
    # if macosx, adjust "macos", the term jobs use in the API
    if (stringr::str_detect(os, "mac"))
        os <- "macos"
    if (stringr::str_detect(os, "win"))
        arch <- "x86_64"

    uni_repo <- uni_repo_url(uni, r_version, os, macosx_name, arch)

    raw_uni_df <- get_raw_uni_df(uni)
    # Packages without an explicit _binaries_arch have universal binaries
    uni_df <- get_uni_df(raw_uni_df) |>
        dplyr::filter(
            `_jobs_r_xy` == r_xy_ver(r_version),
            `_jobs_os_` == get_binary_os(os),
            if (arch == "arm64")
                `_binaries_arch` == "aarch64"
            else
                `_binaries_arch` != "aarch64" | is.na(`_binaries_arch`)
        )

    updated_uni_df <- uni_df |>
        dplyr::mutate(Artifact = ifelse(`_binaries_check` %in% c("FAIL", "ERROR", "CANCELLED") &
                                        `_jobs_check` %in% c("FAIL", "ERROR", "CANCELLED") &
                                        `_binaries_status` == "failure",
                                        NA,
                                        file.path(uni_repo,
                                                  uni_pkg_file(Package, os,
                                                               `_binaries_version`))),
                      JobUrl = ifelse(is.na(`_jobs_job`),
                                      NA,
                                      file.path(`_buildurl`, "job", `_jobs_job`))
        )|>
        dplyr::arrange(Package)
    updated_uni_df
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
#' @param verbose
#'
#' @returns character abbreviation
#'
#' @examples
#' get_comparable_pkgs("bioc", "devel", "4.6.0", "3.23", "windows")
#'
#' @export
get_comparable_pkgs <- function(uni, uni_os_branch, r_version, bioc_version, os,
                                macosx_name = NULL, arch = NULL,
                                verbose = FALSE) {
    ru_builds <- get_binaries_by_os(uni, uni_os_branch, r_version,  bioc_version,
                                    os, macosx_name, arch)
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
#' @param data.frame of R Universe packages
#' @param commit logical Check commit hash in RU and BBS?
#' @param check character vector of acceptable R CMD check statuses
#'
#' @returns data.frame of filtered candidate packages
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
        dplyr::filter(`_binaries_check` %in% check,
                      `_jobs_check` %in% check,
                      `_binaries_status` == "success")

    if (commit) {
        candidates <- candidates |>
            dplyr::filter(BBS_commit == substr(RemoteSha, 1, 7))
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
#' @param r_version R x.y.z version
#' @param os name, full or abbreviation
#' @param macosx_name big-sur or sonoma
#' @param arch x86_64 or arm64
#' @param test logical (default TRUE) don't remove, only print packages marked
#'     for removal
#'
#' @returns vector of the full path of binaries removed
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
