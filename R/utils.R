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
    dplyr::if_else(is.na(version), NA_character_, paste(x, y, sep = "."))
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
    ifelse(as.double(r_version) >= 4.6 & arch == "arm64",
           subpath <- paste0("sonoma", "-", arch),
           subpath <- paste0("big-sur", "-", arch))
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
get_repository_path <- function(repo_root, r_version, os,
                                macosx_name = NA_character_,
                                arch = NA_character_) {
    r_xy <- r_xy_ver(r_version)
    
    is_mac <- !is.na(os) & stringr::str_detect(os, "mac")
    is_win <- !is.na(os) & stringr::str_detect(os, "win")

    if (any(is_mac & is.na(arch)))
        stop("arch must not be NA for os == macosx")
    if (any(is_mac & !stringr::str_detect(arch, "^(x86_64|arm64)$")))
        stop("arch must be x86_64 or arm64")
    if (any(is_mac & !is.na(macosx_name) &
            stringr::str_detect(macosx_name, "sonoma") &
            (as.numeric(r_xy) < 4.6 | stringr::str_detect(arch, "x86_64"))))
        stop("sonoma binaries are for arch == arm64 and >= R 4.6")
    if (any(is_mac & !is.na(macosx_name) &
            !stringr::str_detect(macosx_name, "^(big-sur|sonoma)$")))
        stop("macosx_name must be big-sur or sonoma")
    
    subpath <- dplyr::case_when(
        is_mac ~ file.path("bin", "macosx",
                           get_macosx_subpath(r_xy, ifelse(is.na(arch), "x86_64", arch)),
                           "contrib", r_xy),
        is_win ~ file.path("bin", "windows", "contrib", r_xy),
        .default = "src/contrib"
    )
    
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
uni_repo_url <- function(uni, r_version, os, macosx_name = NA_character_,
                         arch = NA_character_) {
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

#' Get the arch
#' 
#' @param binaries_arch binaries_arch
#' @param job_arch job_arch
#' 
#' @returns character
#'
#' @examples
#' get_arch(binaries_arch, "arm64")
get_arch <- function(binaries_arch, job_arch) {
    dplyr::case_when(is.na(binaries_arch) & job_arch == "arm64" ~ "x86_64",
                     binaries_arch == "aarch64" & job_arch == "arm64" ~ "arm64",
                     .default = job_arch)
}

#' Get packages in an R Universe
#'
#' @param uni character universe name
#'
#' @returns data.frame packages in a universe
#'
#' @examples
#' get_raw_uni_df("bioc")
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

#' Match binaries to a job by R version, OS, and arch
#'
#' @description
#' Helper for `get_uni_df`. Filters a binary data frame to only include binaries
#' that match the given job's R version, OS, and arch. If no `arch` column is
#' present in the binaries data frame, it is assumed to be a universal binary
#' and `arch` is set to `NA`.
#'
#' @param binaries data.frame
#' @param r_xy character
#' @param os_ character
#' @param arch character x86_64 or arm64
#'
#' @returns data.frame subset of `_binaries` matching the job's R version, OS,
#'     and arch. Returns an empty data.frame if no matches are found.
#'
#' @examples
#' binaries <- data.frame(
#'     r = c("4.5.3", "4.6.0"),
#'     os = c("linux", "linux"),
#'     version = c("1.0.0", "1.0.0")
#' )
#' match_binaries(binaries, "4.5", "linux", "x86_64")
#'
#' @noRd
match_binaries <- function(binaries, r_xy, os, arch) {
    if (nrow(binaries) == 0)
        return(binaries)
    
    if (!"arch" %in% names(binaries))
        binaries$arch <- NA_character_
    binaries_os_ <- get_binary_os(binaries$os)
    binaries_r_xy <- r_xy_ver(binaries$r)
    arch_match <- dplyr::case_when(
        is.na(binaries$arch) ~ TRUE,
        arch == "arm64" ~ binaries$arch == "aarch64",
        .default = binaries$arch == arch
    )
    binaries[binaries_os_ == os & binaries_r_xy == r_xy & arch_match, ]
}

#'  get_uni_df helper to expand `_jobs`
#' 
#' @description
#' Adds additional columns to job_r_xy, job_type, job_arch, job_os
#' 
#' @param df data.frame unprocessed data from R Universe API
#' 
#' @returns data.frame
#' 
#' @examples
#' raw_df <- get_raw_uni_df("bioc")
#' df <- .expand_jobs(raw_df)
#'
#' @export
.expand_jobs <- function(df) {
    df |>
        tidyr::unnest(`_jobs`, names_sep = "_", names_repair = "unique") |>
        dplyr::rename(job_id = `_jobs_job`, jobs_time = `_jobs_time`,
                      jobs_config = `_jobs_config`, jobs_r = `_jobs_r`,
                      jobs_check = `_jobs_check`,
                      jobs_artifact = `_jobs_artifact`) |>
        dplyr::mutate(
            job_r_xy = r_xy_ver(jobs_r),
            job_type = dplyr::case_when(
               stringr::str_detect(jobs_config, "bioc-checks") ~ "bioc-checks",
               stringr::str_detect(jobs_config, "source") ~ "source",
               .default = "binary"),
            job_arch = dplyr::case_when(
               stringr::str_detect(jobs_config, "arm64") ~ "arm64",
               stringr::str_detect(jobs_config, "x86_64|windows") ~ "x86_64",
               stringr::str_detect(jobs_config, "wasm") ~ "emscripten",
               .default = NA_character_),
            job_os = dplyr::if_else(job_type == "binary",
                                    get_binary_os(stringr::str_split_i(jobs_config, "-", 1)),
                                    "linux"))
}

#' get_uni_df helper to expand `_binaries` and match with `_job`-related columns
#' 
#' @description
#' Adds additional columns to binary_r_xy and binary_os
#' 
#' @param df data.frame data processed from R Universe API
#' 
#' @returns data.frame
#' 
#' @examples
#' raw_df <- get_raw_uni_df("bioc")
#' df <- .expand_jobs(df)
#' df <- .expand_binaries(df)
#'
#' @noRd
.expand_binaries <- function(df) {
    dplyr::bind_rows(
        df |>
            dplyr::filter(job_type == "binary") |>
            dplyr::mutate(`_binaries` = purrr::pmap(
                list(`_binaries`, job_r_xy, job_os, job_arch),
                match_binaries)) |>
            tidyr::unnest(`_binaries`, names_sep = "_", names_repair = "unique",
                          keep_empty = TRUE) |>
            dplyr::rename(binaries_r = `_binaries_r`,
                          binaries_os_raw = `_binaries_os`,
                          binaries_version = `_binaries_version`,
                          binaries_date = `_binaries_date`,
                          binaries_arch = `_binaries_arch`,
                          binaries_distro = `_binaries_distro`,
                          binaries_commit = `_binaries_commit`,
                          binaries_fileid = `_binaries_fileid`,
                          binaries_status = `_binaries_status`,
                          binaries_check = `_binaries_check`,
                          binaries_buildurl = `_binaries_buildurl`) |>
            dplyr::mutate(binary_os = get_binary_os(binaries_os_raw),
                          binary_r_xy = dplyr::if_else(is.na(binaries_r),
                                                       NA_character_,
                                                       r_xy_ver(binaries_r))),
        df |>
            dplyr::filter(job_type %in% c("bioc-checks", "source")) |>
            dplyr::select(-`_binaries`) |>
            dplyr::mutate(binary_r_xy = NA_character_))
}

#' Flatten raw universe data.frame by matching R, OS, and arch information from
#'  `_jobs` with `_binaries`
#' 
#' @description
#' Adds additional columns to job_r_xy, binary_r_xy, job_type, job_arch,
#' job_os, binary_os
#' 
#' @param raw_df raw data.frame from R Universe API
#' 
#' @returns data.frame
#' 
#' @examples
#' raw_df <- get_raw_uni_df("bioc")
#' get_uni_df(raw_df)
#'
#' @export
get_uni_df <- function(raw_df) {
    uni_df <- raw_df |>
        .expand_jobs() |>
        .expand_binaries() |>
        dplyr::filter(
            (job_type == "binary" & (job_r_xy == binary_r_xy | is.na(binary_r_xy))) |
                (job_type %in% c("bioc-checks", "source")))

    uni_df
}

#' Get jobs for an R Universe build
#'
#' @description Adds artifact and job_url
#'
#' @param universe_df data.frame output of get_uni_df
#' @param bu result of uni_for_bioc
#'
#' @returns data.frame packages in a universe
#'
#' @examples
#' bu <- uni_for_bioc("devel")
#' raw_universe_df <- get_raw_uni_df(bu$universe)
#' universe_df <- get_uni_df(raw_universe_df)
#' get_jobs(universe_df, bu$r_version, bu$universe)
#'
#' @export

get_jobs <- function(universe_df, r_version, universe) {

    # Packages without an explicit binaries_arch have universal binaries
    binary_rows <- universe_df |>
        dplyr::filter(
            job_type == "binary",
            job_r_xy == r_xy_ver(r_version)) |>
        dplyr::mutate(
            artifact = ifelse(
                binaries_check %in% c("FAIL", "ERROR", "CANCELLED") |
                    jobs_check %in% c("FAIL", "ERROR", "CANCELLED") |
                    binaries_status == "failure",
                NA_character_,
                file.path(uni_repo_url(universe, binary_r_xy,
                                       get_binary_os(binary_os),
                                       arch = get_arch(binaries_arch,
                                                       job_arch)),
                          uni_pkg_file(Package, binary_os,
                                       binaries_version))))

    nonbinary_rows <- universe_df |>
        dplyr::filter(job_type != "binary") |>
        dplyr::mutate(artifact = NA_character_)

    updated_universe_df <- dplyr::bind_rows(binary_rows, nonbinary_rows) |>
        dplyr::mutate(job_url = ifelse(is.na(job_id), NA_character_,
                                      file.path(`_buildurl`, "job", job_id))) |>
        dplyr::arrange(Package)
    updated_universe_df
}

#' Filter by arch if it has arch-specific and universal binaries
#' 
#' @description Some builds can have arch-specific and universal binaries. This
#' function filters specifically for data observed in R Universe, which could
#' change in the future.
#' 
#' * mac universal binaries: job_arch == 'arm64' & !is.na(binaries_arch)
#' * linux default binaries: job_arch == 'x86_64' & !is.na(binaries_arch)
#'
#' @param df data.frame from get_jobs()
#' @param os character
#' @param arch character (default: x86_64)
#' 
#' @returns data.frame
#' 
#' @examples
#' bu <- uni_for_bioc("devel")
#' raw_universe_df <- get_raw_uni_df(bu$universe)
#' universe_df <- get_uni_df(raw_universe_df)
#' jobs <- get_jobs(universe_df, bu$r_version, bu$universe)
#' filter_by_os_arch(jobs, "macosx", "arm64")
#' 
#' @export
filter_by_arch <- function(df, os, arch = "x86_64") {
    if (os == "linux" && is.na(arch))
        arch <- "x86_64"

    if (os == "mac" && arch == "x86_64") {
        dplyr::filter(df,
                      (binaries_arch == "x86_64") | (job_arch == "arm64" & is.na(binaries_arch)))
    } else if (os == "mac" && arch == "arm64") {
        dplyr::filter(df,
                      (binaries_arch == "aarch64") | (job_arch == "arm64" & is.na(binaries_arch)))
    } else if (os == "linux" && arch == "x86_64") {
        dplyr::filter(df,
                      (binaries_arch == "x86_64") | (job_arch == "x86_64" & is.na(binaries_arch)))
    } else if (os == "linux" && arch == "arm64") {
        dplyr::filter(df,
                      (binaries_arch == "aarch64") | (job_arch == "x86_64" & is.na(binaries_arch)))
    } else {
        df
    }
}

#' Check if a platform is listed as unsupported for a package
#'
#' @description
#' Parses `Config/Bioconductor/UnsupportedPlatforms` (a comma-separated string)
#' and tests whether the given OS and arch combination is covered. Matches both
#' full names (e.g. "windows", "macosx") and abbreviations (e.g. "win",
#' "mac"), as well as OS+arch combinations (e.g. "macosx-arm64"). Note: linux
#' is always supported and other oses/arches should return FALSE.
#'
#' @param unsupported_str character scalar; value of
#'     `Config/Bioconductor/UnsupportedPlatforms`, may be NA
#' @param os character scalar; 3-char OS abbreviation as used in job_os,
#'     e.g. "win", "mac"
#' @param arch character scalar; "x86_64", "arm64", or NA
#'
#' @returns logical; TRUE if the OS/arch combo is in the unsupported list
#'
#' @examples
#' is_unsupported_platform("windows, macosx-arm64", "win", "x86_64")  # TRUE
#' is_unsupported_platform("windows, macosx-arm64", "mac", "arm64")   # TRUE
#' is_unsupported_platform("windows, macosx-arm64", "mac", "x86_64")  # FALSE
#' is_unsupported_platform("macosx", "mac", "arm64")                  # TRUE
#' is_unsupported_platform(NA, "win", "x86_64")                       # FALSE
#'
#' @export
is_unsupported_platform <- function(unsupported_str, os, arch) {
    if (is.na(unsupported_str) || is.na(os))
        return(FALSE)

    entries <- trimws(tolower(strsplit(unsupported_str, ",")[[1]]))

    for (entry in entries) {
        entry_os <- dplyr::case_when(
            stringr::str_detect(entry, "linux") ~ "linux",
            stringr::str_detect(entry, "win")   ~ "win",
            stringr::str_detect(entry, "mac")   ~ "mac",
            stringr::str_detect(entry, "wasm")  ~ "wasm",
            .default = entry
        )

        # Extract and normalize arch from the entry token
        raw_arch <- stringr::str_extract(entry,
                                         "aarch64|arm64|x86_64|x86|emscripten")
        entry_arch <- dplyr::case_when(
            is.na(raw_arch)             ~ NA_character_,
            raw_arch == "aarch64"       ~ "arm64",
            raw_arch == "x86"           ~ "x86_64",
            .default = raw_arch
        )

        os_match <- (entry_os == os)
        if (!os_match)
            next

        arch_match <- is.na(entry_arch) || (!is.na(arch) && entry_arch == arch)
        if (arch_match)
            return(TRUE)
    }
    FALSE
}

#' Get candidate packages in R Universe based on criteria
#'
#' @description Filters packages by same commit in  BBS, vignettes passing, or
#' check status values.
#'
#' @param bu result of uni_for_bioc
#' @param os character
#' @param arch character (default NA_character_) x86_64 or arm64
#' @param vignettes logical (default TRUE) Check status of source / vignettes
#' @param commit logical (default FALSE) Check commit hash in RU and BBS?
#' @param unsupported_platforms logical (default TRUE) Check for unsupported
#' platforms
#'
#' @returns data.frame of filtered candidate packages
#'
#' @examples
#' bu <- uni_for_bioc("devel")
#' candidates <- get_candidates(bu, "windows", commit = TRUE)
#'
#' @export
get_candidates <- function(bu, os, arch = NA_character_, vignettes = TRUE,
                           commit = FALSE, unsupported_platforms = TRUE,
                           check = c("NOTE", "WARNING", "OK")) {

    os <- get_binary_os(os)
    raw_universe_df <- get_raw_uni_df(bu$universe)
    universe_df <- get_uni_df(raw_universe_df)
    jobs <- get_jobs(universe_df, bu$r_version, bu$universe)
    candidates <- jobs |>
        dplyr::filter(job_os == os)

    if (!is.na(arch))
        candidates <- candidates |>
            filter_by_arch(os, arch)

    if (vignettes) {
        vignette_jobs <- jobs |>
            dplyr::filter(job_type == "source") |>
            dplyr::select(Package, Version, jobs_check) |>
            dplyr::rename(vignettes_check = jobs_check)

        candidates <- merge(candidates, vignette_jobs,
                            by = c("Package", "Version"))

        candidates <- candidates |>
            dplyr::filter(vignettes_check %in% check)
    }

    if (commit) {
        bbs_info <- BiocPkgTools::biocBuildReport(version = bu$bioc_version,
                                                  pkgType = "software")
        bbs_builds <- bbs_info |>
            dplyr::rename(Package = pkg, Version = version,
                          BBS_commit = git_last_commit) |>
            dplyr::select(Package, Version, BBS_commit, Deprecated) |>
            dplyr::distinct()

        candidates <- merge(candidates, bbs_builds,
                            by = c("Package", "Version"))

        candidates <- candidates |>
            dplyr::filter(binaries_check %in% check,
                          jobs_check %in% check,
                          binaries_status == "success") |>
            dplyr::filter(BBS_commit == substr(RemoteSha, 1, 7))
    }

    if (unsupported_platforms) {
        candidates <- candidates |>
            dplyr::filter(
                job_type == "source" |
                    !purrr::pmap_lgl(
                        list(`Config/Bioconductor/UnsupportedPlatforms`,
                             job_os,
                             job_arch),
                        is_unsupported_platform
                    )
            )
    }

    candidates |>
        dplyr::arrange(Package)
}

#' Get information about R Universe building a Bioconductor version
#'
#' @param branch character Bioconductor branch: "devel" or "release"
#'
#' @examples
#' bu <- uni_for_bioc("devel")
#' 
#' @export
uni_for_bioc <- function(branch) {
    bioc_yaml <- yaml::read_yaml("https://bioconductor.org/config.yaml")
    stopifnot(branch %in% c(bioc_yaml$versions, "release", "devel"))

    if (branch == "devel") {
        bioc_version <- bioc_yaml$devel_version
        bioc_branch <- "devel"
        universe <- "bioc"
        r_version <- bioc_yaml$r_version_associated_with_devel
    } else {
        bioc_version <- bioc_yaml$release_version
        bioc_branch <- "release"
        universe <- "bioc-release"
        r_version <- bioc_yaml$r_version_associated_with_release
    }
    
    list(bioc_version = bioc_version,
         bioc_branch = bioc_branch,
         universe = universe,
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
#' bu <- uni_for_bioc("devel")
#' repo_root <- paste0("/home/biocpush/PACKAGES/", bu$bioc_version, "/bioc")
#' remove_old_binaries(repo_root, bu$bioc_version, "windows")
#'
#' @export
remove_old_binaries <- function(repo_root, r_version, os,
                                macosx_name = NA_character_,
                                arch = NA_character_, test = TRUE) {
    binaries_path <- get_repository_path(repo_root, r_version, os, macosx_name,
                                         arch)
    files <- list.files(binaries_path, pattern=".*._[0-9]+\\.[0-9]+\\.[0-9]+\\..*")
    binaries <- data.frame(file = files,
                           latest = NA_character_)
    binaries <- binaries |>
        dplyr::mutate(pkg = sub("_[^_]*$", "", file),
                      full_path = file.path(binaries_path, file),
                      version = stringr::str_extract(file, "(?<=_)\\d+\\.\\d+\\.\\d+(?=\\.)")) |>
        dplyr::arrange(pkg)

    latest <- NULL
    for (i in seq_len(nrow(binaries))) {
        if (is.null(latest)) {
            latest <- i
            binaries$latest[latest] <- TRUE
            next
        }
        
        if (binaries$pkg[latest] != binaries$pkg[i]) {
            latest <- i
            binaries$latest[latest] <- TRUE
            next
        }
        
        if (binaries$pkg[latest] == binaries$pkg[i]) {
            if (package_version(binaries$version[latest]) >
                package_version(binaries$version[i])) {
                binaries$latest[latest] <- TRUE
                binaries$latest[i] <- FALSE
            } else if (package_version(binaries$version[i]) >
                       package_version(binaries$version[latest])) {
                binaries$latest[latest] <- FALSE
                binaries$latest[i] <- TRUE
                latest <- i
            }
        }
    }

    if (!is.null(latest))
        binaries$latest[latest] <- TRUE

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
