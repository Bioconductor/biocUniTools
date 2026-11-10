#' Limit for number of records to retrieve using universe package
#' @noRd
.LIMIT <- 5000L

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

#' Get repo subpath by R version
#'
#' @param os character OS name, e.g. "macosx", "windows", "linux"
#' @param arch character x86_64 or arm64
#' @param r_xy R X.Y character
#' @param cran_subpath (default FALSE) uses CRAN style subpaths
#' @param distro character Linux distribution codename, e.g. "resolute", used
#'     for linux
#'
#' @returns character
#' 
#' @examples
#' get_subpath("macosx", "arm64", "4.6")
#' get_subpath(c("macosx", "windows"), c("arm64", "x86_64"), c("4.6", "4.5"))
#' get_subpath("linux", "x86_64", "4.6", distro = "resolute")
#'
#' @export
get_subpath <- function(os, arch, r_xy, cran_subpath = FALSE,
                        distro = NA_character_) {
    is_mac <- !is.na(os) & stringr::str_detect(os, "mac")
    is_win <- !is.na(os) & stringr::str_detect(os, "win")
    is_linux <- !is.na(os) & stringr::str_detect(os, "linux")

    if (any(is_mac & as.double(r_xy) < 4.6 & arch == "arm64", na.rm = TRUE))
        stop("R version must be greater than or equal to 4.6 if arch is arm64")

    dplyr::case_when(
        is_mac & arch == "arm64" ~ paste("sonoma", arch, sep = "-"),
        is_mac ~ paste("big-sur", arch, sep = "-"),
        is_win & (as.double(r_xy) < 4.6 | cran_subpath) ~ "",
        is_win & arch == "arm64" ~ "clang-arm64",
        is_win ~ "gcc-x86_64",
        is_linux & !is.na(distro) & !is.na(arch) ~ paste(distro, arch, sep = "-"),
        .default = ""
    )
}

#' Get repo path
#'
#' @details R Universe has adopted <os name>-<arch> style path for linux and
#' and macosx, <compiler>-<arch> for windows. CRAN has not adopted these
#' conventions for linux and windows, so Bioconductor has not yet adopted them.
#' The `cran_subpath` parameter should be TRUE to not use R Universe subpath
#' for windows.
#'
#' @param repo_root character
#' @param r_version character
#' @param os character, ignored when `type` is "source"
#' @param arch character x86_64 or arm64
#' @param cran_subpath (default FALSE) uses CRAN style subpaths
#' @param type character "binary" (default) or "source". Source packages are
#'     in `src/contrib`.
#' @param distro character Linux distribution codename, e.g. "resolute",
#'     required for linux binaries
#'
#' @returns character
#'
#' @examples
#' repo_root <- "/home/biocpush/PACKAGES/3.22/bioc"
#' get_repository_path(repo_root, "4.6.0", "windows", "arm64")
#' get_repository_path(repo_root, c("4.6.0", "4.6.0"), c("windows", "macosx"),
#'                     c("arm64", "arm64"))
#' get_repository_path(repo_root, "4.6.0", "linux", "x86_64",
#'                     distro = "resolute")
#' get_repository_path(repo_root, "4.6.0", type = "source")
#'
#' @export
get_repository_path <- function(repo_root, r_version, os = NA_character_,
                                arch = NA_character_, cran_subpath = FALSE,
                                type = c("binary", "source"),
                                distro = NA_character_) {
    type <- match.arg(type)
    if (type == "source")
        return(file.path(repo_root, "src/contrib"))

    is_mac <- !is.na(os) & stringr::str_detect(os, "mac")
    is_win <- !is.na(os) & stringr::str_detect(os, "win")
    is_linux <- !is.na(os) & stringr::str_detect(os, "linux")

    if (any((is_mac | is_win | is_linux) & (is.na(arch) |
        !stringr::str_detect(arch, "^(x86_64|arm64)$"))))
        stop("arch must be x86_64 or arm64")
    if (any(is_linux & is.na(distro)))
        stop("distro is required for linux binaries")

    r_xy <- r_xy_ver(r_version)
    subpath <- get_subpath(os, arch, r_xy, cran_subpath, distro)
    repo_path <- dplyr::case_when(
        is.na(os) ~ NA_character_,
        is_mac ~ file.path("bin", "macosx", subpath, "contrib", r_xy),
        is_win ~ file.path("bin", "windows", subpath, "contrib", r_xy),
        is_linux ~ file.path("bin", "linux", subpath, "contrib", r_xy),
        .default = NA_character_
    )

    repo_path <- gsub("//+", "/", repo_path)

    result <- file.path(repo_root, repo_path)
    result[is.na(repo_path)] <- NA_character_
    result
}

#' Get universe URL for an os and R version
#'
#' @details R Universe puts linux binaries in
#' `bin/linux/<distro>-<arch>/<R x.y>/src/contrib`.
#'
#' @param uni character universe name
#' @param r_version character
#' @param os character, ignored when `type` is "source"
#' @param arch character x86_64 or arm64
#' @param type character "binary" (default) or "source"
#' @param distro character Linux distribution codename, e.g. "resolute",
#'     needed for linux binaries
#'
#' @returns character, `NA` for linux binaries without `distro`
#'
#' @examples
#' uni_repo_url("bioc", "4.5.3", "windows", "x86_64")
#' uni_repo_url("bioc", "4.6.1", "linux", "x86_64", distro = "resolute")
#' uni_repo_url("bioc", "4.6.1", type = "source")
#'
#' @export
uni_repo_url <- function(uni, r_version, os = NA_character_,
                         arch = NA_character_, type = c("binary", "source"),
                         distro = NA_character_) {
    type <- match.arg(type)
    universe <- paste0("https://", uni, ".r-universe.dev")
    if (type == "source")
        return(file.path(universe, "src", "contrib"))

    is_linux <- !is.na(os) & grepl("linux", os)
    dplyr::case_when(
        # R Universe: bin/linux/<distro>-<arch>/<R x.y>/src/contrib
        is_linux & !is.na(distro) ~ file.path(universe, "bin", "linux",
                                              paste(distro, arch, sep = "-"),
                                              r_xy_ver(r_version), "src",
                                              "contrib"),
        # A linux job without a binary has nothing to download
        is_linux ~ NA_character_,
        # Windows and macOS use the same layout as Bioconductor
        .default = get_repository_path(universe, r_version,
                                       ifelse(is_linux, NA, os), arch))
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
#' binaries_arch <- c(NA, "x86_64", "aarch64", NA)
#' get_arch(binaries_arch, "arm64")
#'
#' @export
get_arch <- function(binaries_arch, job_arch) {
    dplyr::case_when(is.na(binaries_arch) & job_arch == "arm64" ~ "x86_64",
                     binaries_arch == "aarch64" & job_arch == "arm64" ~ "arm64",
                     .default = job_arch)
}

#' Get packages in an R Universe
#'
#' @param uni character universe name
#' @param packages character package names. If `NULL` (default), all packages
#'     in the universe. Otherwise, one request per package, which is faster for
#'     a few packages.
#'
#' @returns data.frame packages in a universe
#'
#' @examples
#' get_raw_uni_df("bioc", packages = c("BiocGenerics", "S4Vectors"))
#'
#' @export
get_raw_uni_df <- function(uni, packages = NULL) {
    uni_api_url <- paste0("https://", uni, ".r-universe.dev/api/packages/")
    if (is.null(packages)) {
        pkgs <- httr2::request(uni_api_url) |>
            httr2::req_url_query(limit = as.integer(.LIMIT)) |>
            httr2::req_perform() |>
            httr2::resp_body_json(simplifyVector = FALSE)
    } else {
        pkgs <- lapply(packages, function(pkg) {
            tryCatch(httr2::request(paste0(uni_api_url, pkg)) |>
                         httr2::req_perform() |>
                         httr2::resp_body_json(simplifyVector = FALSE),
                     httr2_http_404 = function(e) NULL)
        })
        pkgs <- Filter(Negate(is.null), pkgs)
    }
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
#' @description Adds job_url and artifact, the download link of the binary or
#' source package a job built, `NA` if the job failed
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

    if (!"binaries_distro" %in% names(universe_df))
        universe_df$binaries_distro <- NA_character_
    failed <- c("FAIL", "ERROR", "CANCELLED")
    in_repo <- function(url, file)
        ifelse(is.na(url), NA_character_, file.path(url, file))

    # Packages without an explicit binaries_arch have universal binaries
    binary_rows <- universe_df |>
        dplyr::filter(
            job_type == "binary",
            job_r_xy == r_xy_ver(r_version)) |>
        dplyr::mutate(
            artifact = ifelse(
                binaries_check %in% failed | jobs_check %in% failed |
                    binaries_status == "failure",
                NA_character_,
                in_repo(uni_repo_url(universe, binary_r_xy,
                                     get_binary_os(binary_os),
                                     arch = get_arch(binaries_arch, job_arch),
                                     distro = binaries_distro),
                        uni_pkg_file(Package, binary_os, binaries_version))))

    # Source packages are built by the source job
    source_rows <- universe_df |>
        dplyr::filter(job_type == "source") |>
        dplyr::mutate(
            artifact = ifelse(
                jobs_check %in% failed | is.na(Version),
                NA_character_,
                in_repo(uni_repo_url(universe, r_version, type = "source"),
                        uni_pkg_file(Package, "source", Version))))

    other_rows <- universe_df |>
        dplyr::filter(!job_type %in% c("binary", "source")) |>
        dplyr::mutate(artifact = NA_character_)

    updated_universe_df <- dplyr::bind_rows(binary_rows, source_rows,
                                            other_rows) |>
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
#' * linux/win default binaries: job_arch == 'x86_64' & !is.na(binaries_arch)
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
#' filter_by_arch(jobs, "macosx", "arm64")
#' 
#' @export
filter_by_arch <- function(df, os, arch = "x86_64") {
    if (os == "linux" && is.na(arch))
        arch <- "x86_64"
    if (is.na(arch))
        return(df)

    if (os == "mac" && arch == "x86_64") {
        dplyr::filter(df,
                      (binaries_arch == "x86_64") | (job_arch == "arm64" & is.na(binaries_arch)))
    } else if (os == "mac" && arch == "arm64") {
        dplyr::filter(df,
                      (binaries_arch == "aarch64") | (job_arch == "arm64" & is.na(binaries_arch)))
    } else if (os %in% c("linux", "win") && arch == "x86_64") {
        dplyr::filter(df,
                      (binaries_arch == "x86_64") | (job_arch == "x86_64" & is.na(binaries_arch)))
    } else if (os %in% c("linux", "win") && arch == "arm64") {
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
#' @description Filters binary or source packages that R Universe built by
#' check results and, optionally, the same commit in BBS.
#'
#' Binary candidates are binaries for `os` and `arch` whose check passed,
#' optionally with a passing source build (`vignettes`) and the same commit as
#' BBS (`commit`). Source candidates have a passing source build and a passing
#' Linux x86_64 `R CMD check`, optionally with the same commit as BBS
#' (`commit`).
#'
#' @param bu result of uni_for_bioc
#' @param os character, for binaries
#' @param arch character (default NA_character_) x86_64 or arm64, for binaries
#' @param type character "binary" (default) or "source"
#' @param vignettes logical (default TRUE) Check status of source / vignettes,
#'     for binaries
#' @param commit logical (default FALSE) Check commit hash in RU and BBS?
#' @param unsupported_platforms logical (default TRUE) Check for unsupported
#'     platforms, for binaries
#' @param check character results that pass
#' @param jobs data.frame from `get_jobs()`. If `NULL` (default), downloaded.
#'
#' @returns data.frame of filtered candidate packages, with `artifact` the
#'     download link
#'
#' @examples
#' \dontrun{
#' bu <- uni_for_bioc("devel")
#' candidates <- get_candidates(bu, "windows", "x86_64", commit = TRUE)
#' sources <- get_candidates(bu, type = "source", commit = TRUE)
#' }
#'
#' @export
get_candidates <- function(bu, os = NA_character_, arch = NA_character_,
                           type = c("binary", "source"), vignettes = TRUE,
                           commit = FALSE, unsupported_platforms = TRUE,
                           check = c("NOTE", "WARNING", "OK"), jobs = NULL) {
    propagation_checks(bu, os = os, arch = arch, type = type,
                       vignettes = vignettes, commit = commit,
                       unsupported_platforms = unsupported_platforms,
                       check = check, jobs = jobs) |>
        dplyr::filter(propagates) |>
        dplyr::select(-propagates, -reason)
}

#' Check which R Universe packages can propagate to Bioconductor
#'
#' @description
#' Applies the rules of `get_candidates()` to every package and reports which
#' rules each one fails. For binaries, a package propagates when its binary
#' for `os` and `arch` passed `R CMD check` and built (`check_ok`), the source
#' build passed (`source_ok`, if `vignettes`), R Universe built the same
#' commit as BBS (`commit_ok`, if `commit`), and the platform is supported
#' (`platform_ok`, if `unsupported_platforms`). For source packages, the
#' source build passed (`source_ok`), `R CMD check` on Linux x86_64 passed
#' (`check_ok`), and, if `commit`, the commit is the same as BBS
#' (`commit_ok`).
#'
#' @inheritParams get_candidates
#' @param bbs data.frame of BBS commits with columns Package, Version,
#'     BBS_commit, and Deprecated, used when `commit` is `TRUE`. If `NULL`
#'     (default), read with `bbs_status()` for every package type.
#'
#' @returns data.frame of jobs with logical columns for each rule that applies,
#'     `propagates`, and `reason`, the rules a package fails
#'
#' @examples
#' \dontrun{
#' bu <- uni_for_bioc("devel")
#' propagation_checks(bu, type = "source", commit = TRUE)
#' }
#'
#' @export
propagation_checks <- function(bu, os = NA_character_, arch = NA_character_,
                               type = c("binary", "source"), vignettes = TRUE,
                               commit = FALSE, unsupported_platforms = TRUE,
                               check = c("NOTE", "WARNING", "OK"), jobs = NULL,
                               bbs = NULL) {
    type <- match.arg(type)
    if (is.null(jobs)) {
        raw_universe_df <- get_raw_uni_df(bu$universe)
        universe_df <- get_uni_df(raw_universe_df)
        jobs <- get_jobs(universe_df, bu$r_version, bu$universe)
    }
    if (commit && is.null(bbs))
        bbs <- .bbs_commits(bu)
    key <- function(df) paste(df$Package, df$Version)
    one_per_package <- function(df)
        dplyr::distinct(df, Package, Version, .keep_all = TRUE)

    source_jobs <- jobs |>
        dplyr::filter(job_type == "source") |>
        one_per_package()

    if (type == "source") {
        rows <- source_jobs
        linux <- jobs |>
            dplyr::filter(job_type == "binary", job_os == "linux") |>
            filter_by_arch("linux", "x86_64") |>
            one_per_package()
        rules <- list(
            source_ok = rows$jobs_check %in% check & !is.na(rows$artifact),
            check_ok = linux$jobs_check[match(key(rows), key(linux))] %in% check)
    } else {
        os <- get_binary_os(os)
        rows <- jobs |>
            dplyr::filter(job_type == "binary", job_os == os)
        if (!is.na(arch))
            rows <- filter_by_arch(rows, os, arch)
        rules <- list()
        if (vignettes)
            rules$source_ok <- source_jobs$jobs_check[
                match(key(rows), key(source_jobs))] %in% check
        if (commit)
            rules$check_ok <- rows$binaries_check %in% check &
                rows$jobs_check %in% check &
                rows$binaries_status %in% "success"
        if (unsupported_platforms)
            rules$platform_ok <- !purrr::pmap_lgl(
                list(rows$`Config/Bioconductor/UnsupportedPlatforms`,
                     rows$job_os, rows$job_arch),
                is_unsupported_platform)
    }
    if (commit) {
        i <- match(key(rows), key(bbs))
        rows$BBS_commit <- bbs$BBS_commit[i]
        rows$Deprecated <- bbs$Deprecated[i]
        same <- substr(rows$RemoteSha, 1, 7) == rows$BBS_commit
        # Packages not in BBS have no commit to compare, so they fail
        rules$commit_ok <- !is.na(same) & same
    }

    labels <- c(source_ok = "source build failed",
                check_ok = if (type == "source") "linux check failed" else
                    "check failed",
                platform_ok = "unsupported platform",
                commit_ok = "commit differs from BBS")
    for (rule in names(rules))
        rows[[rule]] <- rules[[rule]]
    passed <- if (length(rules)) Reduce(`&`, rules) else
        rep(TRUE, nrow(rows))
    rows$propagates <- passed
    rows$reason <- vapply(seq_len(nrow(rows)), function(i) {
        failed <- names(rules)[!vapply(rules, `[`, logical(1), i)]
        paste(labels[failed], collapse = "; ")
    }, character(1))
    rows |>
        dplyr::arrange(Package)
}

#' Commits BBS built, for every package type
#'
#' @param bu result of uni_for_bioc
#'
#' @returns data.frame with columns Package, Version, BBS_commit, Deprecated
#'
#' @noRd
.bbs_commits <- function(bu) {
    do.call(rbind, lapply(bioc_pkg_types()$type, function(type) {
        bbs <- bbs_status(bu$bioc_branch, type)
        data.frame(Package = bbs$Package, Version = bbs$Version,
                   BBS_commit = substr(bbs$commit, 1, 7),
                   Deprecated = bbs$deprecated)
    }))
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

    if (branch %in% c("devel", bioc_yaml$devel_version)) {
        bioc_version <- bioc_yaml$devel_version
        bioc_branch <- "devel"
        universe <- "bioc"
        r_version <- bioc_yaml$r_version_associated_with_devel
    } else if (branch %in% c("release", bioc_yaml$release_version)) {
        bioc_version <- bioc_yaml$release_version
        bioc_branch <- "release"
        universe <- "bioc-release"
        r_version <- bioc_yaml$r_version_associated_with_release
    } else
        stop("Must be the devel or release branch")
    
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
#' @param arch x86_64 or arm64
#' @param type character "binary" (default) or "source", to remove old source
#'     packages from `src/contrib`
#' @param distro character Linux distribution codename, required for linux
#'     binaries
#' @param test logical (default TRUE) don't remove, only print packages marked
#'     for removal
#'
#' @returns vector of the full path of binaries removed
#'
#' @examples
#' \dontrun{
#' bu <- uni_for_bioc("devel")
#' repo_root <- paste0("/home/biocpush/PACKAGES/", bu$bioc_version, "/bioc")
#' remove_old_binaries(repo_root, bu$bioc_version, "windows", "x86_64")
#' }
#'
#' @export
remove_old_binaries <- function(repo_root, r_version, os = NA_character_,
                                arch = NA_character_, test = TRUE,
                                type = c("binary", "source"),
                                distro = NA_character_) {
    binaries_path <- get_repository_path(repo_root, r_version, os, arch,
                                         cran_subpath = TRUE, type = type,
                                         distro = distro)
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
