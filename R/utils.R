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
get_repository_path <- function(repo_root, r_version, os, macosx_name = NA,
                                arch = NA) {
    r_xy <- r_xy_ver(r_version)
    
    is_mac <- !is.na(os) & stringr::str_detect(os, "mac")
    is_win <- !is.na(os) & stringr::str_detect(os, "win")

    # --- validation ---
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
    
    # --- path building ---
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
uni_repo_url <- function(uni, r_version, os, macosx_name = NA, arch = NA) {
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
#' @param binaries_arch _binaries_arch
#' @param jobs_arch _jobs_arch
#' 
#' @returns character
#'
#' @examples
#' get_arch(NA, "arm64")
get_arch <- function(binaries_arch, jobs_arch) {
    dplyr::case_when(is.na(binaries_arch) & jobs_arch == "arm64" ~ "x86_64",
                     binaries_arch == "aarch64" & jobs_arch == "arm64" ~ "arm64",
                     .default = jobs_arch)
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
    if (!"arch" %in% names(binaries))
        binaries$arch <- NA_character_
    binaries_os_ <- get_binary_os(binaries$os)
    binaries_r_xy <- r_xy_ver(binaries$r)
    arch_match <- dplyr::case_when(
        is.na(binaries$arch) ~ TRUE,
        arch == "arm64" ~ binaries$arch == "aarch64",
        .default = binaries$arch == arch
    )
    binaries[binaries_os_ == os &
                 binaries_r_xy == r_xy &
                 arch_match, ]
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
#' raw_df <- get_raw_uni_df("bioc")
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
                .default = "binary"),
            `_jobs_arch` = dplyr::case_when(
                stringr::str_detect(`_jobs_config`, "arm64") ~ "arm64",
                stringr::str_detect(`_jobs_config`, "x86_64|windows") ~ "x86_64",
                stringr::str_detect(`_jobs_config`, "wasm") ~ "emscripten",
                .default = NA),
            `_jobs_os_` = dplyr::if_else(`_jobs_type` == "binary",
                                         get_binary_os(stringr::str_split_i(`_jobs_config`, "-", 1)),
                                         "linux")) |>
        (\(df) dplyr::bind_rows(
            df |>
                dplyr::filter(`_jobs_type` == "binary") |>
                dplyr::mutate(`_binaries` = purrr::pmap(
                    list(`_binaries`, `_jobs_r_xy`, `_jobs_os_`, `_jobs_arch`),
                    match_binaries)) |>
                tidyr::unnest(`_binaries`, names_sep = "_", names_repair = "unique",
                              keep_empty = TRUE) |>
                dplyr::mutate(`_binaries_os_` = get_binary_os(`_binaries_os`),
                              `_binaries_r_xy` = dplyr::if_else(is.na(`_binaries_r`),
                                                                NA_character_,
                                                                r_xy_ver(`_binaries_r`))),
            df |>
                dplyr::filter(`_jobs_type` %in% c("bioc-checks", "source")) |>
                dplyr::select(-`_binaries`) |>
                dplyr::mutate(`_binaries_r_xy` = NA_character_)))() |>
        dplyr::filter(
            (`_jobs_type` == "binary" & (`_jobs_r_xy` == `_binaries_r_xy` | is.na(`_binaries_r_xy`))) |
                (`_jobs_type` %in% c("bioc-checks", "source")))
    
    uni_df
}

#' Get jobs for an R Universe build
#'
#' @description Adds Artifact and JobUrl
#'
#' @param uni character universe name
#' @param uni_os_branch character release or devel
#' @param r_version character
#' @param bioc_version character
#'
#' @returns data.frame packages in a universe
#'
#' @examples
#' get_jobs("bioc", "devel", "4.6.0", "3.23")
#'
#' @export
get_jobs <- function(uni, uni_os_branch, r_version, bioc_version) {
    raw_uni_df <- get_raw_uni_df(uni)
    uni_df <- get_uni_df(raw_uni_df)

    # Packages without an explicit _binaries_arch have universal binaries
    binary_rows <- uni_df |>
        dplyr::filter(
            `_jobs_type` == "binary",
            `_jobs_r_xy` == r_xy_ver(r_version)) |>
        dplyr::mutate(
            Artifact = ifelse(
                `_binaries_check` %in% c("FAIL", "ERROR", "CANCELLED") |
                    `_jobs_check` %in% c("FAIL", "ERROR", "CANCELLED") |
                    `_binaries_status` == "failure",
                NA_character_,
                file.path(uni_repo_url(uni, `_binaries_r_xy`,
                                       get_binary_os(`_binaries_os_`),
                                       arch = get_arch(`_binaries_arch`,
                                                       `_jobs_arch`)),
                          uni_pkg_file(Package, `_binaries_os_`,
                                       `_binaries_version`))))

    nonbinary_rows <- uni_df |>
        dplyr::filter(`_jobs_type` != "binary") |>
        dplyr::mutate(Artifact = NA_character_)
    
    updated_uni_df <- dplyr::bind_rows(binary_rows, nonbinary_rows) |>
        dplyr::mutate(JobUrl = ifelse(is.na(`_jobs_job`), NA,
                                      file.path(`_buildurl`, "job", `_jobs_job`))) |>
        dplyr::arrange(Package)
    updated_uni_df
}

#' Filter by arch if it has arch-specific and universal binaries
#' 
#' @description Some builds can have arch-specific and universal binaries. This
#' function filters specifically for data observed in R Universe, which could
#' change in the future.
#' 
#' * mac universal binaries: `_jobs_arch` == 'arm64' & !is.na(`_binaries_arch`)
#' * linux universal binaries: `_jobs_arch` == 'x86_64' & !is.na(`_binaries_arch`)
#'
#' @param df data.frame from get_jobs()
#' @param os character
#' @param arch character
#' 
#' @returns data.frame
#' 
#' @examples
#' df <- get_jobs("bioc", "devel", "4.6.0", "3.23", "macosx", "arm64")
#' filter_by_arch(df, "macosx", "arm64")
#' 
#' @export
filter_by_arch <- function(df, os, arch = NA) {
    if (os == "mac" && !is.na(arch) && arch == "x86_64") {
        dplyr::filter(df,
                      (`_binaries_arch` == "x86_64") | (`_jobs_arch` == "arm64" & is.na(`_binaries_arch`)))
    } else if (os == "mac" && !is.na(arch) && arch == "arm64") {
        dplyr::filter(df,
                      (`_binaries_arch` == "aarch64") | (`_jobs_arch` == "arm64" & is.na(`_binaries_arch`)))
    } else if (os == "linux" && (is.na(arch) || arch == "x86_64")) {
        dplyr::filter(df,
                      (`_binaries_arch` == "x86_64") | (`_jobs_arch` == "x86_64" & is.na(`_binaries_arch`)))
    } else if (os == "linux" && !is.na(arch) && arch == "arm64") {
        dplyr::filter(df,
                      (`_binaries_arch` == "aarch64") | (`_jobs_arch` == "x86_64" & is.na(`_binaries_arch`)))
    } else {
        df
    }
}

#' Get candidate packages in R Universe based on criteria
#'
#' @description Filters packages by same commit in  BBS, vignettes passing, or
#' check status values. 
#'
#' @param uni character universe name
#' @param uni_os_branch character release or devel
#' @param r_version character
#' @param bioc_version character
#' @param os character
#' @param arch character x86_64 or arm64
#' @param available_pkgs data.frame of R Universe packages
#' @param vignettes logical (default TRUE) Check status of source / vignettes
#' @param commit logical (default FALSE) Check commit hash in RU and BBS?
#' @param check character vector of acceptable R CMD check statuses
#'
#' @returns data.frame of filtered candidate packages
#'
#' @examples
#' candidates <- get_candidates("bioc", "devel", "4.6.0", "3.23", "windows",
#'                              commit = TRUE)
#'
#' @export
get_candidates <- function(uni, uni_os_branch, r_version, bioc_version, os,
                           arch = NA, vignettes = TRUE, commit = FALSE,
                           check = c("NOTE", "WARNING", "OK")) {

    os <- get_binary_os(os)
    jobs <- get_jobs(uni, uni_os_branch, r_version, bioc_version)
    candidates <- jobs |>
        dplyr::filter(`_jobs_os_` == os) |>
        filter_by_arch(os, arch)

    if (vignettes) {
        vignette_jobs <- jobs |>
            dplyr::filter(`_jobs_type` == "source") |>
            dplyr::select(Package, Version, `_jobs_check`) |>
            dplyr::rename(`Vignettes Check` = `_jobs_check`)

        candidates <- merge(candidates, vignette_jobs,
                            by = c("Package", "Version"))

        candidates <- candidates |>
            dplyr::filter(`Vignettes Check` %in% check)
    }

    if (commit) {
        bbs_info <- BiocPkgTools::biocBuildReport(version = bioc_version,
                                                  pkgType = "software")
        bbs_builds <- bbs_info |>
            dplyr::rename(Package = pkg, Version = version,
                          BBS_commit = git_last_commit) |>
            dplyr::select(Package, Version, BBS_commit, Deprecated) |>
            dplyr::distinct()

        candidates <- merge(candidates, bbs_builds,
                            by = c("Package", "Version"))

        candidates <- candidates |>
            dplyr::filter(`_binaries_check` %in% check,
                          `_jobs_check` %in% check,
                          `_binaries_status` == "success") |>
            dplyr::filter(BBS_commit == substr(RemoteSha, 1, 7))
    }
    
    candidates |>
        dplyr::arrange(Package)
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
remove_old_binaries <- function(repo_root, r_version, os, macosx_name = NA,
                                arch = NA, test = TRUE) {
    binaries_path <- get_repository_path(repo_root, r_version, os, macosx_name,
                                         arch)
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
           if (package_version(binaries$version[latest]) >
               package_version(binaries$version[i])) {
               binaries$latest[latest] <- TRUE
               binaries$latest[i] <- FALSE
           } else if (package_version(binaries$version[i]) > package_version(binaries$version[latest])) {
               binaries$latest[i] <- TRUE
               binaries$latest[latest] <- FALSE
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
