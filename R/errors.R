#' Rules for the cause of an R Universe error
#'
#' @description
#' Each rule is a cause and a case-insensitive regular expression. The first
#' rule whose pattern matches a log's error lines is its cause; logs matching no
#' rule are "Other". Override with
#' `options(biocUniTools.error_rules = <data.frame with cause and pattern>)`.
#'
#' @returns data.frame with columns cause and pattern
#'
#' @examples
#' uni_error_rules()
#'
#' @export
uni_error_rules <- function() {
    getOption("biocUniTools.error_rules", data.frame(
        cause = c("Hub resource",
                  "Network/web service",
                  "Missing package",
                  "Missing cached data",
                  "External tool",
                  "Dependency changed",
                  "Timeout",
                  "Test expectation failed"),
        pattern = c(
            "failed to load resource|ExperimentHub|AnnotationHub",
            paste0("HTTP request|HTTP [45][0-9][0-9]|cannot open URL|",
                   "cannot open the connection|download\\.file|req_perform|",
                   "BioMart|Could not resolve host|Failed to retrieve file|",
                   "not all 'rnames' found"),
            paste0("there is no package called|",
                   "unable to load required package|",
                   "package or namespace load failed|",
                   "Hard dependency .* archived|",
                   "is required .* but not installed"),
            paste0("needs to be cached|not found or needs|",
                   "DB file .* not found|Invalid Cache"),
            paste0("pyspark|Java gateway|Conda|python|command not found|",
                   "reticulate|LaTeX|pdflatex|tinytex"),
            paste0("unable to find an inherited method|unused argument|",
                   "is not exported|deprecated|invalid class|no method for|",
                   "object is invalid|this S4 class|could not find function"),
            "action has timed out",
            "Failure \\(|^\\s*Expected `|tests? failed")))
}

#' Steps an R Universe job can fail at, matched against `##[error]` lines
#'
#' @noRd
.ERROR_STEPS <- data.frame(
    step = c("Install", "Dependencies", "Timeout", "Examples", "Tests",
             "Vignettes", "README"),
    pattern = c("Failed to install", "package dependencies|Hard dependency",
                "timed out", "Check: examples", "Check: tests",
                "build vignettes", "build readme"))

#' Get the log of an R Universe job
#'
#' @description
#' Downloads the log R Universe links to from a package's check table. Logs
#' are saved in `cache_dir` by job ID, so each is downloaded once.
#'
#' @param universe character, e.g., "bioc"
#' @param job character job ID
#' @param cache_dir character directory to save logs in, or `NULL` for none
#'
#' @returns character vector of log lines without timestamps, `NULL` if the
#'     log could not be downloaded
#'
#' @examples
#' \dontrun{
#' log <- uni_job_log("bioc", "114711429697")
#' }
#'
#' @export
uni_job_log <- function(universe, job, cache_dir = NULL) {
    path <- if (is.null(cache_dir)) tempfile(fileext = ".log") else
        file.path(cache_dir, paste0(job, ".log"))
    if (is.null(cache_dir) || !file.exists(path)) {
        if (!is.null(cache_dir))
            dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
        url <- sprintf("https://%s.r-universe.dev/api/actions/logs/%s",
                       universe, job)
        ok <- tryCatch({
            httr2::request(url) |>
                httr2::req_retry(max_tries = 3) |>
                httr2::req_perform(path = path)
            TRUE
        }, error = function(e) FALSE)
        if (!ok) {
            unlink(path)
            return(NULL)
        }
    }
    sub("^\\S+Z ", "", readLines(path, warn = FALSE, encoding = "UTF-8"))
}

#' Find the step, cause, and message of a failing R Universe job
#'
#' @param log character vector from `uni_job_log()`
#' @param source logical whether the job is a source build, so a failure
#'     without an identified step is a vignette failure
#'
#' @returns list with step, cause, and excerpt (the first error message, a few
#'     lines)
#'
#' @examples
#' parse_uni_log(c("##[error]Check: examples, Result: ERROR",
#'                 "Error in f(): there is no package called 'x'"))
#'
#' @export
parse_uni_log <- function(log, source = FALSE) {
    annotations <- sub("^##\\[error\\]", "",
                       grep("^##\\[error\\]", log, value = TRUE))
    step_found <- vapply(.ERROR_STEPS$pattern, function(p)
        any(grepl(p, annotations, ignore.case = TRUE)), logical(1))
    step <- if (any(step_found)) .ERROR_STEPS$step[step_found][1] else
        if (source) "Vignettes" else "Other"

    excerpt <- .error_excerpt(log)
    # The excerpt is the most specific, so it is matched first
    error_lines <- c(annotations,
                     grep("^\\s*(Error|!|Warning: Hard)", log, value = TRUE))
    rules <- uni_error_rules()
    match_rule <- function(lines) {
        matched <- vapply(rules$pattern, function(p)
            any(grepl(p, lines, ignore.case = TRUE)), logical(1))
        if (any(matched)) rules$cause[matched][1] else NA_character_
    }
    cause <- match_rule(excerpt)
    if (is.na(cause))
        cause <- match_rule(error_lines)
    if (is.na(cause))
        cause <- "Other"
    list(step = step, cause = cause, excerpt = excerpt)
}

#' First error message in a log
#'
#' @description Looks for, in order, the first failing test, the first failing
#' vignette chunk, and the first `Error` line.
#'
#' @param log character vector of log lines
#' @param n integer maximum number of lines
#'
#' @returns character vector
#'
#' @noRd
.error_excerpt <- function(log, n = 6L) {
    block <- function(start, stop_pattern) {
        if (is.na(start))
            return(NULL)
        lines <- log[start:min(length(log), start + 20L)]
        end <- grep(stop_pattern, lines[-1])[1]
        if (!is.na(end))
            lines <- lines[seq_len(end)]
        lines <- trimws(gsub("\u2500{2,}\\s*$", "", lines))
        lines <- lines[nzchar(lines) &
                           !grepl("^(~+|<error/rlang_error>|Error:)$", lines)]
        utils::head(lines, n)
    }
    test <- grep("^\\s*\u2500\u2500 (Failure|Error) \\(", log)[1]
    if (!is.na(test))
        return(block(test, "^\\s*(Backtrace:|\u2500\u2500 )"))
    vignette <- grep("^Quitting from ", log)[1]
    if (!is.na(vignette))
        return(block(vignette, "^(---|Backtrace:|Error: processing)"))
    error <- grep("^\\s*(Error|Erreur)\\b", log)[1]
    if (!is.na(error))
        return(block(error, "^\\s*(Execution halted|Backtrace:|Calls:|--- failed)"))
    annotations <- grep("^##\\[error\\]", log, value = TRUE)
    utils::head(sub("^##\\[error\\]", "", annotations), n)
}

#' Errors of packages failing in R Universe
#'
#' @description
#' Downloads the log of each package's first failing R Universe job, the
#' source build or the check, and finds the step, cause, and first error
#' message with `parse_uni_log()`. Packages that only fail in BBS are skipped.
#'
#' @param failing data.frame from `compare_failing()`
#' @param cache_dir character directory to save logs in, or `NULL` for none
#' @param delay numeric seconds to wait between downloads, because R Universe
#'     rate limits these links
#'
#' @returns data.frame with columns Package, type, branch, error_step,
#'     error_cause, error_excerpt (a list column), and log_url
#'
#' @examples
#' \dontrun{
#' failing <- compare_failing(compare_builds("devel", "books"))
#' uni_errors(failing)
#' }
#'
#' @export
uni_errors <- function(failing, cache_dir = NULL, delay = 1) {
    job_url <- failing$uni_failure_url
    jobs <- stringr::str_match(job_url,
                               "github.com/r-universe/([^/]+)/actions/runs/[0-9]+/job/([0-9]+)")
    rows <- which(!is.na(jobs[, 3]))
    out <- data.frame(Package = failing$Package[rows],
                      type = failing$type[rows],
                      branch = failing$branch[rows],
                      error_step = NA_character_,
                      error_cause = NA_character_,
                      log_url = sprintf("https://%s.r-universe.dev/api/actions/logs/%s",
                                        jobs[rows, 2], jobs[rows, 3]))
    out$error_excerpt <- I(rep(list(character()), length(rows)))
    for (i in seq_along(rows)) {
        cached <- !is.null(cache_dir) &&
            file.exists(file.path(cache_dir, paste0(jobs[rows[i], 3], ".log")))
        log <- uni_job_log(jobs[rows[i], 2], jobs[rows[i], 3], cache_dir)
        if (!cached && delay > 0)
            Sys.sleep(delay)
        if (is.null(log))
            next
        parsed <- parse_uni_log(
            log, source = startsWith(failing$uni_failure[rows[i]], "source"))
        out$error_step[i] <- parsed$step
        out$error_cause[i] <- parsed$cause
        out$error_excerpt[[i]] <- parsed$excerpt
    }
    out
}
