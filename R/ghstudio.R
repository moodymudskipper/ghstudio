#' @importFrom utils unzip download.file
NULL

globalVariables(c("hash1", "hash2"))


# #' @export
# get_git_log <- function(...) {
#   log_char <- system('git log --pretty=format:"%h;;%an;;%ad;;%s"', intern = TRUE)
#   log_mat <- do.call(rbind, strsplit(log_char, ";;"))
#   colnames(log_mat) <- c("hash", "author", "date", "title")
#   dplyr::filter(as.data.frame(log_mat), ...)
# }

#' Current repo
#'
#' Fetches repo from description
#' @export
current_repo <- function() {
  desc <- readLines("DESCRIPTION")
  bug_report_line <- grep("^BugReports", desc, value = TRUE)
  sub(".*github\\.com/(.*)/issues.*", "\\1", bug_report_line)
}

get_bugreports_url <- function(repo = current_repo()) {
  file.path("https://github.com/", repo, "issues")
}

#' View issue
#' @param id An integerish
#' @param repo A string
#'
#' @export
view_issue <- function(id, repo = current_repo()) {
  tmp <- tempfile(fileext= ".html")
  issue_url <- file.path(get_bugreports_url(repo), "/", id)
  download.file(issue_url, tmp, quiet = TRUE)
  tmp_lines <- readLines(tmp)
  tmp_lines <- c(paste0('<a href="', issue_url, '"> Go to issue</a>'),
                 tmp_lines)
  writeLines(tmp_lines, tmp)
  html <- xml2::read_html(tmp)
  classes <- c(
    ".Header-old", # black header
    ".UnderlineNav", # Code, Issues,..
    ".gh-header-actions", # New issue, Jump to bottom
    ".discussion-timeline-actions",  # Sign up for free etc
    ".footer" # terms, privacy etc
  )
  for(cl in classes) {
    items <- rvest::html_nodes(html, cl)
    xml2::xml_remove(items)
  }
  xml2::write_html(html, file = tmp)
  rstudioapi::viewer(tmp)
}


#' View issues
#'
#' View issues in viewer, `view_assigned()`, `view_created()` and `view_mentions()`
#'   are wrappers around `view_issues()`.
#'
#' @param author A string or `NULL`
#' @param assignee A string or `NULL`
#' @param mentions A string or `NULL`
#' @param open A boolean, not `NA`
#' @param pr A boolean, `NA` for both PRs and issues
#' @param repo A string
#' @param user A string
#'
#' @return Returns `NULL` invisibly, called for side effects.
view_issues <- function(
  author = NULL,
  assignee = NULL,
  mentions = NULL,
  open = TRUE,
  pr = NA,
  repo = current_repo()) {
  open <- if(open) "open" else "closed"
  url <- paste0(get_bugreports_url(repo), "?q=is%3A", open)
  if (!is.null(assignee)) url <- paste0(url, "+assignee%3A", assignee)
  if (!is.null(author)) url <- paste0(url, "+author%3A", author)
  if (!is.null(mentions)) url <- paste0(url, "+mentions%3A", mentions)
  if(!is.na(pr)) {
    if(pr) {
      url <- paste0(url, "+is%3Apr")
    } else {
      url <- paste0(url, "+is%3Aissue")
    }
  }

  tmp <- tempfile(fileext= ".html")
  download.file(url, tmp, quiet = TRUE)
  tmp_lines <- readLines(tmp)
  tmp_lines <- c(paste0('<a href="', url, '"> Go to page</a>'),
                 tmp_lines)
  writeLines(tmp_lines, tmp)
  html <- xml2::read_html(tmp)

  classes <- c(
    ".Header-old", # black header
    ".UnderlineNav", # Code, Issues,..
    ".mb-md-3",
    ".issues-reset-query-wrapper",
    ".d-block",
    ".text-center",
    ".Box-header",
    ".gh-header-actions", # New issue, Jump to bottom
    ".discussion-timeline-actions",  # Sign up for free etc
    ".footer" # terms, privacy etc
  )
  for(cl in classes) {
    items <- rvest::html_nodes(html, cl)
    xml2::xml_remove(items)
  }
  xml2::write_html(html, file = tmp)
  rstudioapi::viewer(tmp)
}

#' @export
#'
#' @rdname view_issues
view_assigned <- function(user = github_name(), open = TRUE, pr = NA,
                          repo = current_repo()) {
  view_issues(assignee = user, open = open, pr = pr, repo = repo)
}

#' @export
#'
#' @rdname view_issues
view_created <- function(user = github_name(), open = TRUE, pr = NA,
                         repo = current_repo()) {
  view_issues(author = user, open = open, pr = pr, repo = repo)
}

#' @export
#'
#' @rdname view_issues
view_mentions <- function(user = github_name(), open = TRUE, pr = NA,
                          repo = current_repo()) {
  view_issues(mentions = user, open = open, pr = pr, repo = repo)
}

#' Github name
#' @export
github_name <- function() {
  nm <- getOption("ghstudio.github_name")
  if (is.null(nm)) {
    stop("You must set `options(ghstudio.github_name=)`")
  }
  nm
}

#' view diff between commits
#'
#' @param sha1,sha2 sha or branch name
#' @inheritParams diffobj::diffFile
#' @param ... Passed to diffobj::diffFile
#'
#' @return Returns `NULL` invisibly, called for side effects.
#' @export
view_diff <- function(
  sha1 = "HEAD",
  sha2 = paste0(sha1, "~"),
  mode = c("auto", "unified", "sidebyside", "context"),
  context = 2,
  ...) {
  mode <- match.arg(mode)
  # run
  tmp_file1 <- tempfile(fileext = ".zip")
  tmp_dir1 <- tempfile()
  dir.create(tmp_dir1)
  tmp_file2 <- tempfile(fileext = ".zip")
  tmp_dir2 <- tempfile()
  dir.create(tmp_dir2)
  empty_file <- tempfile()
  on.exit({
    unlink(tmp_file1)
    unlink(tmp_file2)
    unlink(tmp_dir1, recursive = TRUE)
    unlink(tmp_dir2, recursive = TRUE)
    unlink(empty_file)
  })
  # download both
  cmd1 <- sprintf("git archive --format=zip --output=%s %s", tmp_file1, sha1)
  cmd2 <- sprintf("git archive --format=zip --output=%s %s", tmp_file2, sha2)
  system(cmd1)
  system(cmd2)
  # unzip both
  unzip(tmp_file1, exdir = tmp_dir1)
  unzip(tmp_file2, exdir = tmp_dir2)
  # hash all files

  file1 <- list.files(tmp_dir1, full.names = TRUE, recursive = TRUE)
  file2 <- list.files(tmp_dir2, full.names = TRUE, recursive = TRUE)
  df1 <- data.frame(
    file1,
    hash1 = sapply(file1, digest::digest, file = TRUE, USE.NAMES = FALSE),
    file = sub(paste0(tmp_dir1, "/"), "", file1, fixed = TRUE)
  )
  df2 <- data.frame(
    file2,
    hash2 = sapply(file2, digest::digest, file = TRUE, USE.NAMES = FALSE),
    file = sub(paste0(tmp_dir2, "/"), "", file2, fixed = TRUE)
  )
  df <- merge(df1, df2, by = "file", all = TRUE)
  df$file1[is.na(df$file1)] <- empty_file
  df$file2[is.na(df$file2)] <- empty_file

  diff_df <- subset(df, hash1 != hash2)
  print(mapply(
    diff_df$file1,
    diff_df$file2,
    diff_df$file,
    FUN = function(file1, file2, file) {
      env <- environment()
      top <- sprintf("%s (%s)", file, sha1)
      bottom <- sprintf("%s (%s)", file, sha2)
      env[[top]] <- file1
      env[[bottom]] <- file2
      eval(bquote(diffobj::diffFile(
        .(as.name(top)),
        .(as.name(bottom)),
        mode = mode,
        context = context,
        ...
      )))
    }))
  invisible(NULL)
}
