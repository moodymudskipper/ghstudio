
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ghstudio

Experimental tools to use git/github with RStudio, e.g see issues and
diffs in the viewer

## Installation

install with :

``` r
remotes::install_github("moodymudskipper/ghstudio")
```

## Example

``` r
# view single issue thread
view_issue(1, "RDatatable/data.table")
# view list of issues, with optional filters (see next block)
view_issues("RDatatable/data.table") 

# repo is guessed from DESCRIPTION if not provided
view_mentions("moodymudskipper", repo = "RDatatable/data.table")
view_created("moodymudskipper", repo = "RDatatable/data.table")
view_assigned("moodymudskipper", repo = "RDatatable/data.table")
# and 1st arg is optional
options(ghstudio.github_name = "moodymudskipper")
view_mentions(repo = "RDatatable/data.table")

# view diff in viewer, only on local git project for now
view_diff(sha1, sha2) # with more options passed to diffobj::diffFile()
```

``` r
library(ghstudio)
lapply(mget(sort(getNamespaceExports("ghstudio")), inherits = TRUE), args)
#> $current_repo
#> function () 
#> NULL
#> 
#> $github_name
#> function () 
#> NULL
#> 
#> $view_assigned
#> function (user = github_name(), open = TRUE, pr = NA, repo = current_repo()) 
#> NULL
#> 
#> $view_created
#> function (user = github_name(), open = TRUE, pr = NA, repo = current_repo()) 
#> NULL
#> 
#> $view_diff
#> function (sha1 = "HEAD", sha2 = paste0(sha1, "~"), mode = c("auto", 
#>     "unified", "sidebyside", "context"), context = 2, ...) 
#> NULL
#> 
#> $view_issue
#> function (id, repo = current_repo()) 
#> NULL
#> 
#> $view_issues
#> function (author = NULL, assignee = NULL, mentions = NULL, open = TRUE, 
#>     pr = NA, repo = current_repo()) 
#> NULL
#> 
#> $view_mentions
#> function (user = github_name(), open = TRUE, pr = NA, repo = current_repo()) 
#> NULL
```
