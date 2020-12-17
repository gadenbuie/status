# Library ----
library(gh)
library(memoise)
library(purrr)
library(dplyr)
library(tidyr)
library(glue)
library(readr)


# gh functions ----
gh_workflows <- memoise::memoise(function(owner, repo, ...) {
  gh("/repos/{owner}/{repo}/actions/workflows", owner = owner, repo = repo) %>%
    .$workflows
}, cache = cache_memory())

gh_runs <- memoise::memoise(function(owner, repo, workflow_id, ...) {
  gh(
    "/repos/{owner}/{repo}/actions/workflows/{workflow_id}/runs",
    owner = owner,
    repo = repo,
    workflow_id = workflow_id,
    per_page = 1
  )$workflow_runs[[1]]
}, cache = cache_memory())

gh_url <- memoise::memoise(function(url) {
  gh(url)
}, cache = cache_memory())


# Get repos
gh_get_repo_status <- function(repos) {
  repos <- map_dfr(repos, ~ tibble(repo = .), .id = "owner")

  repos$workflows <- repos %>% pmap(gh_workflows)
  repos <- repos %>%
    unnest(workflows) %>%
    mutate(
      workflow_id = map_chr(workflows, "id"),
      badge_url = map_chr(workflows, "badge_url")
    )

  repos$runs <- pmap(repos, gh_runs)
  repos <- repos %>%
    mutate(
      html_url_run = map_chr(runs, "html_url"),
      run_conclusion = map_chr(runs, "conclusion"),
      commit_message = map_chr(runs, ~ .x$head_commit$message),
      commit_id = map_chr(runs, `[[`, c("head_commit", "id")),
      repo_name = map_chr(runs, `[[`, c("head_repository", "full_name")),
      html_url_repo = map_chr(runs, `[[`, c("head_repository", "html_url")),
      .repo = map(map_chr(runs, `[[`, c("head_repository", "url")), gh_url),
      stargazers_count = map_dbl(.repo, "stargazers_count"),
      watchers_count = map_dbl(.repo, "watchers_count"),
      open_issues_count = map_dbl(.repo, "open_issues_count"),
      forks_count = map_dbl(.repo, "forks_count"),
      open_issues_count = map_dbl(.repo, "open_issues_count")
    )

  repos %>%
    select_if(negate(is.list)) %>%
    arrange(repo_name) %>%
    write_csv("repos.csv")

  repos
}
