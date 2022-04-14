# Library ----
library(gh)
library(memoise)
library(purrr)
library(dplyr)
library(tidyr)
library(glue)
library(readr)


# gh functions ----
gh_workflows <- function(owner, repo, ...) {
  tryCatch(
    gh("/repos/{owner}/{repo}/actions/workflows", owner = owner, repo = repo) %>%
      .$workflows,
    error = function(e) NULL
  )
}

gh_runs <- function(owner, repo, workflow_id, ...) {
  gh(
    "/repos/{owner}/{repo}/actions/workflows/{workflow_id}/runs",
    owner = owner,
    repo = repo,
    workflow_id = workflow_id,
    per_page = 1
  )$workflow_runs[[1]]
}

gh_url <- function(url) {
  gh(url)
}

gh_repo <- function(owner, repo, ...) {
  gh("/repos/{owner}/{repo}", owner = owner, repo = repo)
}

gh_owner_repos <- function(owner) {
  gh("/users/{username}/repos", username = owner, .limit = Inf, type = "owner") %>%
    map(keep, negate(is.null)) %>%
    map(keep, negate(is.list)) %>%
    map_dfr(as_tibble) %>%
    filter(!private, !fork) %>%
    mutate(owner = owner) %>%
    mutate(subscribers_count = map(subscribers_url, gh) %>% map_int(length)) %>%
    select(owner, repo = name, full_name, contains("count"), html_url_repo = html_url, fork) %>%
    arrange(desc(stargazers_count))
}

# memoize everything to avoid repeated interactive calls over a short period
gh_workflows <- memoise::memoise(gh_workflows, cache = cache_memory())
gh_runs <- memoise::memoise(gh_runs, cache = cache_memory())
gh_url <- memoise::memoise(gh_url, cache = cache_memory())
gh_repo <- memoise::memoise(gh_repo, cache = cache_memory())
gh_owner_repos <- memoise::memoise(gh_owner_repos, cache = cache_memory())

gh_repo_stats <- function(repos) {
  # repos should be a tibble now with owner, repo
  repos$.repo <- pmap(repos, gh_repo)
  repos %>%
    mutate(
      full_name = map_chr(.repo, "full_name"),
      stargazers_count = map_dbl(.repo, "stargazers_count"),
      subscribers_count = map_dbl(.repo, "subscribers_count"),
      open_issues_count = map_dbl(.repo, "open_issues_count"),
      forks_count = map_dbl(.repo, "forks_count"),
      open_issues_count = map_dbl(.repo, "open_issues_count"),
      fork = map_lgl(.repo, "fork"),
      html_url_repo = map_chr(.repo, "html_url")
    )
}

gh_repo_workflows <- function(repos) {
  # repos should be a tibble with owner, repo
  repos$.workflows <- repos %>% pmap(gh_workflows)

  workflows <- repos %>%
    unnest(.workflows) %>%
    mutate(
      workflow_id = map_chr(.workflows, "id"),
      badge_url = map_chr(.workflows, "badge_url")
    )

  workflows$runs <- pmap(workflows, gh_runs)
  workflows %>%
    mutate(
      event = map_chr(runs, "event"),
      html_url_run = map_chr(runs, "html_url"),
      run_conclusion = map_chr(runs, "conclusion", .default = NA_character_),
      commit_message = map_chr(runs, ~ .x$head_commit$message),
      commit_id = map_chr(runs, `[[`, c("head_commit", "id"))
    )
}

# Get repos
gh_get_repo_status <- function(
  repo_list = NULL,
  all_by_owner = NULL,
  .write_csv = !interactive()
) {
  if (is.null(repo_list) && is.null(all_by_owner)) {
    stop("At least one repo must be listed or a username must be provided in `all_by_owner`")
  }

  by_vars <- c("owner", "repo")

  repos <- if (!is.null(repo_list)) {
    repo_list %>%
      map_dfr(~ tibble(repo = .), .id = "owner") %>%
      gh_repo_stats()
  }

  if (!is.null(all_by_owner)) {
    owner_repos <- gh_owner_repos(all_by_owner)
    if (!is.null(repos)) {
      owner_repos <- owner_repos %>% anti_join(repos, by = by_vars)
    }
    repos <- bind_rows(repos, owner_repos)
  }

  workflows <- repos %>% select(owner, repo) %>% gh_repo_workflows()

  workflows <-
    workflows %>%
    filter(event %in% c("push", "schedule")) %>%
    mutate(badge = glue::glue("[![]({badge_url})]({html_url_run})")) %>%
    group_by(owner, repo, commit_id, commit_message) %>%
    summarize(badge = paste(badge, collapse = " ")) %>%
    ungroup()

  repos <- bind_rows(
    inner_join(repos, workflows, by = by_vars),
    anti_join(repos, workflows, by = by_vars)
  )

  if (isTRUE(.write_csv)) {
    repos %>%
      select_if(negate(is.list)) %>%
      arrange(full_name) %>%
      write_csv("repos.csv")
  }

  repos
}
