#' Map lifetimes to calendar days
#'
#' @param lifetimes a numeric vector of lifetime durations
#' @param start_day a length one numeric vector allowing new cohorts to be added in a ragged fashion (as they come)
#'
#' @return tibble::tibble
#' @export
#'
#' @examples
#' cohort_to_calendar(lifetimes = rweibull(100, 0.8, 500))
cohort_to_calendar <- function(lifetimes, start_day = 1) {
  tibble::tibble(day = start_day:(10*365)) %>%
    dplyr::mutate(remaining_upgrades = purrr::map_dbl(day, function(x) {
      length(lifetimes[which(lifetimes >= x)])
    }))
}

#' Simulation of subscribers
#'
#' @param name a length one character vector describing the name of the simulation
#' @param rng a random number generator reflecting subscription lifetimes
#'
#' @return tibble::tibble
#' @export
#'
#' @examples
#' subscribers_by_day("My experiment", function(x) { rweibull(x, 0.8, 500) })
subscribers_by_day <- function(name, rng) {
  tibble::tibble(
    upgrade_day = 1:(10*365),
    upgrades = 1e1
  ) %>%
    dplyr::mutate(lifetimes = purrr::map2(upgrade_day, upgrades, function(x, N) { rng(N) })) %>%
    dplyr::mutate(lifetime_playout = purrr::map2(lifetimes, upgrade_day, cohort_to_calendar)) %>%
    dplyr::select(upgrade_day, lifetime_playout) %>%
    tidyr::unnest() %>%
    dplyr::group_by(day) %>%
    dplyr::summarise(subscribers = sum(remaining_upgrades)) %>%
    dplyr::mutate(group = !!name)
}
