cohort_to_calendar <- function(lifetimes, start_day, last_day) {
  tibble::tibble(day = start_day:last_day) %>%
    dplyr::mutate(remaining_upgrades = purrr::map_dbl(.data$day, function(x) {
      length(lifetimes[which(lifetimes >= x)])
    }))
}



#' Simulation of subscribers
#'
#' @param cohorts an integer vector of the count of subscriptions sold per day, assumed continuous from an origin day 1.
#' @param dist the distribution characterizing churn (weibull and lnorm are implemented)
#' @param ... parameters that complete the characterization of churn, like shape = 0.8
#'
#' @return tibble::tibble
#' @export
#'
#' @examples
#' # rep(1e2, 10) is 100 subscriptions simulated for for ten days.
#' subscribers_by_day(cohorts = rep(1e2, 100), dist = "weibull", 0.8, 300)
subscribers_by_day <- function(cohorts, dist = c("weibull", "lnorm"), ...) {
  dist <- match.arg(dist)
  dots <- eval(substitute(alist(...)))
  rng <- paste0("r", substitute(dist))
  tibble::tibble(
    # cohorts = rep(1e2, 365*10) would be like 10 subscriptions sold per day for ten years
    upgrade_day = 1:length(cohorts),
    upgrades = cohorts
  ) %>%
    dplyr::mutate(lifetimes = purrr::map2(.data$upgrade_day, .data$upgrades, function(x, N) { do.call(rng, c(list(n = N), dots)) })) %>%
    dplyr::mutate(lifetime_playout = purrr::pmap(list(lifetimes = .data$lifetimes,
                                                      start_day = .data$upgrade_day,
                                                      last_day = max(.data$upgrade_day)),
                                                 cohort_to_calendar)) %>%
    dplyr::select(.data$upgrade_day, .data$lifetime_playout) %>%
    tidyr::unnest() %>%
    dplyr::group_by(.data$day) %>%
    dplyr::summarise(subscribers = sum(.data$remaining_upgrades)) %>%
    dplyr::mutate(group = paste(c(dist, dots), collapse = ", "))
}
