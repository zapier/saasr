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
    dplyr::mutate(remaining_upgrades = purrr::map_dbl(.data$day, function(x) {
      length(lifetimes[which(lifetimes >= x)])
    }))
}

#' Simulation of subscribers
#'
#' @param dist the distribution characterizing churn (weibull and lnorm are implemented)
#' @param ... parameters that complete the characterization of churn, like shape = 0.8
#'
#' @return tibble::tibble
#' @export
#'
#' @examples
#' subscribers_by_day("weibull", 0.8, 300)
subscribers_by_day <- function(dist = c("weibull", "lnorm"), ...) {
  dist <- match.arg(dist)
  dots <- eval(substitute(alist(...)))
  rng <- paste0("r", substitute(dist))
  tibble::tibble(
    upgrade_day = 1:(10*365),
    upgrades = 1e1
  ) %>%
    dplyr::mutate(lifetimes = purrr::map2(.data$upgrade_day, .data$upgrades, function(x, N) { do.call(rng, c(list(n = N), dots)) })) %>%
    dplyr::mutate(lifetime_playout = purrr::map2(.data$lifetimes, .data$upgrade_day, cohort_to_calendar)) %>%
    dplyr::select(.data$upgrade_day, .data$lifetime_playout) %>%
    tidyr::unnest() %>%
    dplyr::group_by(.data$day) %>%
    dplyr::summarise(subscribers = sum(.data$remaining_upgrades)) %>%
    dplyr::mutate(group = paste(c(dist, dots), collapse = ", "))
}
