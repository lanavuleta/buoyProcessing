#' Title
#'
#' @inheritParams flag
#'
#' @importFrom lubridate time_length hours
#'
#' @return numeric vector
#' @export
get_flag_intervals <- function(data) {

  # When deciding what counts as a small block of missing values, select the
  # smaller option of the following: number of rows that cover 5 hours worth
  # of data OR 10 data points
  time_small <- min(time_length(hours(5))/time_length(data$datetime[2] - data$datetime[1]),
                    10)
  # When deciding what counts as a large block of missing values, select the
  # bigger option of the following: number of rows that cover 24 hours worth
  # of data OR 50 data points
  time_large <- max(time_length(hours(24))/time_length(data$datetime[2] - data$datetime[1]),
                    50)

  flag_intervals <- c(time_small = time_small, time_large = time_large)

}

#' Title
#'
#' @param values numeric vector. Values used to calculate the 99 CI
#'
#' @importFrom stats sd qnorm
#'
#' @return numeric vector
#' @export
calculate_99_ci <- function(values) {

  values_mean <- mean(values)
  n <- length(values)
  standard_deviation <- sd(values)

  error <- qnorm(0.995) * standard_deviation/sqrt(n)

  return(c(bound_lower = values_mean - error,
           bound_upper = values_mean + error))

}

match_chars_error <- function(sensor_chars, error_drift) {

  matches_char_error <- regex_join(sensor_chars, error_drift,
                                   by = c("sensor_header" = "sensor", "unit"),
                                   mode = "left") %>%
    # unit from error_drift not needed. If there was a match, value is in
    # unit.x, and if there was not a match, this would have been caught with
    # error_missing
    mutate(unit = unit.x) %>%
    select(-c(unit.x, unit.y))

}

