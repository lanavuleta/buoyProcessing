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

  values_mean <- mean(values, na.rm = TRUE)
  n <- length(values)
  standard_deviation <- sd(values, na.rm = TRUE)

  error <- qnorm(0.995) * standard_deviation/sqrt(n)

  return(c(bound_lower = values_mean - error,
           bound_upper = values_mean + error))

}
