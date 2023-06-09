#' Title
#'
#' @inheritParams flag_and_error
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
#' @param data dataframe. Output from flag_and_error
#'
#' @return dataframe
#' @export
handle_dup_names <- function(data) {
  for (i in 1:ncol(data)) {
    if (grepl("\\.\\.\\.\\d{1,2}", colnames(data)[i])) {
      colnames(data)[i] <- gsub("\\.\\.\\.\\d{1,2}", "", colnames(data)[i])
    }
  }

  return(data)
}

