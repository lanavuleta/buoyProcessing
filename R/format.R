#' Title
#'
#' @param data dataframe. Output from read_data()
#' @inheritParams process_buoy
#'
#' @importFrom dplyr mutate
#'
#' @return dataframe
#' @export
format_datetime <- function(data, datetime_format, timezone) {

  date_col <- grep("date|time", colnames(data), ignore.case = TRUE)

  if (length(date_col) != 1) {
    # TBD - what do in this case?
    stop(paste("Issue with buoy data. Could not identify a unique date/time column."))
  }

  # To standardize the datetime column name
  colnames(data)[date_col] <- "datetime"

  # Set the timezone because default would be to
  # use the computer's datetime, which can lead to different datetimes
  # between the different datasets
  data <- mutate(data, datetime = as.POSIXct(datetime,
                                             format = datetime_format,
                                             tz = timezone))

}
