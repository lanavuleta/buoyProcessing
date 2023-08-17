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

  # Will always expect that the first column is the datetime
  date_col <- 1

  # To standardize the datetime column name
  colnames(data)[date_col] <- "datetime"

  # Set the timezone because default would be to
  # use the computer's datetime, which can lead to different datetimes
  # between the different datasets
  data <- mutate(data, datetime = as.POSIXct(datetime,
                                             format = datetime_format,
                                             tz = timezone))

  if (all(is.na(data$datetime))) {
    stop(paste("Issue with the input datetime format. It looks like the format",
               "input does not match up with the structure of the datetime",
               "column.\nOpen the buoy data file in Notepad to see how",
               "it is stored and try using the tool again."),
         call. = FALSE)
  }

  return(data)

}

#' Title
#'
#' @inheritParams process_buoy
#'
#' @importFrom stringi stri_replace_all_regex
#'
#' @return string. The datetime format in R-readable format
#' @export
format_datetime_string <- function(datetime_format) {

  datetime_format <- stri_replace_all_regex(datetime_format,
                                            c("yyyy", "yy", "mm", "dd",
                                              "hh",   "HH", "MM", "SS",
                                              "am",   "pm", "AM", "PM"),
                                            c("%Y",   "%y", "%m", "%d",
                                              "%I",   "%H", "%M", "%S",
                                              "%P",   "%P", "%p", "%p"),
                                            vectorize_all = FALSE)

  return(datetime_format)

}
