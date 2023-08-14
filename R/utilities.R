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


#' Title
#'
#' @param sca dataframe. sensor_chars as edited in edit_sensor_chars()
#'
#' @importFrom stringr str_match str_match_all
#'
#' @return dataframe (or the formatted accuracy) OR NA if no accuracy was
#'  detected OR error message if accuracy was incorrectly input
#' @export
format_accuracy <- function(sca) {

  # apply() gives the row as a named character vector. as.list() makes it easier
  # to access each element by name
  sca <- as.list(sca)

  if (is.na(sca$accuracy) | grepl("Unknown", sca$accuracy, ignore.case = TRUE)) {
    stop(paste("Issue with the accuracy listed for sensor", sca$sensor_header,
               "\nTrying to calculate error and",
               "assign grades. For each of the parameters listed in the error",
               "drift sheet, the sensor characteristics sheet must list an",
               "accuracy, as the accuracy is used to determine the grade.",
               "\nIn the sensor characteristics sheet, add an accuracy to the",
               "parameters listed above and try again."),
         call. = FALSE)
  } else {

    if (!grepl("\\(([+-]?(\\d*\\.)?\\d+) *- *([+-]?(\\d*\\.)?\\d+)", sca$accuracy)) {

      # Are working with basic regex and accuracy (just in the form of
      # +/- value % OR +/- value unit)
      formatted_accuracy <- sca$accuracy %>%
        str_match(sca$accuracy_regex_basic) %>%
        as.data.frame() %>%
        select(accuracy, unit) %>%
        mutate(accuracy = as.numeric(accuracy))

      # If unit doesn't match with the correct unit or %, an incorrect unit was written in the accuracy cell.
      # If unit is NA, no match was completed.
      if ((is.na(formatted_accuracy$unit)) |
          ((formatted_accuracy$unit != sca$unit) & (formatted_accuracy$unit != "%"))) {
        stop(paste("Issue with the accuracy listed for sensor", sca$sensor_header,
                   "\nThe tool identified this as a basic accuracy (ie same accuracy",
                   "applies for the entire range). The expected accuracy setup is:",
                   "\n\t'+/- accuracy %' OR '+/- accuracy unit'",
                   "\n\tex:'+/- 1 %' OR '+/- 0.2 NTU'",
                   "\n, where 'accuracy' is a numeric value indicating the accuracy and 'unit'",
                   "matches with the unit listed for the sensor in the Sensor",
                   "Characteristics sheet.",
                   "\nEdit the Sensor Characteristics sheet and try again."),
             call. = FALSE)
      }
    } else if (grepl("\\(([+-]?(\\d*\\.)?\\d+) *- *([+-]?(\\d*\\.)?\\d+)", sca$accuracy)) {
      # Are working with complex regex that exists in separate ranges (in the form of
      # +/- value1 % OR unit (minrange1 - maxrange1 unit), +/- value2 % OR unit (minrange2 - maxrange2 unit), ...)
      formatted_accuracy <- sca$accuracy %>%
        str_match_all(sca$accuracy_regex_range) %>%
        as.data.frame() %>%
        select(accuracy, unit, lowbound, highbound) %>%
        mutate(accuracy = as.numeric(accuracy),
               lowbound = as.numeric(lowbound),
               highbound = as.numeric(highbound))

      issues <- formatted_accuracy %>%
        mutate(unit_issue = ifelse((is.na(formatted_accuracy$unit)) |
                                     ((formatted_accuracy$unit != sca$unit) &
                                        (formatted_accuracy$unit != "%")),
                                   TRUE, FALSE),
               # Checking if lowbound and highbound are always smaller and bigger AND if
               # highbound of one row is the same lowbound of the next row (inclusive in
               # first row and exclusive in second row. To make sure that all ranges are
               # covered)
               difference_issue = ifelse(lowbound > highbound, TRUE, FALSE),
               range_issue = ifelse(highbound != lead(lowbound), TRUE, FALSE))

      # If unit doesn't match with the correct unit or %, an incorrect unit was written in the accuracy cell.
      # If unit is NA, no match was completed.
      if (any(issues$unit_issue)) {
        stop(paste("Issue with the accuracy listed for sensor", sca$sensor_header,
                   "\nThe tool identified this as an advanced accuracy (ie different accuracy",
                   "applies for different ranges). The expected accuracy setup is:",
                   "\n\t'+/- value1 x (range_min1 - range_max1 unit), +/- value2 y (range_min2 - range_max2 unit) ...'",
                   "\n, where x and y are either the unit as is listed for that sensor in Sensor Characteristics.",
                   "\nEdit the Sensor Characteristics sheet and try again."),
             call. = FALSE)
      }

      if (any(issues$difference_issue)) {
        stop(paste("Issue with the accuracy listed for sensor", sca$sensor_header,
                   "\nEnsure that when specifying the range for which an accuracy",
                   "applies that the lower bound is less than or equal to the upper bound.",
                   "\nFor example, an accuracy that is:",
                   "\n\t'3 NTU (11-10 NTU)'",
                   "\nis invalid.",
                   "\nEdit the Sensor Characteristics sheet and try again."),
             call. = FALSE)
      }

      if (any(issues$range_issue, na.rm = TRUE)) {
        stop(paste("Issue with the accuracy listed for sensor", sca$sensor_header,
                   "\nEnsure that when specifying the range for which an accuracy",
                   "applies that the upper bound of one range is equal to the lower",
                   "bound of the following range.",
                   "\nFor example, an accuracy that is:",
                   "\n\t'3 NTU (0-10), 4 NTU (11-20)'",
                   "\nis invalid, because no accuracy is applied between 10 and 20.",
                   "\nRecall that the upper bound is inclusive (up to and including)",
                   "whereas the lower bound is exclusive (strictly greater than).",
                   "\nEdit the Sensor Characteristics sheet and try again."),
             call. = FALSE)
      }
    } else {
      stop(paste("Issue with the accuracy listed for sensor", sca$sensor_header,
                 "\nCould not identify the accuracy. Expected accuracy follows one",
                 "of the four options listed below:",
                 "\n\t Empty cell \t\tif accuracy is unknown",
                 "\n\t 'Unknown' \t\tif accuracy is unknown",
                 "\n\t +/- value unit \t where unit is the same as is listed for that sensor in Sensor Characteristics",
                 "\n\t +/- value % \t\t if accuracy is given as a percent rather than as a unit",
                 "\n\t +/- value1 x (range_min1 - range_max1 unit), +/- value2 y (range_min2 - range_max2 unit)",
                 "\n\t\t, where x and y are either the unit as is listed for that sensor in Sensor Characteristics",
                 "or %"))
    }
  }

  return(formatted_accuracy)

}
