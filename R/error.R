#' Calculate data grade
#' Calculate data grade based on how the sensor error determined during sensor
#' calibration and described in the error drift buoy info sheet compares to the
#'  sensor accuracy as defined in the sensor characteristics sheet.
#'
#' @inheritParams error_poi
#'
#' @importFrom dplyr arrange mutate case_when
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
calculate_grade <- function(sensor_accuracy, error_info) {

  if (!"lowbound" %in% colnames(sensor_accuracy)) {
    # Simple accuracy, in the form of "+/- value %" or "+/- value unit"
    error_info <- error_info %>%
      mutate(accuracy = sensor_accuracy$accuracy,
             accuracy_unit = sensor_accuracy$unit)
  } else {
    # Complex accuracy with different accuracy for different range
    error_info <- sqldf("SELECT * FROM error_info
                         LEFT JOIN sensor_accuracy
                         ON error_info.value >= sensor_accuracy.lowbound AND
                            error_info.value < sensor_accuracy.highbound") %>%
      select(end_datetime, error, error_perc, accuracy, accuracy_unit = unit)
  }

  error_info <- error_info %>%
    arrange(end_datetime) %>%
    # If unit is % we want to examine the error as percentage, otherwise we
    # want to examine the absolute difference error
    mutate(error = ifelse(accuracy_unit == "%", error_perc, error)) %>%
    mutate(grade = case_when(is.na(error)        ~ NA,
                             error <= accuracy   ~ "E",
                             error <= accuracy*2 ~ "VG",
                             error <= accuracy*4 ~ "G",
                             error <= accuracy*6 ~ "F",
                             TRUE                ~ "P"))

}
