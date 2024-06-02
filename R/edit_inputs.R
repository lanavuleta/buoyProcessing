#' Edit sensor maintenance sheet
#' Format the sensor maintenance dates to be in propse datetime format and edit
#' the datetimes to start 10 minutes earlier and end 30 minutes later to account
#' for the time period during which the boat is settling.
#'
#' @param sensor_maint dataframe. Output from read_sensor_maint()
#'
#' @importFrom dplyr mutate select
#' @importFrom lubridate year month day hour minute minutes
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
edit_sensor_maint <- function(sensor_maint) {

  sensor_maint <- sensor_maint %>%
    mutate(start_datetime = as.POSIXct(start_datetime,
                                       tryFormats = c("%Y-%m-%d %H:%M",
                                                      "%Y-%m-%d %H:%M:%S",
                                                      "%Y-%m-%d %I:%M %p",
                                                      "%Y-%m-%d %I:%M:%S %p",
                                                      "%Y-%m-%d %I:%M %P",
                                                      "%Y-%m-%d %I:%M:%S %P"))
           # Will flag X an additional 10 min before the
           # end of the maintenance to allow parameters
           # to settle
           - minutes(10),
           end_datetime   = as.POSIXct(end_datetime,
                                       tryFormats = c("%Y-%m-%d %H:%M",
                                                      "%Y-%m-%d %H:%M:%S",
                                                      "%Y-%m-%d %I:%M %p",
                                                      "%Y-%m-%d %I:%M:%S %p",
                                                      "%Y-%m-%d %I:%M %P",
                                                      "%Y-%m-%d %I:%M:%S %P"))
           # Will flag X an additional 30 min after the
           # end of the maintenance to allow parameters
           # to settle
           + minutes(30)) %>%
    select(start_datetime, end_datetime, flag)

}

#' Edit error drift sheet
#' Calculate and store specific values required to later calculate error.
#'
#' @param error_drift dataframe. Output from read_error_drift()
#' @inheritParams read_error_drift
#'
#' @importFrom dplyr mutate select rowwise
#' @importFrom tidyr nest
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
edit_error_drift <- function(error_drift, sensor_maint) {

  check_error_drift(error_drift)

  error_drift <- match_error_maint(error_drift, sensor_maint)

  check_error_maint_match(error_drift)

  error_drift <- error_drift %>%
    mutate(error_calib = abs(pre_calibration - post_calibration),
           error_clean = abs(pre_clean - post_clean),
           error_calib_perc = 100*(error_calib/pre_calibration),
           error_clean_perc = 100*(error_clean/pre_clean)) %>%
    # Need to do rowwise because sum returns the sum of ALL values present in
    # its arguments, and we must use sum to remove NAs
    rowwise() %>%
    mutate(error = case_when(is.na(error_calib) & is.na(error_clean) ~ NA,
                             TRUE ~ sum(error_calib, error_clean, na.rm = TRUE)),
           # Need error percent if accuracy is given as %age
           error_perc = sum(error_calib_perc, error_clean_perc, na.rm = TRUE),
           # Need to store the larger value in case if accuracy differs in different
           # ranges. We always go with the bigger value
           value = max(pre_calibration, post_calibration, pre_clean, post_clean, na.rm = TRUE)) %>%
    select(-c(pre_calibration, post_calibration, pre_clean, post_clean,
              error_calib, error_clean, error_calib_perc, error_clean_perc, date)) %>%
    # There might be numerous calibrations over a season for each sensor
    nest(error_info = c(end_datetime, error, error_perc, value))

}

#' Edit sensor characteristics sheet
#' Format the accuracy column for better read-in by the tool.
#'
#' @param sensor_chars dataframe. Output from read_sensor_chars()
#' @inheritParams read_sensor_chars
#'
#' @importFrom dplyr rowwise mutate
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
edit_sensor_chars <- function(sensor_chars, error_drift) {

  # To keep track of each sensor and not have to join later (avoids any issues
  # that might arise from sensors being given the same name)
  sensor_chars <- mutate(sensor_chars, row_num = row_number())

  # We only need the sensors that will have error assigned to them to follow
  # the correct accuracy formatting
  sensor_chars_accuracy <- sensor_chars %>%
    filter(sensor_header %in% error_drift$sensor_header)

  sensor_chars_accuracy <- sensor_chars_accuracy %>%
    # NA unit messes with the paste in regex pattern creation below
    mutate(unit = ifelse(is.na(unit), "", unit),
           # Looking for accuracy in the form of:
           # 1 m/s (0-10m/s), 2 m/s (10-20m/s)
           accuracy_regex_range = paste0("(\\+/-)? *(?<accuracy>(\\d*\\.)?\\d+) *(?<unit>%|",
                                         unit,
                                         ") *(\\( *(?<lowbound>[+-]?(\\d*\\.)?\\d+) *",
                                         "- *(?<highbound>[+-]?(\\d*\\.)?\\d+) *(",
                                         unit,
                                         ")? *\\))"),
           # Looking for accuracy in the form of:
           # 1 m/s
           accuracy_regex_basic = paste0("^(\\+/-)? *(?<accuracy>(\\d*\\.)?\\d+) *(?<unit>%|", unit, ")$")) %>%
    select(sensor_header, unit, accuracy, accuracy_regex_range, accuracy_regex_basic, row_num)

  formatted_accuracy <- apply(sensor_chars_accuracy, 1, format_accuracy)

  for (i in 1:nrow(sensor_chars_accuracy)) {
    sensor_chars_accuracy$accuracy[i] <- formatted_accuracy[i]
  }

  sensor_chars_accuracy <- sensor_chars_accuracy %>%
    select(row_num, accuracy)

  sensor_chars <- sensor_chars %>%
    left_join(sensor_chars_accuracy, by = "row_num") %>%
    # We don't further use the accuracies listed for the other sensors. Only
    # needed for sensors to which we will assign grade
    mutate(accuracy = accuracy.y) %>%
    select(-c(row_num, accuracy.x, accuracy.y))

}

#' Add error drift into to sensor characteristics dataframe
#' Enrich the sensor characteristics dataframe with the information from the error
#' drift sheet.
#'
#' @inheritParams edit_sensor_chars
#' @inheritParams edit_error_drift
#'
#' @return dataframe
#' @export
combine_chars_with_error <- function(sensor_chars, error_drift) {

  # Switch NA units to none to be able to match - R does not match NAs to NAs
  sensor_chars <- sensor_chars %>%
    mutate(unit = ifelse(is.na(unit), "none", unit))

  error_drift <- error_drift %>%
    mutate(unit = ifelse(is.na(unit), "none", unit))

  check_chars_error_match(sensor_chars, error_drift)

  sensor_chars <- sensor_chars %>%
    match_chars_error(error_drift) %>%
    select(sensor_header, unit, accuracy,
           operating_range_min, operating_range_max,
           roc_threshold, repeat_0s_max,
           local_range_min, local_range_max,
           error_info) %>%
    mutate(unit = ifelse(unit == "none", NA, unit))

  return(sensor_chars)

}
