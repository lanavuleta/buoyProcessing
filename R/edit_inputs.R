#' Title
#'
#' @param sensor_maint dataframe. Output from read_sensor_maint()
#' @inheritParams process_buoy
#'
#' @importFrom dplyr mutate select
#' @importFrom lubridate year month day hour minute minutes
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
edit_sensor_maint <- function(sensor_maint, timezone) {

  sensor_maint <- sensor_maint %>%
    mutate(start_datetime = as.POSIXct(start_datetime,
                                       tryFormats = c("%Y-%m-%d %H:%M",
                                                      "%Y-%m-%d %H:%M:%S"),
                                       tz = timezone)
           # Will flag X an additional 10 min before the
           # end of the maintenance to allow parameters
           # to settle
           - minutes(10),
           end_datetime   = as.POSIXct(end_datetime,
                                       tryFormats = c("%Y-%m-%d %H:%M",
                                                      "%Y-%m-%d %H:%M:%S"),
                                       tz = timezone)
           # Will flag X an additional 30 min after the
           # end of the maintenance to allow parameters
           # to settle
           + minutes(30)) %>%
    select(start_datetime, end_datetime, flag)

}

#' Title
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
           error_clean = abs(pre_clean - post_clean)) %>%
    # Need to do rowwise because sum returns the sum of ALL values present in
    # its arguments, and we must use sum to remove NAs
    rowwise() %>%
    mutate(error = case_when(is.na(error_calib) & is.na(error_clean) ~ NA,
                             TRUE ~ sum(error_calib, error_clean, na.rm = TRUE))) %>%
    select(-c(pre_calibration, post_calibration, pre_clean, post_clean,
              error_calib, error_clean, date)) %>%
    # There might be numerous calibrations over a season for each sensor
    nest(error_info = c(end_datetime, error))

}

#' Title
#'
#' @param sensor_chars dataframe. Output from read_sensor_chars()
#'
#' @importFrom dplyr rowwise mutate
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
edit_sensor_chars <- function(sensor_chars) {

  check_sensor_chars(sensor_chars)

  sensor_chars <- sensor_chars %>%
    rowwise() %>%
    mutate(accuracy_unit = ifelse(is.na(unit),
                                  NA,
                                  # .*? does lazy matching, and matches the first instance
                                  # and the .* at the end catches anything else
                                  sub(pattern = paste0(".*?\\d+\\.?\\d* *(", unit, ").*"),
                                      replacement = "\\1",
                                      x = accuracy)),
           accuracy_val  = ifelse(is.na(unit),
                                  sub(pattern = paste0(".*?(\\d+\\.?\\d*).*"),
                                      replacement = "\\1",
                                      x = accuracy),
                                  sub(pattern = paste0(".*?(\\d+\\.?\\d*) *", unit, ".*"),
                                      replacement = "\\1",
                                      x = accuracy)),
           accuracy_val  = as.numeric(accuracy_val))

}

#' Title
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
    select(sensor_header, unit, accuracy_val,
           operating_range_min, operating_range_max,
           roc_threshold, repeat_0s_max,
           local_range_min, local_range_max,
           error_info) %>%
    mutate(unit = ifelse(unit == "none", NA, unit))

  check_accuracy_for_grading(sensor_chars)

  return(sensor_chars)

}
