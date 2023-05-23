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
    mutate(start_datetime = ISOdate(year(sensor_maint$start_date),
                                    month(sensor_maint$start_date),
                                    day(sensor_maint$start_date),
                                    # Done like this because excel times are
                                    # read in with an undesired dummy date
                                    hour(sensor_maint$start_time),
                                    minute(sensor_maint$start_time),
                                    tz = timezone)
           # Will flag X an additional 10 min before the
           # end of the maintenance to allow parameters
           # to settle
           - minutes(10),
           end_datetime   = ISOdate(year(sensor_maint$end_date),
                                    month(sensor_maint$end_date),
                                    day(sensor_maint$end_date),
                                    # Done like this because excel times are
                                    # read in with an undesired dummy date
                                    hour(sensor_maint$end_time),
                                    minute(sensor_maint$end_time),
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
#'
#' @importFrom dplyr mutate select rowwise
#' @importFrom tidyr nest
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
edit_error_drift <- function(error_drift) {

  check_error_drift(error_drift)

  error_drift <- error_drift %>%
    mutate(error_calib = abs(pre_calibration - post_calibration),
           error_clean = abs(pre_clean - post_clean)) %>%
    # Need to do rowwise because sum returns the sum of ALL values present in
    # its arguments, and we must use sum to remove NAs
    rowwise() %>%
    mutate(error = case_when(is.na(error_calib) & is.na(error_clean) ~ NA,
                             TRUE ~ sum(error_calib, error_clean, na.rm = TRUE))) %>%
    select(-c(pre_calibration, post_calibration, pre_clean, post_clean,
              error_calib, error_clean)) %>%
    # There might be numerous calibrations over a season for each sensor
    nest(data = c(date, error))

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
    select(colnames(sensor_chars),
           pre_calibration, post_calibration,
           pre_clean, post_clean) %>%
    mutate(unit = ifelse(unit == "none", NA, unit))

  check_accuracy_for_grading(sensor_chars)

  return(sensor_chars)

}
