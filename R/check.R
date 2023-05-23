#' Title
#'
#' @inheritParams process_buoy
#'
#' @importFrom readxl excel_sheets
#'
#' @export
check_buoy_info <- function(info_fpath) {

  sheets_correct <- c("sensor_characteristics",
                      "sensor_maintenance",
                      "error_drift")

  sheets_current <- excel_sheets(info_fpath)

  if (any(duplicated(sheets_current))) {
    stop(paste("Issue with the buoy info file. File has sheets with the same",
               "name. Each sheet should have a unique name. Try again."),
         call. = FALSE)
  }

  if (any(!sheets_correct %in% sheets_current)) {
    stop(paste("Issue with the buoy info file. File must contain the following",
               "sheets: 'sensor_characteristics', 'sensor_maintenance',",
               "'error_drift'.",
               "\nEdit accordingly and try again."),
         call. = FALSE)
  }
}

#' Title
#'
#' @param data_params_units dataframe. Output from read_data_params_units()
#' @inheritParams edit_sensor_chars
#'
#' @export
check_params_units <- function(data_params_units, sensor_chars) {

  chars_params_units <- data.frame(param = sensor_chars$sensor_header,
                                   unit  = sensor_chars$unit)

  # We use identical and must take into account the order because some parameters
  # have the same name but could have different characteristics.
  if (!identical(data_params_units, chars_params_units)) {
    stop(paste("Issue with the sensor characteristics sheet. The parameters",
               "and units from the buoy data must match exactly (including",
               "order) with the sensor_characteristics spreadsheet.",
               "\nEdit accordingly and try again."),
         call. = FALSE)
  }
}

#' Title
#'
#' @inheritParams edit_error_drift
#'
#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#'
#' @export
check_error_drift <- function(error_drift) {

  # Checking that each set of pre and post values are valid --------------------
  unmatched_pre_post <- error_drift %>%
    filter(xor(is.na(pre_calibration), is.na(post_calibration)) |
             xor(is.na(pre_clean), is.na(post_clean)))

  if (nrow(unmatched_pre_post) != 0) {
    print.data.frame(unmatched_pre_post)
    stop(paste("Issue with the error drift sheet. If a parameter has one of the",
               "pre or post values filled in, the associated pre or post value",
               "is also expected to be filled in. The above rows do not fulfill",
               "this requirement. Edit accordingly and try again."),
         call. = FALSE)
  }

}

#' Title
#'
#' @inheritParams edit_sensor_chars
#'
#' @importFrom dplyr rowwise mutate filter select case_when
#' @importFrom magrittr "%>%"
#'
#' @export
check_sensor_chars <- function(sensor_chars) {

  # Checking if the accuracy
  bad_accuracy <- sensor_chars %>%
    rowwise() %>%
    mutate(accuracy_issues = ifelse(is.na(accuracy),
                                    NA,
                                    case_when(is.na(unit)
                                              ~ !grepl(paste0("+/- ?\\d+\\.?\\d*.*"),
                                                       accuracy),
                                              TRUE
                                              ~ !grepl(paste0("+/- ?\\d+\\.?\\d* *(", unit, ").*"),
                                                       accuracy)))) %>%
    filter(isTRUE(accuracy_issues)) %>%
    select(-accuracy_issues)

  if (nrow(bad_accuracy) != 0) {
    print.data.frame(bad_accuracy)
    stop(paste("Issue with the sensor characteristics sheet. Expected values in",
               "the accuracy column look like '+/- VALUE UNIT', where VALUE is",
               "some numeric value, and UNIT matches the unit listed in the",
               "unit column.",
               "\nEdit the sensor characteristics sheet accordingly and try again."))
  }

}

#' Title
#'
#' @inheritParams edit_sensor_chars
#' @inheritParams edit_error_drift
#'
#' @importFrom dplyr mutate filter select
#' @importFrom magrittr "%>%"
#' @importFrom fuzzyjoin regex_join
#'
#' @export
check_chars_error_match <- function(sensor_chars, error_drift) {

  error_missing <- regex_join(sensor_chars, error_drift,
                              by = c("sensor_header" = "sensor", "unit"),
                              mode = "right") %>%
    filter(is.na(sensor_header)) %>%
    mutate(unit = unit.y) %>%
    # Reorder to match the original error_drift file for better user understanding
    select(colnames(error_drift))

  if (nrow(error_missing != 0)) {
    print.data.frame(error_missing)
    stop(paste("Issue with the error drift sheet. The above row(s) could not",
               "be matched with a parameter and unit from the buoy data.",
               "\nNote that the matching process checks if the phrase found in",
               "the 'sensor' column of the error drift is in any of the",
               "parameter names from the buoy data sheet. That means that an",
               "error reading with sensor 'pH' would be matched with the",
               "'Deep_pH' and 'Shallow_pH' buoy parameters.",
               "\nEdit the error drift sheet and try again."),
         call. = FALSE)
  } else {
    matches_chars_error <- match_chars_error(sensor_chars, error_drift)

    print.data.frame(matches_chars_error %>%
                       select(sensor_chars_param = sensor_header,
                              error_drift_param = sensor,
                              unit) %>%
                       filter(!is.na(error_drift_param)))
    message(paste("Check the above table before proceeding. These are the matches",
                  "between the error drift sheet and the available parameters in",
                  "the buoy data identified by the tool.",
                  "\nAre these correct? If not, edit the sensor names in the",
                  "error drift sheet.",
                  "\nNote that the matching process checks if the phrase found in",
                  "the 'sensor' column of the error drift is in any of the",
                  "parameter names from the buoy data sheet. That means that an",
                  "error reading with sensor 'pH' would be matched with the",
                  "'Deep_pH' and 'Shallow_pH' buoy parameters."))
  }
}

#' Title
#'
#' @inheritParams edit_sensor_chars
#'
#' @importFrom dplyr select filter
#' @importFrom magrittr "%>%"
#'
#' @export
check_accuracy_for_grading <- function(sensor_chars) {

  missing_accuracy <- sensor_chars %>%
    # Select all rows with a pre calibration or clean value (check_error will
    # have ensured that if a pre value exists, a post value does too). These are
    # the rows for which a grade will be calculated and an accuracy is therefore
    # required
    filter((!is.na(pre_calibration) | !is.na(pre_clean)) & is.na(accuracy_val))

  if (nrow(missing_accuracy != 0)) {
    print.data.frame(missing_accuracy %>%
                       select(sensor_header:roc_threshold))
    stop(paste("Issue with sensor characteristics. Trying to calculate error and",
               "assign grades. For each of the parameters listed in the error",
               "drift sheet, the sensor characteristics sheet must list an",
               "accuracy, as the accuracy is used to determine the grade.",
               "\nIn the sensor characteristics sheet, add an accuracy to the",
               "parameters listed above and try again."),
         call. = FALSE)
  }

}
