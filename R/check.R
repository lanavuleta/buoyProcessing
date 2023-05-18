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
#' @param sensor_chars dataframe. Output from read_sensor_chars()
#'
#' @return dataframe
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
#' @param missing_chunks dataframe. Edited run length encoding created in do_flag_1()
#' @inheritParams do_flag_1
#'
#' @importFrom dplyr lead lag
#'
#' @return dataframe
#' @export
check_missing_chunks <- function(missing_chunks, time_small, time_large) {

  for (i in 1:nrow(missing_chunks)) {
    # Check for values missing for short period of time
    if (missing_chunks$values[i] == "M" & missing_chunks$lengths[i] <= time_small) {
      missing_chunks$flag_1[i] <- "B1"
    }
    # Check for few values recorded in a large block of missing values
    if (missing_chunks$values[i] == "" & missing_chunks$lengths[i] <= time_small &
        # Using all() to account for start or end rows. If a short block of values
        # is followed/preceded by a large chunk of missing values and is at the
        # start/end (respectively) of the dataset, it still counts as a few values
        # recorded in a large block of missing
        all((lead(missing_chunks)$lengths[i] >= time_large &
             lead(missing_chunks)$values[i] == "M"),
            (lag(missing_chunks)$lengths[i] >= time_large &
             lag(missing_chunks)$values[i] == "M"),
            na.rm = TRUE)) {
      missing_chunks$flag_1[i] <- "B1"
    }
  }

  return(missing_chunks)

}

#' Title
#'
#' @param error_drift dataframe. Output from read_error_drift()
#' @inheritParams check_params_units
#'
#' @importFrom dplyr mutate select filter
#' @importFrom magrittr "%>%"
#' @importFrom fuzzyjoin regex_join
#'
#' @return printed statement if fail OR dataframe if success
#' @export
check_error <- function(error_drift, data_params_units) {

  data_params_units <- data_params_units %>%
    mutate(unit = ifelse(is.na(unit), "none", unit))

  error_drift <- error_drift %>%
    mutate(unit = ifelse(is.na(unit), "none", unit))

  error_matches <- regex_join(data_params_units, error_drift,
                              by = c("param" = "sensor", "unit"),
                              mode = "right") %>%
    # unit from data_params_units not needed. If there was a match, value is in
    # unit.y, and if there was not a match, this is indicated by an empty param
    # column
    mutate(unit = unit.y) %>%
    select(-c(unit.x, unit.y))

  error_missing <- error_matches %>%
    filter(is.na(param)) %>%
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
    print.data.frame(select(error_matches, colnames(error_drift)))
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

    return(error_matches)
  }

}
