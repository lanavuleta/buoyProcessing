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
               "name. Each sheet should have a unique name. Try again."))
  }

  if (any(!sheets_correct %in% sheets_current)) {
    stop(paste("Issue with the buoy info file. File must contain the following",
               "sheets: 'sensor_characteristics', 'sensor_maintenance',",
               "'error_drift'.",
               "\nEdit accordingly and try again."))
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
