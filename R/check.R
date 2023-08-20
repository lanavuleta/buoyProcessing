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
#' @importFrom dplyr filter summarise group_by n
#' @importFrom magrittr "%>%"
#'
#' @export
check_error_drift <- function(error_drift) {

  # Checking that each set of pre and post values are valid --------------------
  unmatched_pre_post <- error_drift %>%
    filter(xor(is.na(pre_calibration), is.na(post_calibration)) |
             xor(is.na(pre_clean), is.na(post_clean)))

  if (nrow(unmatched_pre_post) != 0) {
    stop(paste("Issue with the error drift sheet. If a parameter has one of the",
               "pre or post values filled in, the associated pre or post value",
               "is also expected to be filled in.\nEdit accordingly and try again."),
         call. = FALSE)
  }

  # Ensure there's just one row for each sensor checked for each date ----------
  error_drift_dups <- error_drift %>%
    group_by(sensor_header, unit, date) %>%
    summarise(n = n()) %>%
    filter(n != 1)

  if (nrow(error_drift_dups) != 0) {
    stop(paste("Issue with the error drift sheet. Each sensor/date combination",
               "should only exist once.\nEdit accordingly and try again."),
         call. = FALSE)
  }

}

#' Title
#'
#' @param accuracy string. Accuracy from sensor_chars row
#' @param unit string. Characters remaining after the "+/- x.xx" is extracted
#'
#' @return boolean. TRUE if accuracy input is incorrect, FALSE otherwise
#' @export
get_accuracy_units <- function(accuracy, unit) {
  accuracy_unit <- str_match(accuracy, "\\+/-\\s*\\d+\\.?\\d*(.*)")[,2]

  accuracy_issues <- FALSE

  # Make paste0 easier with an NA unit
  if (is.na(unit)) {
    unit <- ""
  }

  if (!grepl(paste0("^\\s*", unit, "\\s*$"), accuracy_unit) &
      !grepl("^\\s*%\\s*$", accuracy_unit)) {
    accuracy_issues <- TRUE
  }

  return(accuracy_issues)
}

#' Title
#'
#' @inheritParams edit_sensor_chars
#' @inheritParams edit_error_drift
#'
#' @importFrom dplyr mutate filter select left_join
#' @importFrom magrittr "%>%"
#' @importFrom fuzzyjoin regex_join
#'
#' @export
check_chars_error_match <- function(sensor_chars, error_drift) {

  error_missing <- left_join(error_drift, sensor_chars,
                             by = c("sensor_header", "unit"),
                             keep = TRUE) %>%
    # No sensor_header from sensor_chars - no match!
    filter(is.na(sensor_header.y)) %>%
    mutate(unit = unit.x,
           sensor_header = sensor_header.x) %>%
    # Reorder to match the original error_drift file for better user understanding
    select(sensor_header, unit) %>%
    unique()

  if (nrow(error_missing) != 0) {

    m <- ""

    for (i in 1:nrow(error_missing)) {
      row <- paste(error_missing[i,]$sensor_header, "/", error_missing[i,]$unit, collapse = ", ")
      m <- paste(m, "\n", row)
    }

    stop(paste("Issue with the Error Drift sheet. The following parameter/unit combinations",
               "from the Error Drift sheet",
               "were not identified in the Sensor Characteristics sheet:",
               "\n", m,
               "\nNote that the matching process checks if the phrase found in",
               "the 'sensor' column of the error drift is in any of the",
               "parameter names from the buoy data sheet. That means that an",
               "error reading with sensor 'pH' would be matched with the",
               "'Deep_pH' and 'Shallow_pH' buoy parameters.",
               "\nEdit the error drift sheet and try again."),
         call. = FALSE)
  }
}

#' Title
#'
#' @param error_drift dataframe. Output from match_error_maint()
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#'
#' @return print statement if error
#' @export
check_error_maint_match <- function(error_drift) {

  missing_date <- error_drift %>%
    filter(is.na(end_datetime)) %>%
    select(date) %>%
    unique()

  if (nrow(missing_date) != 0) {
    stop(paste("Issue with the error drift sheet. The following date(s)",
               "could not be matched with a date in the sensor maintenance",
               "sheet:",
               "\n\t", paste(missing_date$date, collapse = ", "),
               "\nEdit the dates accordingly and try again."),
         call. = FALSE)
  }

}

#' Title
#'
#' @param input dataframe. The input whose column classes are to be checked
#' @param input_name string. The name of the input being checked
#' @param classes_correct character list. The correct classes for the columns of interest
#' @param cols_correct character list. The names of the columns to check
#'
#' @importFrom dplyr filter left_join mutate
#' @importFrom tidyr pivot_longer everything
#'
#' @return character list. The columns that were entered incorrectly
#' @export
check_input_class <- function(input, input_name, classes_correct, cols_correct) {

  classes_correct <- data.frame(colname = cols_correct, type_correct = classes_correct)

  # Use as.list because datetime have 2 types (POSIXct, POSIXt) and any datetimes
  # alter the shape of the dataframe. as.list() ensures that the structure is
  # consistent with or without datetimes
  classes_input <- as.data.frame(as.list(sapply(input, class)))[1,] %>%
    pivot_longer(cols = everything(), names_to = "colname", values_to = "type_actual") %>%
    filter(colname %in% classes_correct$colname)

  classes_issue <- left_join(classes_correct, classes_input, by = "colname") %>%
    filter(type_correct != type_actual) %>%
    mutate(type_correct = ifelse(type_correct == "POSIXct", "Date", type_correct))

  if (nrow(classes_issue) != 0) {

    m <- ""

    for (i in 1:nrow(classes_issue)) {
      row <- sprintf("\tFor column %s, the expected type is %s and the actual type is %s\n",
                     classes_issue[i,1], classes_issue[i,2], classes_issue[i,3])
      m <- paste(m, row, collapse = "")
    }

    stop(paste("Issue with", input_name, "\nThe values input in the following",
               "columns were not the expected type:\n",
               m,
               "\nNote: check the column type in the input Excel spreadsheet",
               "and ensure it lines up with what is described in the Help - Buoy Info files.\nNote: a",
               "column that should be numeric can only contain digits and no",
               "special characters or letters, such as '<' or any text.",
               "\nNote: a column that should be a date or a datetime should follow",
               "a consistent format and be easily identifiable as a date or datetime.",
               "You can follow the following format for a date: 'YYYY-mm-dd', and",
               "the following format for a datetime: 'YYYY-mm-dd HH:MM:SS'"),
         paste("\nEdit accordingly and try again."),
         call. = FALSE)
  }

}

# Checking column input validity -----------------------------------------------

#' Title
#'
#' @param flags character string. The flags set by the user in the sensor_maint sheet
#'
#' @return error message if flags were not properly assigned
check_flags <- function(flags) {

  flags_expected <- c("X1", "X2", "X3")

  if (any(!flags %in% flags_expected)) {
    stop(paste("Issue with the flags in Sensor Maintenance", "\nThe acceptable",
               "flags are: X1, X2, X3.", "\nEdit accordingly and try again."),
         call. = FALSE)
  }

}

check_accuracy <- function(accuracy, unit) {}
