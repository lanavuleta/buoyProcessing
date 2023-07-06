#' Title
#'
#' @inheritParams process_buoy
#'
#' @importFrom stringr str_remove
#' @importFrom magrittr "%>%"
#' @importFrom utils read.csv
#'
#' @return dataframe
#' @export
read_data_params_units <- function(data_fpath, row_param_names, row_units) {

  data <- read.csv(data_fpath)

  # Do row_xx-1 because the read in process turns the first row into column header
  data_params_units  <- data.frame(param = c(t(data[row_param_names-1,])),
                                   unit  = c(t(data[row_units-1,]))) %>%
    # When columns are read in that have the same name, .x is added to the end
    # of the column name to allow for differentiation
    mutate(param = str_remove(param, "\\.\\d$"),
           # Some parameters might have a random space at the end of the name
           # which will not be read in from the sensor_chars sheet
           param = str_remove(param, " $"),
           unit = ifelse(unit == "", NA, unit)) %>%
    filter(!grepl("date|time", param, ignore.case = T))

  return(data_params_units)

}

#' Title
#'
#' @inheritParams process_buoy
#' @inheritParams check_params_units
#'
#' @importFrom dplyr filter row_number
#' @importFrom magrittr "%>%"
#' @importFrom readr read_csv
#'
#' @return dataframe
#' @export
read_data <- function(data_fpath, row_param_names, row_data_start, data_params_units) {

  data <- read_csv(data_fpath,
                   # Do row_xx-1 because the read in process turns the first row
                   # into column header
                   skip = row_param_names-1,
                   # Read all columns as character to keep any missing data strings
                   col_types = strrep("c", nrow(data_params_units) + 1)) %>%
    # Need to calculate the new start of the data considering the rows that have
    # already been skipped
    filter(row_number() >= row_data_start - row_param_names)

  # At this point column names will likely differ from those in the original file
  # because R will add digits to differentiate between identical columns and
  # replace any spaces with ".". This is fine because subsequent use of buoy_info
  # will use the column position rather than the column name

}

#' Title
#'
#' @inheritParams process_buoy
#'
#' @importFrom readxl read_xlsx
#'
#' @return dataframe
#' @export
read_sensor_maint <- function(info_fpath, timezone) {

  sensor_maint <- read_xlsx(info_fpath,
                            sheet = "sensor_maintenance")

  cols_correct <- c("start_datetime",	"end_datetime",	"flag")

  if (any(!cols_correct %in% colnames(sensor_maint))) {
    stop(paste("Issue with the sensor maintenance sheet. Required column",
               "names are:", paste(cols_correct, collapse = ", "), ". One or",
               "more required column is missing.\nEdit accordingly and try again."))
  }

  sensor_maint <- edit_sensor_maint(sensor_maint, timezone)

  return(sensor_maint)

}

#' Title
#'
#' @inheritParams process_buoy
#' @param sensor_maint dataframe. Output from read_sensor_maint()
#'
#' @importFrom readxl read_xlsx
#'
#' @return dataframe
#' @export
read_error_drift <- function(info_fpath, sensor_maint) {

  error_drift <- read_xlsx(info_fpath,
                           sheet = "error_drift")

  cols_correct <- c("sensor", "calibration_type", "unit", "date",
                    "sensor_model",	"depth", "pre_calibration",
                    "post_calibration", "pre_clean", "post_clean")

  if (any(!cols_correct %in% colnames(error_drift))) {
    stop(paste("Issue with the sensor characteristics sheet. Required column",
               "names are:", paste(cols_correct, collapse = ", "), ". One or",
               "more required column is missing.\nEdit accordingly and try again."))
  }

  error_drift <- edit_error_drift(error_drift, sensor_maint)

  return(error_drift)
}

#' Title
#'
#' @inheritParams process_buoy
#'
#' @importFrom readxl read_xlsx
#'
#' @return dataframe
#' @export
read_sensor_chars <- function(info_fpath) {

  sensor_chars <- read_xlsx(info_fpath,
                            sheet = "sensor_characteristics")

  cols_correct <- c("sensor_header", "unit", "accuracy",
                    "operating_range_min", "operating_range_max",
                    "roc_threshold", "repeat_0s_max",
                    "local_range_max", "local_range_min")

  if (any(!cols_correct %in% colnames(sensor_chars))) {
    stop(paste("Issue with the sensor characteristics sheet. Required column",
               "names are:", paste(cols_correct, collapse = ", "), ". One or",
               "more required column is missing.\nEdit accordingly and try again."))
  }

  sensor_chars <- edit_sensor_chars(sensor_chars)

  return(sensor_chars)

}
