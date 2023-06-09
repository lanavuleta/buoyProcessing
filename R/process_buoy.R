#' Title
#'
#' @param info_fpath file path. Path to the buoy info excel file
#' @param data_fpath file path. Path to the raw buoy data file
#' @param row_param_names numeric. Row at which parameter names are listed
#' @param row_data_start numeric. Row at which buoy data begins
#' @param row_units numeric. Row at which parameter units are listed
#' @param datetime_format string. Datetime format as would be used by as.Date()
#' @param missing_vals string vector. Cell contents that indicate a missing value
#' @param timezone string. Timezone of the data used. See options with OlsonNames()
#' @param combine_flags boolean. TRUE results in an output with one flag column
#'    per sensor that contains all the flags assigned. FALSE results in each
#'    data column getting 6 associated flag columns (From flag_x to flag_4)
#'
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
process_buoy <- function(info_fpath = "data/input/example_buoy_input.xlsx",
                         data_fpath = "data/input/BP_SuperBuoy_Data2022.csv",
                         row_param_names = 2,
                         row_data_start = 5,
                         row_units = 3,
                         datetime_format = "%m-%d-%Y %H:%M:%S",
                         missing_vals = c("No Data", -100000, ""),
                         timezone = "America/Regina",
                         combine_flags = TRUE) {

  # Account for cells that are entirely empty (this case cannot be input by the
  # GUI)
  missing_vals <- c(missing_vals, "", NA)

  # Read in info from buoy_info ------------------------------------------------
  # Ensure sheets are done correctly such that process can continue as planned
  check_buoy_info(info_fpath)

  sensor_maint <- read_sensor_maint(info_fpath, timezone)
  error_drift  <- read_error_drift(info_fpath, sensor_maint)
  sensor_chars <- read_sensor_chars(info_fpath) %>%
    combine_chars_with_error(error_drift)

  # Read in data from buoy_data ------------------------------------------------
  data_params_units <- read_data_params_units(data_fpath,
                                              row_param_names,
                                              row_units)

  # Because every parameter in parameter_data should have a matching row in
  # sensor_chars to be able to assign flags
  check_params_units(data_params_units, sensor_chars)

  data <- read_data(data_fpath, row_param_names, row_data_start, data_params_units) %>%
    format_datetime(datetime_format, timezone)

  # Perform steps 2 (flag) and 3 (error) ---------------------------------------

  data <- data %>%
    flag_and_error(sensor_chars, sensor_maint, combine_flags, missing_vals)# %>%
    #handle_dup_names()

}

