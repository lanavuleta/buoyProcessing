#' Title
#'
#' @inheritParams process_buoy
#' @inheritParams combine_chars_with_error
#'
#' @importFrom stringr str_remove
#' @importFrom magrittr "%>%"
#' @importFrom utils read.csv
#' @importFrom dplyr slice
#'
#' @return dataframe
#' @export
read_data_params_units <- function(data_fpath, row_param_names, row_units, sensor_chars) {

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
    slice(-1)

  if (nrow(data_params_units) != nrow(sensor_chars)) {
    stop(paste("Issue with the input buoy data. The tool can only accept data",
               "that has one single column for the",
               "date and time and one column for each of the sensors listed in the",
               "Sensor Characteristics sheet."),
         call. = FALSE)
  }

  return(data_params_units)

}

#' Title
#'
#' @inheritParams process_buoy
#' @inheritParams check_params_units
#' @inheritParams combine_chars_with_error
#'
#' @importFrom dplyr filter row_number
#' @importFrom magrittr "%>%"
#' @importFrom readr read_csv
#'
#' @return dataframe
#' @export
read_data <- function(data_fpath, row_param_names, row_data_start, data_params_units, sensor_chars) {

  data <- read_csv(data_fpath,
                   # Do row_xx-1 because the read in process turns the first row
                   # into column header
                   skip = row_param_names-1,
                   # Read all columns as character to keep any missing data strings
                   col_types = strrep("c", nrow(data_params_units) + 1)) %>%
    # Need to calculate the new start of the data considering the rows that have
    # already been skipped
    filter(row_number() >= row_data_start - row_param_names)

  if (sensor_chars$sensor_header[1] != colnames(data)[2]) {
    stop(paste("Issue with the input buoy data. The tool can only accept data",
               "that has one single column for the",
               "date and time and one column for each of the sensors listed in the",
               "Sensor Characteristics sheet.\nThe second column of the data did",
               "not align with the first sensor input in the Sensor Characteristics sheet."),
         call. = FALSE)
  }

  # At this point column names will likely differ from those in the original file
  # because R will add digits to differentiate between identical columns and
  # replace any spaces with ".". This is fine because subsequent use of buoy_info
  # will use the column position rather than the column name

  return(data)
}

#' Title
#'
#' @inheritParams process_buoy
#'
#' @importFrom readxl read_xlsx
#'
#' @return dataframe
#' @export
read_sensor_maint <- function(info_fpath) {

  sensor_maint <- read_xlsx(info_fpath,
                            sheet = "sensor_maintenance")

  cols_correct <- c("start_datetime",	"end_datetime",	"flag")

  if (any(!cols_correct %in% colnames(sensor_maint))) {
    stop(paste("Issue with the sensor maintenance sheet. Required column",
               "names are:", paste(cols_correct, collapse = ", "), ". One or",
               "more required column is missing.\nEdit accordingly and try again."),
         call. = FALSE)
  }

  check_input_class(sensor_maint,
                    "Sensor Maintenance",
                    c("POSIXct", "POSIXct", "character"),
                    cols_correct)
  check_flags(sensor_maint$flag)

  sensor_maint <- edit_sensor_maint(sensor_maint)

  return(sensor_maint)

}

#' Title
#'
#' @inheritParams process_buoy
#' @param sensor_maint dataframe. Output from read_sensor_maint()
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate
#'
#' @return dataframe
#' @export
read_error_drift <- function(info_fpath, sensor_maint) {

  error_drift <- read_xlsx(info_fpath,
                           sheet = "error_drift") %>%
    # R will read this in as a datetime instead of as a Date
    mutate(date = as.Date(date))

  cols_correct <- c("sensor_header", "unit", "date",
                    "pre_calibration", "post_calibration",
                    "pre_clean", "post_clean")

  if (any(!cols_correct %in% colnames(error_drift))) {
    stop(paste("Issue with the sensor error drift sheet. Required column",
               "names are:", paste(cols_correct, collapse = ", "), ". One or",
               "more required column is missing.\nEdit accordingly and try again."),
         call. = FALSE)
  }

  check_input_class(error_drift,
                    "Error Drift",
                    c("character", "character", "Date",
                      "numeric", "numeric", "numeric", "numeric"),
                    cols_correct)

  error_drift <- edit_error_drift(error_drift, sensor_maint)

  return(error_drift)
}

#' Title
#'
#' @inheritParams process_buoy
#' @param error_drift dataframe. Output from read_error_drift()
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate select
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
read_sensor_chars <- function(info_fpath, error_drift) {

  sensor_chars <- read_xlsx(info_fpath,
                            sheet = "sensor_characteristics")

  cols_correct <- c("sensor_header", "unit", "accuracy_used", "accuracy_factory_specs",
                    "operating_range_min", "operating_range_max",
                    "roc_threshold", "repeat_0s_max",
                    "local_range_max", "local_range_min")

  if (any(!cols_correct %in% colnames(sensor_chars))) {
    stop(paste("Issue with the sensor characteristics sheet. Required column",
               "names are:", paste(cols_correct, collapse = ", "), ". One or",
               "more required column is missing.\nEdit accordingly and try again."),
         call. = FALSE)
  }

  sensor_chars <- sensor_chars %>%
    mutate(accuracy = accuracy_used) %>%
    select(-c(accuracy_used, accuracy_factory_specs))

  check_input_class(sensor_chars,
                    "Sensor Characteristics",
                    c("character", "character", "character", "character",
                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"),
                    cols_correct)

  sensor_chars <- edit_sensor_chars(sensor_chars, error_drift)

  return(sensor_chars)

}

#' Title
#'
#' @param data_qcd_fpath string. Path to QCd data csv
#'
#' @importFrom readr read_csv
#' @importFrom dplyr distinct select contains mutate_all
#'
#' @return dataframe, as was output by combine_buoy()
#' @export
read_data_qcd <- function(data_qcd_fpath) {
  data_qcd <- read_csv(data_qcd_fpath, name_repair = "minimal")

  flag_problems <- data_qcd %>%
    as.data.frame() %>%
    # In case there are any duplicate names. select() requires unique colnames
    distinct() %>%
    select(contains("_Flag")) %>%
    mutate_all(~strsplit(., " ?, ?"))

  flag_problems <- unique(unlist(flag_problems))

  if(any(!flag_problems %in% c("1", "2", "3", "4", "X1", "X2", "X3", "X", "M", NA))) {
    stop(paste("Issue with the QC'd reuploaded data.\nTool expects the following",
               "flags, assigned as described in the 'Help' section, and separated",
               "by a comma in cases where multiple flags have been assigned",
               "to one measurement:",
               "\n\t1\n\t2\n\t3\n\t4\n\tX1\n\tX1\n\tX3\n\tX\n\tM",
               "Ensure the data adheres to these rules and try again."
               ),
         call. = FALSE)
  }

  return(data_qcd)
}

#' Title
#'
#' @param data_qcd dataframe. Output from read_data_qcd()
#'
#' @importFrom purrr map2
#'
#' @return list of dataframes, as was output by process_buoy()
#' @export
split_data_qcd <- function(data_qcd) {
  start_cols <- NULL
  end_cols <- NULL

  i <- 2

  while (i <= ncol(data_qcd)) {
    start_cols <- c(start_cols, i)

    parameter <- colnames(data_qcd)[i]

    # We know that the column at i+1 is the Flag col, but don't know if the column
    # at i+2 is the Error col or the next parameter
    if (grepl(paste0(parameter, "_Error"), colnames(data_qcd)[i+2])) {
      end_col <- i+2
    } else {
      end_col <- i+1
    }

    end_cols <- c(end_cols, end_col)

    i <- end_col + 1
  }

  data_qcd_pois <- map2(start_cols, end_cols, split_data, data_qcd)
}

#' Title
#'
#' @param start_col numeric. Column at which poi data begins
#' @param end_col numeric. Column at which poi data ends
#' @param data_qcd dataframe. QCd data
#'
#' @importFrom dplyr select all_of
#'
#' @return dataframe. The columns of interest for a specified parameter
#' @export
split_data <- function(start_col, end_col, data_qcd) {
  # If the sensor is a duplicate name (ex there are two sensors named
  # "Underwater PAR"), R will add a differentiating character to the end of the
  # column when reading in the csv. This must be removed

  data_poi <- select(data_qcd, all_of(c(1, start_col:end_col)))

  #if (grepl("_Flag.*", colnames(data_poi)[3])) {
  #  # The characters used by R to differentiate between duplicates
  #  differentiator <- sub("_Flag", "", str_extract(colnames(data_poi)[3],
  #                                                 "_Flag(.*)"))
  #}
}
