#' Title
#'
#' @inheritParams format_datetime
#' @inheritParams check_params_units
#' @param sensor_maint Output from read_sensor_maint()
#' @inheritParams process_buoy
#'
#' @importFrom purrr map map2 map_dfc
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select all_of
#'
#' @return dataframe
#' @export
flag_and_error <- function(data, sensor_chars, sensor_maint, combine_flags,
                           missing_vals) {

  time_small <- get_flag_intervals(data)[1]
  time_large <- get_flag_intervals(data)[2]

  data <- data %>%
    do_flag_x(sensor_maint)

  cols <- seq(2,ncol(data)-1)

  # pois = ParameterS Of Interest
  data_pois <- cols %>% map(function(x) select(data, all_of(c(1, x))))

  sensor_chars_listed <- sensor_chars %>%
    select(-unit) %>% #unit not needed for flagging and error
    rename(parameter = sensor_header) %>%
    split(seq(nrow(.)))

  inputs <- map2(data_pois, sensor_chars_listed, prep_input)

  data_pois <- map_dfc(inputs,
                       handle_poi,
                       time_small, time_large, combine_flags, missing_vals)
  #append(data_pois, as.list(sc_t))

  data <- data %>%
    select(datetime, flag_x) %>%
    cbind(data_pois) %>%
    do_flag_final() %>%
    select(-flag_x)

}

#' Title
#'
#' @param data_poi dataframe. Datetime column and parameter of interest column
#' @param sensor_chars_poi list. Sensor chars of interest associated with the
#'    parameter of interest
#'
#' @importFrom tidyr nest
#'
#' @return list
#' @export
prep_input <- function(data_poi, sensor_chars_poi) {
  input <- append(nest(data_poi), sensor_chars_poi)

  # R gives the default name "data" which is confusing with the variable
  # data in flag_and_error()
  names(input)[1] <- "data_poi"

  return(input)
}

#' Title
#' @param input list. data_poi and all the sensor_chars of interest
#' @param time_small numeric. Number of rows that indicate a small time step
#' @param time_large numeric. Number of rows that indicate a large time step
#' @inheritParams flag_and_error
#'
#' @return dataframe
#' @export
handle_poi <- function(input,
                       time_small, time_large, combine_flags, missing_vals) {

  data_poi <- input$data_poi[[1]] %>% # data is stored in a list within the input list
    flag_poi(input$operating_range_min, input$operating_range_max,
             input$roc_threshold,
             input$local_range_min, input$local_range_max,
             time_small, time_large,
             input$repeat_0s_max,
             input$parameter,
             combine_flags, missing_vals) %>%
    error_poi(input$accuracy_val, input$error_info[[1]],
              input$parameter) %>%
    select(-(datetime))

  # Use name from sensor_chars because if two columns from buoy data have the
  # same name, R will change the column name
  colnames(data_poi)[1] <- paste(input$parameter)

  return(data_poi)

}

#' Title
#'
#' @inheritParams handle_poi
#' @inheritParams prep_input
#' @param operating_range_min numeric. Operating range minimum
#' @param operating_range_max numeric. Operating range maximum
#' @param roc_threshold numeric. Maximum allowed rate of change
#' @param local_range_min numeric. Operating range minimum
#' @param local_range_max numeric. Operating range maximum
#' @param repeat_0s_max numeric. Max allowable number of repeated 0s
#' @param parameter string. Parameter name
#'
#' @importFrom dplyr mutate mutate_at case_when
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
flag_poi <- function(data_poi,
                     operating_range_min, operating_range_max,
                     roc_threshold,
                     local_range_min, local_range_max,
                     time_small, time_large,
                     repeat_0s_max,
                     parameter,
                     combine_flags = FALSE, missing_vals) {

  data_poi <- data_poi %>%
    mutate(flag_m = case_when(.[[2]] %in% missing_vals ~ "M",
                              TRUE ~ "")) %>%
    mutate_at(2, as.numeric) %>%

    do_flag_1(time_small, time_large, repeat_0s_max) %>%
    do_flag_2(operating_range_min, operating_range_max) %>%
    do_flag_3(local_range_min, local_range_max) %>%
    do_flag_4(roc_threshold, time_small) %>%
    do_flag_m_to_4(combine_flags) %>%
    select(datetime, 2, flag)

  # Use name from sensor_chars because if two columns from buoy data have the
  # same name, R will change the column name
  colnames(data_poi)[3] <- paste(parameter, "Flag", sep = "_")

  return(data_poi)

}

#' Title
#'
#' @inheritParams flag_poi
#' @param accuracy_val numeric. Instrument accuracy
#' @param error_info dataframe
#'
#' @importFrom dplyr mutate case_when select
#'
#' @return dataframe
#' @export
error_poi <- function(data_poi, accuracy_val, error_info, parameter) {

  if (!is.null(error_info)) {

    error_info <- calculate_grade(accuracy_val, error_info)

    data_poi <- mutate(data_poi, error = NA_character_)

    for (i in nrow(error_info):1) {
      data_poi <- data_poi %>%
        mutate(error = case_when(datetime <= error_info$end_datetime[i]
                                 ~ error_info$grade[i],
                                 TRUE
                                 ~ error))

    }

    # Use name from sensor_chars because if two columns from buoy data have the
    # same name, R will change the column name
    colnames(data_poi)[4] <- paste(parameter, "Error", sep = "_")

  }

  return(data_poi)

}


