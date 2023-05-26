#' Title
#'
#' @inheritParams format_datetime
#' @inheritParams check_params_units
#' @param sensor_maint Output from read_sensor_maint()
#'
#' @importFrom purrr map pmap_dfc
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select all_of
#'
#' @return dataframe
#' @export
flag_and_error <- function(data, sensor_chars, sensor_maint) {

  flag_intervals <- get_flag_intervals(data)
  time_small <- flag_intervals[[which(names(flag_intervals) == "time_small")]]
  time_large <- flag_intervals[[which(names(flag_intervals) == "time_large")]]

  data <- data %>%
    do_flag_x(sensor_maint)

  cols <- seq(2,ncol(data)-1)

  # pois = ParameterS Of Interest
  data_pois <- cols %>% map(function(x) select(data, all_of(c(1, x))))

  operating_range_mins <- sensor_chars$operating_range_min
  operating_range_maxs <- sensor_chars$operating_range_max

  roc_thresholds <- sensor_chars$roc_threshold

  accuracies  <- sensor_chars$accuracy_val
  error_infos <- sensor_chars$error_info

  data_pois <- pmap_dfc(list(data_pois,
                             operating_range_mins, operating_range_maxs,
                             roc_thresholds,
                             accuracies, error_infos),
                        handle_poi,
                        time_small, time_large)

  data <- data %>%
    select(datetime, flag_x) %>%
    cbind(data_pois) %>%
    do_flag_final() %>%
    select(-flag_x)

}

#' Title
#'
#' @param data_poi dataframe. Datetime column and parameter of interest column
#' @param operating_range_min numeric. Operating range minimum
#' @param operating_range_max numeric. Operating range maximum
#' @param roc_threshold numeric. Maximum allowed rate of change
#' @param accuracy numeric. Instrument accuracy
#' @param error_info dataframe
#' @param time_small numeric. Number of rows that indicate a small time step
#' @param time_large numeric. Number of rows that indicate a large time step
#'
#' @return dataframe
#' @export
handle_poi <- function(data_poi,
                       operating_range_min, operating_range_max,
                       roc_threshold,
                       accuracy, error_info,
                       time_small, time_large) {

  data_poi <- data_poi %>%
    flag_poi(operating_range_min, operating_range_max,
             roc_threshold,
             time_small, time_large) %>%
    error_poi(accuracy, error_info)

}

#' Title
#'
#' @inheritParams handle_poi
#'
#' @importFrom dplyr mutate mutate_at case_when
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
flag_poi <- function(data_poi,
                     operating_range_min, operating_range_max,
                     roc_threshold,
                     time_small, time_large) {

  parameter <- colnames(data_poi)[2]

  data_poi <- data_poi %>%
    mutate(flag_m = case_when(.[[2]] %in% missing_vals ~ "M",
                              TRUE ~ "")) %>%
    mutate_at(2, as.numeric) %>%

    do_flag_1(time_small, time_large) %>%
    do_flag_2(operating_range_min, operating_range_max) %>%
    do_flag_3() %>%
    do_flag_4(roc_threshold) %>%
    do_flag_m_to_4() %>%
    select(datetime, 2, flag)

  colnames(data_poi)[3] <- paste(colnames(data_poi)[2], "Flag", sep = "_")

  return(data_poi)

}

#' Title
#'
#' @inheritParams handle_poi
#' @param data_poi dataframe. Output from flag_poi()
#'
#' @importFrom dplyr mutate case_when select
#'
#' @return dataframe
#' @export
error_poi <- function(data_poi, accuracy, error_info) {

  if (!is.null(error_info)) {

    error_info <- calculate_grade(accuracy, error_info)

    data_poi <- mutate(data_poi, error = "")

    for (i in nrow(error_info):1) {
      data_poi <- data_poi %>%
        mutate(error = case_when(datetime <= error_info$end_datetime[i]
                                 ~ error_info$grade[i],
                                 TRUE
                                 ~ error))

    }

    colnames(data_poi)[4] <- paste(colnames(data_poi)[2], "Error", sep = "_")

  }

  data_poi <- select(data_poi, -(datetime))

  return(data_poi)

}


