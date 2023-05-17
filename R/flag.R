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
flag <- function(data, sensor_chars, sensor_maint) {

  flag_intervals <- get_flag_intervals(data)

  data <- data %>%
    do_flag_x(sensor_maint)

  cols <- seq(2,ncol(data)-1)

  # pois = ParameterS Of Interest
  data_pois <- cols %>% map(function(x) select(data, all_of(c(1, x))))

  operating_range_mins <- sensor_chars$operating_range_min
  operating_range_maxs <- sensor_chars$operating_range_max

  roc_thresholds <- sensor_chars$roc_threshold

  data_pois <- pmap_dfc(list(data_pois,
                             operating_range_mins, operating_range_maxs,
                             roc_thresholds),
                        flag_poi,
                        sensor_chars, sensor_maint,
                        time_small = flag_intervals[[which(names(flag_intervals) == "time_small")]],
                        time_large = flag_intervals[[which(names(flag_intervals) == "time_large")]])

  data <- data %>%
    select(datetime, flag_x) %>%
    cbind(data_pois) %>%
    do_flag_final() %>%
    select(-flag_x)

}

#' Title
#'
#' @inheritParams flag
#'
#' @importFrom sqldf sqldf
#' @importFrom dplyr select rename
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
do_flag_x <- function(data, sensor_maint) {

  data <- sqldf("SELECT * FROM data
                LEFT JOIN sensor_maint
                ON data.datetime >= sensor_maint.start_datetime AND
                  data.datetime <= sensor_maint.end_datetime") %>%
    select(-c(start_datetime, end_datetime)) %>%
    rename(flag_x = flag)

}

#' Title
#'
#' @param data_poi dataframe. Buoy data's datetime column, parameter of interest
#'  and created flag columns as their respective do_flag functions are run
#' @param operating_range_min numeric. Operating range minimum
#' @param operating_range_max numeric. Operating range maximum
#' @param roc_threshold numeric. Maximum allowed rate of change
#' @inheritParams flag
#' @param time_small numeric. Number of rows that counts as a small block of data
#' @param time_large numeric. Number of rows that counts as a large block of data
#'
#' @importFrom dplyr mutate mutate_at case_when
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
flag_poi <- function(data_poi,
                     operating_range_min, operating_range_max,
                     roc_threshold,
                     sensor_chars, sensor_maint, time_small, time_large) {

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
    select(2, flag)

  colnames(data_poi)[2] <- paste(colnames(data_poi)[1], "Flag", sep = "_")

  return(data_poi)

}

#' Title
#'
#' @inheritParams flag_poi
#'
#' @importFrom dplyr mutate select case_when
#' @importFrom sqldf sqldf
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
do_flag_1 <- function(data_poi, time_small, time_large) {

  parameter <- colnames(data_poi)[2]

  missing_chunks <- data.frame(unclass(rle(data_poi$flag_m))) %>%
    mutate(end_datetime_i   = cumsum(lengths),
           start_datetime_i = case_when(row_number() == 1 ~ 1,
                                        TRUE ~ lag(end_datetime_i)+1),
           end_datetime   = data_poi$datetime[end_datetime_i],
           start_datetime = data_poi$datetime[start_datetime_i],
           flag_1 = "",
           lengths = as.numeric(lengths)) %>%
    select(-c(end_datetime_i, start_datetime_i)) %>%
    check_missing_chunks(time_small, time_large)

  data_poi <- sqldf("SELECT * FROM data_poi
                 LEFT JOIN missing_chunks
                 ON data_poi.datetime >= missing_chunks.start_datetime AND
                     data_poi.datetime <= missing_chunks.end_datetime") %>%
    select(-(lengths:start_datetime))

  if (grepl(parameter, "sp.*cond", ignore.case = TRUE) |
      grepl(parameter, "pH", ignore.case = FALSE) |
      grepl(parameter, "dissolved.*oxygen", ignore.case = TRUE) |
      grepl(parameter, "DO", ignore.case = FALSE)) {
    data_poi <- data_poi %>%
      mutate(flag_1 = case_when(.[[2]] == 0 ~ "B1",
                                TRUE ~ flag_1))
  }

  return(data_poi)

}

#' Title
#'
#' @inheritParams flag_poi
#'
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
do_flag_2 <- function(data_poi, operating_range_min, operating_range_max) {

  data_poi <- data_poi %>%
    mutate(flag_2 = case_when(.[[2]] <= operating_range_min |
                                .[[2]] >= operating_range_max
                              ~ "B2",
                              TRUE ~ ""))

}

#' Title
#'
#' @inheritParams flag_poi
#'
#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
do_flag_3 <- function(data_poi) {

  ci <- calculate_99_ci(filter(data_poi,
                               flag_m != "M" & flag_1 != "B1" & flag_2 != "B2")[,2])

  local_range_min <- ci[[which(names(ci) == "bound_lower")]]
  local_range_min <- ci[[which(names(ci) == "bound_upper")]]

  data_poi <- data_poi %>%
    mutate(flag_3 = case_when(.[[2]] <= local_range_min |
                                .[[2]] >= local_range_min
                              ~ "B3",
                              TRUE ~ ""))

}

#' Title
#'
#' @inheritParams flag_poi
#'
#' @importFrom dplyr mutate select case_when lag lead
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
do_flag_4 <- function(data_poi, roc_threshold) {

  data_poi <- data_poi %>%
    mutate(diff = abs(.[[2]] - lag(.[[2]])),
           flag_4 = case_when((    diff   >= roc_threshold |
                               lag(diff)  >= roc_threshold |
                               lead(diff) >= roc_threshold) ~ "B4",
                              TRUE ~ "")) %>%
    select(-diff)

}

#' Calculate flag for parameter of interest (not including X flags)
#'
#' @inheritParams flag_poi
#'
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
do_flag_m_to_4 <- function(data_poi) {

  data_poi <- data_poi %>%
    mutate(flag = case_when(flag_1 == "B1" ~ flag_1,
                            flag_m == "M"  ~ flag_m,
                            flag_2 == "B2" ~ flag_2,
                            flag_3 == "B3" ~ flag_3,
                            flag_4 == "B4" ~ flag_4))

}

#' Title
#'
#' @param data dataframe. datetime, flag_x, and all other parameter cols and
#'  their flags
#'
#' @importFrom dplyr mutate_at case_when vars matches
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
do_flag_final <- function(data) {

  data <- data %>%
    mutate_at(vars(matches("_Flag")), ~ case_when(flag_x != "" ~ flag_x,
                                             TRUE ~ .x))

}
