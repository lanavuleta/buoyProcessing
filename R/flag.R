#' Title
#'
#' @inheritParams format_datetime
#' @inheritParams check_params_units
#' @param sensor_maint Output from read_sensor_maint()
#'
#' @importFrom purrr map pmap
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select all_of
#'
#' @return dataframe
#' @export
flag <- function(data, sensor_chars, sensor_maint) {

  flag_intervals <- get_flag_intervals(data)

  data <- data %>%
    do_flag_x(sensor_maint)

  cols <- seq(2,ncol(data))

  # pois = ParameterS Of Interest
  data_pois <- cols %>% map(function(x) select(data, all_of(c(1, x))))

  operating_range_mins <- sensor_chars$operating_range_min
  operating_range_maxs <- sensor_chars$operating_range_max

  data_pois <- pmap(data_pois, operating_range_mins, operating_range_maxs,
                    flag_poi,
                    sensor_chars, sensor_maint,
                    time_small = flag_intervals[[which(names(flag_intervals) == "time_small")]],
                    time_large = flag_intervals[[which(names(flag_intervals) == "time_large")]])

}

#' Title
#'
#' @param data_poi dataframe. Buoy data's datetime column, parameter of interest
#'  and created flag columns as their respective do_flag functions are run
#' @param operating_range_min numeric. Operating range minimum
#' @param operating_range_max numeric. Operating range maximum
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
                     sensor_chars, sensor_maint, time_small, time_large) {

  parameter <- colnames(data_poi)[2]

  data_poi <- data_poi %>%
    mutate(flag_m = case_when(.[[2]] %in% missing_vals ~ "M",
                              TRUE ~ "")) %>%
    mutate_at(2, as.numeric) %>%

    do_flag_1(time_small, time_large) %>%
    do_flag_2(operating_range_min, operating_range_max)

  #colnames(data_poi)[3] <- paste(colnames(data_poi)[2], "Flag", sep = "_")

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

