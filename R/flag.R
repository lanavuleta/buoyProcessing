#' Title
#'
#' @inheritParams flag_and_error
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
    missing_chunks_flag_1(time_small, time_large)

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

  ci <- calculate_99_ci(data_poi[,2])

  local_range_min <- ci[[which(names(ci) == "bound_lower")]]
  local_range_max <- ci[[which(names(ci) == "bound_upper")]]

  data_poi <- data_poi %>%
    mutate(flag_3 = case_when(.[[2]] <= local_range_min |
                                .[[2]] >= local_range_max
                              ~ "B3",
                              TRUE ~ ""))

}

#' Title
#'
#' @inheritParams flag_poi
#'
#' @importFrom dplyr mutate select case_when lag lead
#' @importFrom magrittr "%>%"
#' @importFrom RcppRoll roll_median
#'
#' @return dataframe
#' @export
do_flag_4 <- function(data_poi, roc_threshold, time_small) {

  # In literature, flag data when their absolute deviation around the median
  # calculated over the last w previous values is larger than 3 times their median.
  # If no ROC is given, use the equation used in the literature
  if (is.na(roc_threshold)) {
    data_poi$med <- lag(roll_median(unlist(data_poi[,2]),
                                     n = 10, fill = NA, align = "right"))

    data_poi <- data_poi %>%
      mutate(flag_4 = case_when(abs(med - .[[2]]) > 3*med ~ "B4",
                                TRUE ~ ""),
             # Flag the values on either side of the large roc
             flag_4 = ifelse((lag(flag_4) == "B4" | lead(flag_4) == "B4"),
                             "B4",
                             ""),
             # roll_median leaves NAs
             flag_4 = ifelse(is.na(flag_4), "", flag_4)) %>%
      select(-med)
  } else {
    data_poi <- data_poi %>%
      mutate(diff = abs(.[[2]] - lag(.[[2]])),
             flag_4 = case_when((    diff   >= roc_threshold |
                                 # Flag the values on either side of the large roc
                                 lag(diff)  >= roc_threshold |
                                 lead(diff) >= roc_threshold) ~ "B4",
                                TRUE ~ "")) %>%
      select(-diff)
  }

  return(data_poi)

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


#' Title
#'
#' @param missing_chunks dataframe. Edited run length encoding created in do_flag_1()
#' @inheritParams do_flag_1
#'
#' @importFrom dplyr lead lag
#'
#' @return dataframe
#' @export
missing_chunks_flag_1 <- function(missing_chunks, time_small, time_large) {

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
