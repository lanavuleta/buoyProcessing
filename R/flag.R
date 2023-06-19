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
do_flag_1 <- function(data_poi, time_small, time_large, repeat_0s_max) {

  # Flag based on missing vals -------------------------------------------------
  # If vals missing for short period of time OR
  # there are few vals in large block of missing vals

  missing_chunks <- data.frame(unclass(rle(data_poi$flag_m))) %>%
    mutate(end_datetime_i   = cumsum(lengths),
           start_datetime_i = case_when(row_number() == 1 ~ 1,
                                        TRUE ~ lag(end_datetime_i)+1),
           end_datetime   = data_poi$datetime[end_datetime_i],
           start_datetime = data_poi$datetime[start_datetime_i],
           flag_1 = "",
           # rle does not consistently return numeric
           lengths = as.numeric(lengths)) %>%
    select(-c(end_datetime_i, start_datetime_i)) %>%
    missing_chunks_flag_1(time_small, time_large) %>%
    filter(flag_1 == "1") %>%
    select(start_datetime, end_datetime, flag_1)

  # Flag based on repeat 0s ----------------------------------------------------
  zeros <- data.frame(unclass(rle(data_poi[[2]]))) %>%
    mutate(end_datetime_i   = cumsum(lengths),
           start_datetime_i = case_when(row_number() == 1 ~ 1,
                                        TRUE ~ lag(end_datetime_i)+1),
           end_datetime   = data_poi$datetime[end_datetime_i],
           start_datetime = data_poi$datetime[start_datetime_i],
           flag_1z = "1",
           # rle does not consistently return numeric
           lengths = as.numeric(lengths),
           values = as.numeric(values)) %>%
    filter(values == 0 & lengths >= repeat_0s_max) %>%
    select(start_datetime, end_datetime, flag_1z)

  # Apply flags for date ranges discovered above -------------------------------
  data_poi <- sqldf("SELECT * FROM data_poi
                 LEFT JOIN missing_chunks
                 ON data_poi.datetime >= missing_chunks.start_datetime AND
                     data_poi.datetime <= missing_chunks.end_datetime
                 LEFT JOIN zeros
                 ON data_poi.datetime >= zeros.start_datetime AND
                     data_poi.datetime <= zeros.end_datetime") %>%
    select(-c(start_datetime, end_datetime)) %>%
    mutate(flag_1 = case_when(flag_1z == "1" | flag_1 == "1" ~ "1",
                              TRUE ~ NA_character_)) %>%
    select(-c(flag_1z))

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
                              ~ "2",
                              TRUE ~ NA_character_))

}

#' Title
#'
#' @inheritParams flag_poi
#'
#' @importFrom dplyr filter pull
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
do_flag_3 <- function(data_poi, local_range_min, local_range_max) {

  data_poi <- data_poi %>%
    mutate(flag_3 = case_when(.[[2]] <= local_range_min |
                                .[[2]] >= local_range_max
                              ~ "3",
                              TRUE ~ NA_character_))

}

#' Title
#'
#' @inheritParams flag_poi
#'
#' @importFrom dplyr mutate select case_when lag lead
#' @importFrom magrittr "%>%"
#' @importFrom forecast tsoutliers
#'
#' @return dataframe
#' @export
do_flag_4 <- function(data_poi, roc_threshold, time_small) {

  # Check if ROC as defined by user is surpassed -------------------------------
  data_poi <- data_poi %>%
    mutate(diff = abs(.[[2]] - lag(.[[2]])),
           flag_4 = case_when((
             diff   >= roc_threshold |
               # Flag the values on either side of the large roc
               lag(diff)  >= roc_threshold |
               lead(diff) >= roc_threshold)
             ~ "4",
           TRUE
             ~ NA_character_)) %>%
    select(-diff)

  # Flag outliers as identified by tsoutliers() --------------------------------
  data_poi$flag_4[tsoutliers(pull(data_poi, 2))$index] <- "4"

  return(data_poi)

}

#' Calculate flag for parameter of interest (not including X flags)
#'
#' @inheritParams flag_poi
#'
#' @importFrom dplyr mutate case_when
#' @importFrom magrittr "%>%"
#' @importFrom tidyr unite
#'
#' @return dataframe
#' @export
do_flag_m_to_4 <- function(data_poi, combine_flags) {

  # flag_m would've been set to "" instead of NA_character_. Now we set to NA
  data_poi <- data_poi %>%
    mutate(flag_m = ifelse(flag_m == "", NA_character_, flag_m))

  if (isFALSE(combine_flags)) {
    data_poi <- data_poi %>%
      mutate(flag = case_when(flag_1 == "1" ~ flag_1,
                              flag_m == "M"  ~ flag_m,
                              flag_2 == "2" ~ flag_2,
                              flag_3 == "3" ~ flag_3,
                              flag_4 == "4" ~ flag_4,
                              TRUE ~ NA_character_))
  } else {
    data_poi <- data_poi %>%
      # TBD What we want flags to look like
      # All "" set to na for na.rm to work in unite() below
      mutate(flag_m = ifelse(!is.na(flag_1) | flag_m == "", NA_character_, flag_m)) %>%
      unite("flag", flag_m:flag_4, remove = TRUE, na.rm = TRUE, sep = ",")
  }

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
      missing_chunks$flag_1[i] <- "1"
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
      missing_chunks$flag_1[i] <- "1"
    }
  }

  return(missing_chunks)

}
