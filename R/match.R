#' Title
#'
#' @inheritParams check_chars_error_match
#'
#' @importFrom dplyr mutate select
#' @importFrom fuzzyjoin regex_join
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
match_chars_error <- function(sensor_chars, error_drift) {

  matches_char_error <- regex_join(sensor_chars, error_drift,
                                   by = c("sensor_header" = "sensor", "unit"),
                                   mode = "left") %>%
    # unit from error_drift not needed. If there was a match, value is in
    # unit.x, and if there was not a match, this would have been caught with
    # error_missing
    mutate(unit = unit.x) %>%
    select(-c(unit.x, unit.y))

}

#' Title
#'
#' @inheritParams edit_error_drift
#'
#' @importFrom dplyr mutate select left_join
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
match_error_maint <- function(error_drift, sensor_maint) {

  sensor_maint <- sensor_maint %>%
    mutate(end_date = as.Date(end_datetime))

  error_drift <- error_drift %>%
    left_join(sensor_maint, by = c("date" = "end_date")) %>%
    select(colnames(error_drift), end_datetime)

}
