#' Title
#'
#' @inheritParams error_poi
#'
#' @importFrom dplyr arrange mutate case_when
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
calculate_grade <- function(accuracy, error_info) {

  error_info <- error_info %>%
    arrange(end_datetime) %>%
    mutate(grade = case_when(is.na(error)        ~ NA,
                             error <= accuracy   ~ "E",
                             error <= accuracy*2 ~ "VG",
                             error <= accuracy*4 ~ "G",
                             error <= accuracy*6 ~ "F",
                             TRUE                ~ "P"))

}
