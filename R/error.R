#' Title
#'
#' @inheritParams error_poi
#'
#' @importFrom dplyr arrange mutate case_when
#' @importFrom magrittr "%>%"
#'
#' @return dataframe
#' @export
calculate_grade <- function(accuracy_val, error_info) {

  error_info <- error_info %>%
    arrange(end_datetime) %>%
    mutate(grade = case_when(is.na(error)        ~ NA,
                             error <= accuracy_val   ~ "E",
                             error <= accuracy_val*2 ~ "VG",
                             error <= accuracy_val*4 ~ "G",
                             error <= accuracy_val*6 ~ "F",
                             TRUE                ~ "P"))

}
