calculate_grade <- function(sensor_chars) {

  sensor_chars1 <- sensor_chars %>%
    mutate(grade = case_when(is.na(error)            ~ NA,
                             error <= accuracy_val   ~ "E",
                             error <= accuracy_val*2 ~ "VG",
                             error <= accuracy_val*4 ~ "G",
                             error <= accuracy_val*6 ~ "F",
                             TRUE                    ~ "P")) %>%
    select(param, unit, date, grade)

}
