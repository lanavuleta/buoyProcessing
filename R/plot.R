#' Title
#'
#' @param data dataframe. Output from combine_buoy()
#' @param index numeric. List index of parameter we wish to plot
#'
#' @return ggplot
#' @export
process_plot <- function(data, index) {
  make_plot(get_plot_data(data, index))
}

#' Title
#'
#' @param data dataframe. Output from combine_buoy()
#' @param index numeric. List index of parameter we wish to plot
#'
#' @return dataframe. One of the list elements output from process_buoy()
#' @export
get_plot_data <- function(data, index) {
  data[[index]]
}

#' Title
#'
#' @param data_poi dataframe. Output from get_plot_data()
#'
#' @importFrom dplyr case_when mutate_at vars matches rename
#'
#' @return ggplot
#' @export
edit_plot_data <- function(data_poi) {
  flags <- c("Unflagged", "M",
             "1","1,2","1,3","1,4","1,2,3","1,2,4","1,3,4","1,2,3,4",
             "2","2,3","2,4","2,3,4",
             "3","3,4",
             "4",
             "X")

  errors <- c("E", "VG", "G", "F", "P", "")

  data_poi <- data_poi %>%
    mutate_at(vars(matches("_Flag")),
              ~ case_when(. == "" ~ "Unflagged",
                          grepl("X", .) ~ "X",
                          TRUE ~ .)) %>%
    mutate_at(vars(matches("_Flag")),
              ~ factor(., levels = flags)) %>%
    # Particularly relevant when reading in QCd data - R might not realize that
    # the column is a datetime
    mutate(datetime = as.POSIXct(datetime)) %>%
    # To make plotting easier (cannot plot based on column index, and names will
    # change from parameter to parameter)
    rename(values = 2, flag = 3)
}

#' Title
#'
#' @param data_poi dataframe. Output from get_plot_data()
#'
#' @importFrom dplyr mutate_at rename filter vars matches
#' @importFrom ggplot2 ggplot geom_tile geom_point theme_minimal aes
#'     scale_fill_manual scale_color_manual scale_shape_manual xlab ylab
#'
#' @return ggplot
#' @export
make_plot <- function(data_poi) {
  flags <- c("Unflagged", "M",
             "1","1,2","1,3","1,4","1,2,3","1,2,4","1,3,4","1,2,3,4",
             "2","2,3","2,4","2,3,4",
             "3","3,4",
             "4",
             "X")

  errors <- c("E", "VG", "G", "F", "P", "")

  parameter <- names(data_poi)[2]

  # We separate flagged and unflagged data so as to plot the flagged data on top
  # of the unflagged data to better see it
  data_unflagged <- filter(data_poi, flag == "Unflagged")
  data_flagged   <- filter(data_poi, flag != "Unflagged")

  full_plot <- ggplot() +
    geom_point(data = data_unflagged, aes(x = datetime, y = values, colour = flag, shape = flag)) +
    geom_point(data = data_flagged, aes(x = datetime, y = values, colour = flag, shape = flag)) +
    theme_minimal() +
    scale_color_manual(
      values = c("black", "black",
                 "#F748A5","#F748A5","#F748A5","#F748A5",
                 "#F748A5","#F748A5","#F748A5","#F748A5",
                 "#D55E00","#D55E00","#D55E00","#D55E00",
                 "#359B73","#359B73",
                 "#2271B2",
                 "black"),
      name = c("Flags"),
      drop = FALSE) +
    scale_shape_manual(
      values = c(20, 20,
                 1,3,3,3,2,2,2,0,
                 1,3,3,2,
                 1,3,
                 1,
                 4),
      name = c("Flags"),
      drop = FALSE) +
    xlab("Date") +
    ylab(parameter)

  # Parameter has an error column as well to be handled
  if (ncol(data_poi) == 4) {
    data_poi <- data_poi %>%
      # Making error column easier to plot
      mutate_at(vars(matches("_Error")),
                ~ ifelse(is.na(.), "", .)) %>%
      mutate_at(vars(matches("_Error")),
                ~ factor(., levels = errors)) %>%
      # To make plotting easier (cannot plot based on column index, and names will
      # change from parameter to parameter)
      rename(error = 4)

    # Calculating values needed to set the error background colour positions
    max_v <- max(data_poi$values, na.rm = T)
    min_v <- min(data_poi$values, na.rm = T)

    midpt  <- ((max_v - min_v)/2) + min_v
    height <- max_v - min_v

    full_plot <- full_plot +
      geom_tile(data = data_poi, aes(x = datetime, y = midpt, fill = error),
                alpha = 0.1, height = height) +
      scale_fill_manual("Error",
                        values = c("#359B73", "#3DB7E9",
                                   "#F0E442","#E69F00",
                                   "#D55E00", "white"),
                        drop = FALSE)
  }

  return(full_plot)

}
