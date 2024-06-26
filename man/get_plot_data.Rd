% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{get_plot_data}
\alias{get_plot_data}
\title{Retrieve data to plot in user interface}
\usage{
get_plot_data(data, index)
}
\arguments{
\item{data}{dataframe. Output from combine_buoy()}

\item{index}{numeric. List index of parameter we wish to plot}
}
\value{
dataframe. One of the list elements output from process_buoy()
}
\description{
Retrieve data to plot in user interface
}
