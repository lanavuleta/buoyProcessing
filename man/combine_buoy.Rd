% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_buoy.R
\name{combine_buoy}
\alias{combine_buoy}
\title{Combine the list of dataframes output from process_buoy()
The list of dataframes output makes visualizing the data easier in the user
interface. For better data export, the lists must be combined into one single
dataset.}
\usage{
combine_buoy(data)
}
\arguments{
\item{data}{list of dataframes. Output from process_buoy()}
}
\value{
dataframe. All buoy data in one single dataframe
}
\description{
Combine the list of dataframes output from process_buoy()
The list of dataframes output makes visualizing the data easier in the user
interface. For better data export, the lists must be combined into one single
dataset.
}
