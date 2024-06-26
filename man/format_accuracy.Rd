% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{format_accuracy}
\alias{format_accuracy}
\title{Format the accuracy column of the sensor characteristics sheet
Make sure that the user-provided accuracy for all sensors listed in the error
drift sheet fulfill that format requirements so that the tool can properly
understand the user-provided accuracy. Send error messages if user has not
provided satisfactory accuracies.}
\usage{
format_accuracy(sca)
}
\arguments{
\item{sca}{dataframe. sensor_chars as edited in edit_sensor_chars()}
}
\value{
dataframe (or the formatted accuracy) OR NA if no accuracy was
detected OR error message if accuracy was incorrectly input
}
\description{
Format the accuracy column of the sensor characteristics sheet
Make sure that the user-provided accuracy for all sensors listed in the error
drift sheet fulfill that format requirements so that the tool can properly
understand the user-provided accuracy. Send error messages if user has not
provided satisfactory accuracies.
}
