% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match.R
\name{match_chars_error}
\alias{match_chars_error}
\title{Match sensors and units: sensor chars and error drift
Match the sensor names and units in the sensor characteristics sheet to those
in the error drift sheet.}
\usage{
match_chars_error(sensor_chars, error_drift)
}
\arguments{
\item{sensor_chars}{dataframe. Output from read_sensor_chars()}

\item{error_drift}{dataframe. Output from read_error_drift()}
}
\value{
dataframe
}
\description{
Match sensors and units: sensor chars and error drift
Match the sensor names and units in the sensor characteristics sheet to those
in the error drift sheet.
}
