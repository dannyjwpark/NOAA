% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization tools.R
\name{plot_earthquakes_timeline}
\alias{plot_earthquakes_timeline}
\title{Plot the earthquake timeline and optionally save}
\usage{
plot_earthquakes_timeline(df, save_png = FALSE)
}
\arguments{
\item{df}{A dataframe of the earthquake data your wish to plot}

\item{save_png}{Boolean, default is FALSE, of whether to save as a png file}
}
\description{
Given a subset of earthquake data (df) this will plot the
data as a timeline (without label annotations) and apply formatting.
The resulting image will then be saved as earthquakes_timeline.png if
the parameter save_png is TRUE.
}
\examples{
\dontrun{
df <- df \%>\%
  filter(COUNTRY \%in\% c("CHINA", "USA"), YEAR > 2000)
plot_earthquakes_timeline(df, save_png=TRUE)
}
}
