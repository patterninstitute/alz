% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cut_by.R
\name{bins}
\alias{bins}
\title{An enumeration of bin labels for positivity status}
\usage{
bins(n = 2)
}
\arguments{
\item{n}{Number of levels to be returned. Use \code{n = 2} for binary
classification (negative and positive). Use \code{n = 3} to also include the
intermediate category (\code{"~"}).}
}
\value{
A character vector of length 2 or 3, representing diagnostic bin
labels.
}
\description{
\code{\link[=bins]{bins()}} returns a character vector representing the levels used to classify
biomarker values into diagnostic bins:
\itemize{
\item \code{"-"}, for negative / unimpaired / normal status
\item \code{"~"}, for indeterminate / borderline / uncertain status
\item \code{"+"}, for positive / impaired / abnormal status
}
}
\examples{
# Use `n = 2` for normal (`"-"`) and abnormal (`"+"`)
bins(n = 2)

# Use `n = 3` to also include the indeterminate category (`"~"`)
bins(n = 3)

}
