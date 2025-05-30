% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cutpoints.R
\name{cutpoints}
\alias{cutpoints}
\alias{cutpts}
\title{Retrieve biomarker cut-point values or metadata}
\usage{
cutpoints()

cutpts(cutpt_id)
}
\arguments{
\item{cutpt_id}{A character scalar specifying the identifier of the
cut-point to retrieve.}
}
\value{
\itemize{
\item For \code{cutpoints()}: a tibble with columns \code{cutpt_id}, \code{bmk_id},
\code{cutpts}, \code{unit}, \code{assay_id}, \code{intended_use}, \code{source}, and \code{notes}.
\item For \code{cutpts()}: numeric value(s) corresponding to the threshold(s) defined
for that biomarker-assay combination.
}
}
\description{
\itemize{
\item \code{cutpoints()}: Provides access to a curated table of biomarker cut-points
used to dichotomise or categorise continuous biomarker measurements in
clinical or research settings.
\item \code{cutpts()} returns only the numeric cut-point(s) associated with that
identifier. These thresholds are typically used for classifying biomarker
values into diagnostic categories such as positive, negative, or
indeterminate, see \code{\link[=bin_by]{bin_by()}}.
}
}
\examples{
# Return the full cut-point table with metadata
cutpoints()

# Retrieve the numeric cut-point(s) for a specific identifier
cutpts("ab42-csf-elecsys-willemse2018")
cutpts("ptau181-ab42-ratio-csf-elecsys-willemse2018")

}
\seealso{
\code{\link[=bin_by]{bin_by()}}, \code{\link[=cv_cutpts]{cv_cutpts()}}
}
