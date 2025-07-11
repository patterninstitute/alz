% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assays.R
\name{assays}
\alias{assays}
\title{Alzheimer's disease assays}
\usage{
assays()
}
\value{
A tibble with the following columns:
\describe{
\item{\code{assay_id}}{Unique identifier for the assay.}
\item{\code{analyte_id}}{Reference to the measured analyte (see \code{\link[=analytes]{analytes()}}).}
\item{\code{modality}}{Either \code{"fluid"} or \code{"imaging"}, denoting the type of measurement.}
\item{\code{matrix}}{Biological specimen used, such as \code{"CSF"}, \code{"plasma"}, or \code{"brain"}.}
\item{\code{cv}}{Coefficient of variation.}
\item{\code{lloq}}{Lower limit of quantification.}
\item{\code{uloq}}{Upper limit of quantification.}
\item{\code{unit}}{Measurement unit (e.g., \code{"pg/mL"}, \code{"SUVR"}).}
\item{\code{technology}}{Underlying method used (e.g., \code{"ELISA"}, \code{"PET"}, \code{"Simoa"}).}
\item{\code{vendor}}{Manufacturer of the assay.}
\item{\code{platform}}{Commercial platform or product line name.}
\item{\code{description}}{Descriptive label for the assay.}
}
}
\description{
Returns a tibble containing curated metadata for assay implementations used
to measure fluid or imaging biomarkers relevant to Alzheimer's disease
research. Each row describes a specific combination of vendor, platform, and
technology used to quantify a given analyte.

Assays represent product-specific implementations (e.g., Elecsys, Simoa HD-X,
INNOTEST) that produce numeric measurements for downstream biomarker
classification. Quantitative performance parameters such as lower and upper
limits of quantification (LLOQ, ULOQ) are included where available.
}
\examples{
assays()

}
\seealso{
\code{\link[=analytes]{analytes()}}, \code{\link[=biomarkers]{biomarkers()}}
}
