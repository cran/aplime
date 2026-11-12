\name{aplime_csi}
\alias{aplime_csi}
\title{Coefficient Stability Index}
\description{Computes pairwise interval overlaps among matching MARS coefficient groups. Knot positions are ignored when matching terms but hinge directions and interaction structure are retained. When multiple coefficients share a group in one fit, their arithmetic mean is used and the normal interval uses their joint covariance. Groups with fewer than two comparable intervals cannot contribute to the index.}
\usage{
aplime_csi(records, ci_method = c("normal", "bootstrap"))
}
\arguments{
\item{records}{List of repeated explanations containing coefficient confidence interval tables.}
\item{ci_method}{Confidence interval type: \code{"normal"} for normal-approximation intervals or \code{"bootstrap"} for percentile-bootstrap intervals.}
}
\value{Percentage of overlap averaged across comparable groups, or \code{NA_real_} if none can be compared.}
