\name{aplime_knot_report}
\alias{aplime_knot_report}
\title{Report Original MARS Hinge Knots}
\description{Lists each original hinge basis term and its coefficient, with a knot-insensitive group key and a logical flag indicating whether different original terms were matched in the group. The report retains all original thresholds even though CSI uses grouped coefficient means.}
\usage{
aplime_knot_report(records)
}
\arguments{
\item{records}{List of repeated \code{aplime_explanation} objects.}
}
\value{Data frame with repetition, term_group, original_term, hinges, coefficient, and different_knots.}
