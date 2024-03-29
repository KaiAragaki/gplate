% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rel_dim}
\alias{rel_dim}
\title{Performs \code{flip_dim} if necessary}
\usage{
rel_dim(gp, dim, rel)
}
\arguments{
\item{gp}{A \code{gp}}

\item{dim}{Symbol. Column to conditionally flip. Should be the name of a column that exists in \code{gp$well_data}}

\item{rel}{Character. Column to check \code{if_fwd} on. If TRUE, return \code{dim} as is. If FALSE (\code{rel} is backwards), flip the column.}
}
\value{
A vector that is flipped (see \code{flip_dim}) if \code{rel} is backwards (see \code{is_fwd})
}
\description{
Performs \code{flip_dim} if necessary
}
