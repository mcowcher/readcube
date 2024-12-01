% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data.R
\name{read_data}
\alias{read_data}
\title{Read Volumetric Data}
\usage{
read_data(result)
}
\arguments{
\item{result}{A named list where result$inp is the connection to the
file to read from, and result$out is an (initially) empty list to
write parsed data to.}
}
\value{
A named list the same as its argument, with newly parsed
data.
}
\description{
Reads the volumetric data into a dataframe of a single variable, where each
row is the data at a single voxel. The positional data associated with each
row depends on how the file was generated, but the convention is to write
values from an inner z loop, to an outer x loop.
}
