% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GEObridge.R
\name{print_top_N_platform}
\alias{print_top_N_platform}
\title{The function to print the most frequent platform.}
\usage{
print_top_N_platform(
  limitN = 2000,
  topN = 10,
  verbose = TRUE,
  db = con,
  subTable = "gse"
)
}
\arguments{
\item{limitN}{An integer variable to choose largest number query from the database.}

\item{topN}{Due to too much platform collected in the database, it is better to only print N most frequent platforms.}

\item{verbose}{A logical variable. It will print some disgnostic information if TRUE.}

\item{db}{The connection variable which contains all metadata.}

\item{subTable}{Character variable. It is the sub-table name which stores different types of metadata. .}
}
\value{
A variable contains Top N platform information.
}
\description{
The function to print the most frequent platform.
}
