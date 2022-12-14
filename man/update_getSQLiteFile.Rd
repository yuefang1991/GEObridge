% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GEObridge.R
\name{update_getSQLiteFile}
\alias{update_getSQLiteFile}
\title{This function has two goals. First, prepare the database locally for downstream searching.
Second, print the detailed columns in subTable (e.g. gse).}
\usage{
update_getSQLiteFile(
  destdir = getwd(),
  destfile = "GEOmetadb.sqlite.gz",
  type = "full"
)
}
\arguments{
\item{destdir}{The destination directory of the downloaded file.}

\item{destfile}{The filename of the downloaded file. This filename should end in ".gz" as the unzipping assumes that is the case.}

\item{type}{Type of GEOmetadb.sqlite to download, if it is 'normal', a full database will be downloaded, otherwise a demo database will be downloaded, which is 25MB..}
}
\value{
The local filename for use later..
}
\description{
This function has two goals. First, prepare the database locally for downstream searching.
Second, print the detailed columns in subTable (e.g. gse).
}
