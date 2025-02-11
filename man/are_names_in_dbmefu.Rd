% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/are_names_in_dbmefu.R
\name{are_names_in_dbmefu}
\alias{are_names_in_dbmefu}
\title{Find which names are present in dbmefu}
\usage{
are_names_in_dbmefu(names, dbmefu, folder = NA)
}
\arguments{
\item{names}{a list of names}

\item{dbmefu}{the one and only MeFu database}

\item{folder}{if set to "choose" let you choose navigating in your folder system.
If a path is provided it'll save there. N.B.: The path format has to be of
the kind "C://folder1//folder2"}
}
\value{
a list of two vectors: first are names in dbmefu, second are names not present in dbmefu
}
\description{
Find which names are present in dbmefu and store outputs in csv
}
