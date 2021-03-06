% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phylo_parse.R
\name{phylo_parse}
\alias{phylo_parse}
\alias{tip_parse}
\title{Clean and organize data from phylogenetic trees}
\usage{
tip_parse(a)
}
\arguments{
\item{a}{a character vector of a strsplit tip.label}
}
\value{
matricies of the parsed and cleaned data or phlo objects with cleaned tips
}
\description{
functions to parse tip data and make matrices of data that can be inserted into our postSQLgis database
}
\details{
returns a matrix with species, specimen id, and location for each tip on a tree
}
\author{
Matthew R. Helmus
}
