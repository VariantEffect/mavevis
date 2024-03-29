% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orthologs.R
\name{fetch.domains.pfam}
\alias{fetch.domains.pfam}
\title{Find Pfam domains for a Uniprot accession}
\usage{
fetch.domains.pfam(acc)
}
\arguments{
\item{acc}{the Uniprot accession}
}
\value{
a \code{data.frame} with the following columns:
\itemize{
  \item type: the type of domain or feature (SIGNAL, DOMAIN, or REPEAT)
  \item method: the experimental method for this structure, e.g NMR, X-ray or Model
  \item name: the name of the domain or feature
  \item start: the start amino acid position
  \item end: the end amino acid position
}
}
\description{
Finds Pfam domains belonging to the given Uniprot accession.
Checks for a local cache file of previous results. If such a cache exists, the pre-calculated
results will be returned, otherwise queries to Uniprot and PDB will be made.
}
