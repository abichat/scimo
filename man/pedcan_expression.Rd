% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{pedcan_expression}
\alias{pedcan_expression}
\title{Gene Expression of Pediatric Cancer}
\format{
A \link[tibble:tibble-package]{tibble} with columns:
\describe{
\item{cell_line}{Cell line name.}
\item{sex}{One of \code{Male}, \code{Female} or \code{Unknown}.}
\item{event}{One of \code{Primary}, \code{Metastasis} or \code{Unknown}.}
\item{disease}{One of \code{Neuroblastoma}, \verb{Ewing Sarcoma},
\code{Rhabdomyosarcoma}, \verb{Embryonal Tumor} or \code{Osteosarcoma}.}
\item{other columns}{Expression of the gene, given in log2(TPM + 1).}
}
}
\source{
This dataset is generated from DepMap Public 23Q4 primary files.
\url{https://depmap.org/portal/download/all/}.
}
\usage{
data("pedcan_expression", package = "scimo")
}
\description{
Gene expression of 108 CCLE cell lines from 5 different pediatric cancers.
}
\examples{
data("pedcan_expression", package = "scimo")
pedcan_expression
}
\keyword{datasets}
