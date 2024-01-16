#' Pediatric cancer gene expression
#'
#' Gene expression of 108 CCLE cell lines from 5 different pediatric cancers.
#'
#' @usage data("pedcan_expression", package = "scimo")
#'
#' @format A [tibble][tibble::tibble-package] with columns:
#' \describe{
#'   \item{cell_line}{Cell line name.}
#'   \item{sex}{One of `Male`, `Female` or `Unknown`.}
#'   \item{event}{One of `Primary`, `Metastasis` or `Unknown`.}
#'   \item{disease}{One of `Neuroblastoma`, `Ewing Sarcoma`,
#'   `Rhabdomyosarcoma`, `Embryonal Tumor` or `Osteosarcoma`.}
#'   \item{other columns}{Expression of the gene, given in log2(TPM + 1).}
#' }
#'
#' @source This dataset is generated from DepMap Public 23Q4 primary files.
#' \url{https://depmap.org/portal/download/all/}.
#'
#' @examples
#' data("pedcan_expression", package = "scimo")
#' pedcan_expression
"pedcan_expression"
