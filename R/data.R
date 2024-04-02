#' Gene Expression of Pediatric Cancer
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


#' Abundance of Fungal Communities in Cheese
#'
#' Fungal community abundance of 74 ASVs sampled from the surface of three
#' different French cheeses.
#'
#' @usage data("cheese_abundance", package = "scimo")
#'
#' @format For `cheese_abundance`, a [tibble][tibble::tibble-package]
#' with columns:
#' \describe{
#'   \item{sample}{Sample ID.}
#'   \item{cheese}{Appellation of the cheese. One of `Saint-Nectaire`,
#'   `Livarot` or `Epoisses`.}
#'   \item{rind_type}{One of `Natural` or `Washed`.}
#'   \item{other columns}{Count of the ASV.}
#' }
#'
#' @source This dataset came from \doi{10.24072/pcjournal.321}.
#'
#' @examples
#' data("cheese_abundance", package = "scimo")
#' cheese_abundance
"cheese_abundance"

#' @rdname cheese_abundance
#'
#' @usage data("cheese_taxonomy", package = "scimo")
#'
#' @format For `cheese_taxonomy`, a [tibble][tibble::tibble-package]
#' with columns:
#' \describe{
#'   \item{asv}{Amplicon Sequence Variant (ASV) ID.}
#'   \item{lineage}{Character corresponding to a standard concatenation of
#'   taxonomic clades.}
#'   \item{other columns}{Clade to which the ASV belongs.}
#' }
#' @examples
#' data("cheese_taxonomy", package = "scimo")
#' cheese_taxonomy
"cheese_taxonomy"
