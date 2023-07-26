#' Example of a \linkS4class{GuideSet} object storing gRNA sequences targeting
#'     the CDS of the human gene KRAS
#'
#' Example of a \linkS4class{GuideSet} object (with no additional annotation)
#'    storing gRNA sequences targeting the coding sequence of human gene KRAS
#'    (ENSG00000133703) for SpCas9 nuclease.
#' 
#' @format A \linkS4class{GuideSet} object.
#' 
#' @details The object was obtained by using \code{crisprDesign::findSpacers}
#'     on a GRanges of the CDS region of human gene KRAS. See code in
#'     \code{inst/scripts/generateKrasData.R}.
#' @usage data(guideSetExample_basic, package="crisprShiny")
"guideSetExample_basic"



#' Example of a \linkS4class{GuideSet} object storing gRNA sequences targeting
#'     the CDS of the human gene KRAS
#'
#' Example of a fully annotated \linkS4class{GuideSet} object storing gRNA
#'    sequences targeting the coding sequence of human gene KRAS
#'    (ENSG00000133703) for SpCas9 nuclease.
#' 
#' @format A \linkS4class{GuideSet} object.
#' 
#' @details The object was obtained by applying all available \code{add*}
#'     annotation functions in \code{crisprDesign} on a GuideSet storing gRNAs
#'     targeting the CDS region of human gene KRAS. See code in
#'     \code{inst/scripts/generateKrasData.R}.
#' @usage data(guideSetExample_kras, package="crisprShiny")
"guideSetExample_kras"



#' Example of a \linkS4class{GuideSet} object storing gRNA sequences targeting
#'     the CDS of the human gene KRAS and NTCs
#'
#' Example of a fully annotated \linkS4class{GuideSet} object storing gRNA
#'    sequences targeting the coding sequence of human gene KRAS
#'    (ENSG00000133703) and some non-targeting controls (NTCs) for SpCas9
#'    nuclease.
#' 
#' @format A \linkS4class{GuideSet} object.
#' 
#' @details The object was obtained by applying all available \code{add*}
#'     annotation functions in \code{crisprDesign} on a GuideSet storing gRNAs
#'     targeting the CDS region of human gene KRAS and some NTCs. See code in
#'     \code{inst/scripts/generateKrasData.R}.
#' @usage data(guideSetExample_kras, package="crisprShiny")
"guideSetExample_ntcs"



#' Example of a \linkS4class{GuideSet} object storing gRNA sequences targeting
#'     the CDS of the human gene KRAS
#'
#' Example of a \linkS4class{GuideSet} object storing gRNA
#'    sequences targeting the coding sequence of human gene KRAS
#'    (ENSG00000133703) for BE4max nuclease.
#' 
#' @format A \linkS4class{GuideSet} object.
#' 
#' @details The object was obtained by applying all base-editor-specific
#'     annotation functions in \code{crisprDesign} on the
#'     CDS region of human gene KRAS. See code in
#'     \code{inst/scripts/generateKrasData.R}.
#' @usage data(guideSetExample_kras_be, package="crisprShiny")
"guideSetExample_kras_be"
