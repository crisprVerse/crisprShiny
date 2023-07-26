#' @importFrom S4Vectors mcols
#' @importFrom crisprDesign bsgenome queryTxObject
#' @importFrom GenomeInfoDb commonName seqnames
#' @importFrom utils data
#' @importFrom BiocGenerics start end strand
.getGuideBrowserParameters <- function(guideSet,
                                       gene,
                                       geneModel,
                                       selectedRows,
                                       plotWindow,
                                       guideStacking
){
    guideSet <- guideSet[selectedRows]
    if (length(guideSet) == 0){ # invalid selection
        params <- list("error"="index")
        return(params)
    }
    chrs <- GenomeInfoDb::seqnames(guideSet)
    if (length(unique(chrs)) > 1){ # multiple chromosomes
        params <- list("error"="chr")
        return(params)
    }
    if (length(guideSet) > 20){ # gRNA stack limit
        params <- list("error"="render")
        return(params)
    }
    
    if (gene == ""){
        gene <- NULL
    }
    bsgenome <- crisprDesign::bsgenome(guideSet)
    species <- GenomeInfoDb::commonName(bsgenome)
    species <- tolower(species)
    # txObject <- paste0("txdb_", species) ## ARG SHOULD BE PROVIDED
    if (!is.null(geneModel)){
        geneRanges <- crisprDesign::queryTxObject(
            txObject=geneModel,
            featureType="transcripts",
            queryColumn="gene_symbol",
            queryValue=gene
        )
    } else {
        geneRanges <- NULL
    }
    if (isTRUE(plotWindow == "zoom")){
        start <- min(BiocGenerics::start(guideSet))
        end <- max(BiocGenerics::end(guideSet))
    } else {
        if (length(geneRanges) > 0){
            start <- min(c(BiocGenerics::start(geneRanges),
                           BiocGenerics::start(guideSet)))
            end <- max(c(BiocGenerics::end(geneRanges),
                         BiocGenerics::end(guideSet)))
        } else {
            start <- min(BiocGenerics::start(guideSet))
            end <- max(BiocGenerics::end(guideSet))
            gene <- NULL
        }
    }
    
    extensionFactor <- 0.1
    extension <- max(100, round(extensionFactor * (end - start))) # 100 as min padding
    start <- max(1, start - extension)
    end <- end + extension
    chr <- as.character(unique(GenomeInfoDb::seqnames(guideSet)))
    coordinates <- paste0(
        chr, ":",
        format(start, big.mark=",", scientific=FALSE, trim=TRUE), "-",
        format(end, big.mark=",", scientific=FALSE, trime=TRUE)
    )
    
    if (guideStacking == "full"){
        guideStacking <- NA
    }
    
    plotParameters <- list(
        start=start,
        end=end,
        coordinates=coordinates,
        guideSet=guideSet,
        gene=gene,
        geneModel=geneModel,
        guideStacking=guideStacking
    )
    return(plotParameters)
}
