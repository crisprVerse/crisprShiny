#' @importFrom crisprBase nucleaseName
.getAlignmentParameters_default <- function(primaryData,
                                            nuclease
){
    mmCount <- grep("^n[0-9]$", colnames(primaryData), value=TRUE)
    mmCount <- gsub("n", "", mmCount)
    if (length(mmCount) == 0){
        mmCount <- 0
    } else {
        mmCount <- max(as.numeric(mmCount))
    }
    mmCount <- min(2, mmCount)
    
    scoreSorting <- switch(
        crisprBase::nucleaseName(nuclease),
        "SpCas9"="score_cfd",
        NULL)
    
    list(
        mmCount=mmCount,
        targetRegion="all",
        canonicalPam=TRUE,
        scoreSorting=scoreSorting
    )
}


#' @importFrom methods formalArgs
.getAlignmentParameters <- function(primaryData,
                                    nuclease,
                                    mmCount,
                                    targetRegion,
                                    canonicalPam,
                                    scoreSorting
){
    defaults <- .getAlignmentParameters_default(
        primaryData=primaryData,
        nuclease=nuclease)
    settableParams <- setdiff(
        methods::formalArgs(.getAlignmentParameters),
        methods::formalArgs(.getAlignmentParameters_default)
    )
    for (i in settableParams){
        if (is.null(get(i))){
            assign(i, defaults[[i]])
        }
    }
    
    l <- list(
        mmCount=mmCount,
        targetRegion=targetRegion,
        canonicalPam=canonicalPam,
        scoreSorting=scoreSorting
    )
    return(l)
}








.formatAlignmentsData <- function(alignments,
                                  id,
                                  mmCount,
                                  targetRegion,
                                  canonicalPam,
                                  scoreSorting,
                                  nuclease
                                  
){
    alignments <- alignments[alignments$ID == id, , drop=FALSE]
    # mmCount <- c(0, mmCount)
    
    ## break up into separate functions...?
    
    
    ## filtering and sorting data:
    good <- alignments[["n_mismatches"]] <= mmCount
    alignments <- alignments[good, , drop=FALSE]
    
    if (!is.null(targetRegion)){
        alignments <- switch(
            targetRegion,
            "all"=alignments,
            "cds"=alignments[!is.na(alignments[["cds"]]), , drop=FALSE],
            "tss"=alignments[!is.na(alignments[["promoters"]]), , drop=FALSE]
        )
    }
    
    if (isFALSE(canonicalPam)){
        good <- alignments[["canonical"]]
        alignments <- alignments[good, , drop=FALSE]
    }
    
    if (isTRUE(scoreSorting %in% colnames(alignments))){
        scoreOrder <- order(alignments[[scoreSorting]],
                            -alignments$n_mismatches,
                            decreasing=TRUE)
    } else {
        scoreOrder <- order(-alignments$n_mismatches,
                            decreasing=TRUE)
    }
    alignments <- alignments[scoreOrder,]
    
    
    ## formating data columns
    ID <- alignments[["ID"]]
    alignmentType <- paste0("Off-target (", alignments$n_mismatches, "mm)",
                            recycle0=TRUE)
    alignmentType[alignments$n_mismatches == 0] <- "On-target"
    mismatchesAndPam <- .formatAlignmentMismatches(
        alignments=alignments,
        nuclease=nuclease
    )
    scoreCols <- c("score_cfd", "score_mit")
    scoreValues <- lapply(scoreCols, function(x){
        if (x %in% colnames(alignments)){
            round(alignments[[x]], 3)
        }
    })
    names(scoreValues) <- scoreCols
    
    genomicContext <- .formatAlignmentGenomicContext(
        alnData=alignments
    )
    pamSite <- paste0(alignments$chr, ":",
                      alignments$pam_site,
                      "(", alignments$strand, ")",
                      recycle0=TRUE)
    ## combine into data.frame
    formattedData <- data.frame(
        ID,
        alignmentType,
        mismatchesAndPam
    )
    
    for (i in names(scoreValues)){
        formattedData[[i]] <- scoreValues[[i]]
    }
    formattedData$genomicContext <- genomicContext
    formattedData$pam_site <- pamSite
    
    return(formattedData)
}






#' @importFrom crisprBase pamSide pams
#' @importFrom Biostrings compareStrings matchPattern
#' @importFrom BiocGenerics start
.formatAlignmentMismatches <- function(alignments,
                                       nuclease
){
    pamSide <- crisprBase::pamSide(nuclease)
    
    .getAlignment <- function(spacer,
                              protospacer,
                              pam,
                              pamSide,
                              nuclease
    ){
        stringComp <- Biostrings::compareStrings(spacer, protospacer)
        mms <- Biostrings::matchPattern("?", stringComp)
        mms <- BiocGenerics::start(mms)
        
        seq <- unlist(strsplit(as.character(protospacer), ""))
        seq <- vapply(seq_along(seq), function(x){
            if (x %in% mms){
                if ((pamSide == "3prime" && x > 10) ||
                    (pamSide == "5prime" && x < 10)){ # seed region
                    as.character(
                        shiny::strong(seq[x], style="color: red;")
                    )
                } else {
                    as.character(shiny::strong(seq[x]))
                }
            } else {
                "."
            }
        }, FUN.VALUE=character(1))
        
        seq <- paste0(seq, collapse="")
        canonicalPams <- crisprBase::pams(nuclease, as.character=TRUE)
        if (!as.character(pam) %in% canonicalPams){
            pam <- as.character(
                shiny::span(shiny::strong(as.character(pam)), style="color: goldenrod")
            )
        }
        if (pamSide == "5prime"){
            seq <- paste(pam, seq)
        } else {
            seq <- paste(seq, pam)
        }
        return(seq)
    }
    
    formattedMismatches <- vapply(seq_len(nrow(alignments)), function(x){
        .getAlignment(
            spacer=alignments$spacer[x],
            protospacer=alignments$protospacer[x],
            pam=alignments$pam[x],
            pamSide=pamSide,
            nuclease=nuclease
        )
    }, FUN.VALUE=character(1))
    
    return(formattedMismatches)
}











.formatAlignmentGenomicContext <- function(alnData
){
    .formatContext <- function(target
    ){
        loci <- c("cds", "fiveUTRs", "threeUTRs", "exons", "introns",
                  "promoters", "intergenic")
        hits <- target[loci]
        hits <- unlist(hits)
        validHits <- hits[!is.na(hits) & hits != ""]
        distance <- target[["intergenic_distance"]]
        
        if (length(validHits) == 0 || (!is.na(distance) && distance > 10000)){
            locusSummary <- as.character(
                shiny::div("intergenic (no genes within 10kb)")
            )
        } else if (!is.na(target[["intergenic"]])){
            locusSummary <- as.character(
                shiny::div(
                    paste0("intergenic (",
                           target[["intergenic"]],
                           ", ", distance, "bp away)")
                )
            )
        } else {
            locusSummary <- ""
            for (i in loci[-length(loci)]){
                if (!is.na(hits[i])){
                    locus <- unlist(strsplit(hits[i], ";"))
                    locus <- locus[locus != ""]
                    locus <- vapply(locus, function(x){
                        as.character(shiny::div(paste(i, x)))
                    }, FUN.VALUE=character(1))
                    locusSummary <- paste(locusSummary,
                                          paste(locus, collapse=" "))
                }
            }
        }
        return(locusSummary)
    }
    
    formattedGenomicContext <- vapply(seq_len(nrow(alnData)), function(x){
        .formatContext(alnData[x,])
    }, FUN.VALUE=character(1))
    
    return(formattedGenomicContext)
}












#' @importFrom crisprDesign crisprNuclease bsgenome queryTxObject
#' @importFrom BSgenome commonName
#' @importClassesFrom crisprDesign GuideSet
#' @importFrom crisprBase spacerLength pamLength getProtospacerRanges
#' @importFrom GenomeInfoDb genome genome<-
#' @importFrom BiocGenerics start end
.getAlignmentsBrowserPlotParameters <- function(context,
                                                target,
                                                guideSet,
                                                geneModel
){
    
    if (grepl("intergenic", context)){
        targetGene <- gsub("(^.+\\()|(,.+$)", "", context)
    } else {
        targetGene <- unlist(strsplit(context, split="\\s+"))
        targetGene <- targetGene[targetGene != ""]
        regions <- c("cds", "exons", "fiveUTRs", "threeUTRs", "introns",
                     "promoters")
        targetGene <- setdiff(targetGene, regions)
    }
    
    pamSite <- target[["pam_site"]]
    pamSite <- unlist(strsplit(pamSite, split="[:\\(\\)]"))
    nuclease <- crisprDesign::crisprNuclease(guideSet)
    bsgenome <- crisprDesign::bsgenome(guideSet)
    
    ## create GuideSet for putative target, with placeholder sequences
    gs <- crisprDesign::GuideSet(
        protospacers=paste0(rep("A", crisprBase::spacerLength(nuclease)), collapse=""),
        pams=paste0(rep("A", crisprBase::pamLength(nuclease)), collapse=""),
        seqnames=pamSite[1],
        pam_site=as.numeric(pamSite[2]),
        strand=pamSite[3],
        CrisprNuclease=nuclease,
        bsgenome=bsgenome,
        ids='spacer'
    )
    GenomeInfoDb::genome(gs) <- unique(GenomeInfoDb::genome(bsgenome))
    names(gs) <- "spacer"
    
    from <- NULL
    to <- NULL
    if (!is.null(geneModel)){
        targetWindow <- crisprDesign::queryTxObject(
            txObject=geneModel,
            featureType = "transcripts",
            queryColumn="gene_symbol",
            queryValue=targetGene
        )
        from <- min(BiocGenerics::start(targetWindow))
        to <- max(BiocGenerics::end(targetWindow))
    }
    from <- min(
        from,
        BiocGenerics::start(
            crisprBase::getProtospacerRanges(gs, nuclease=nuclease)
        )
    )
    to <- max(
        to,
        BiocGenerics::end(
            crisprBase::getProtospacerRanges(gs,nuclease=nuclease)
        )
    )
    extend <- round(0.1 * (to - from))
    
    params <- list(
        gs=gs,
        geneModel=geneModel,
        targetGene=targetGene,
        from=from,
        to=to,
        extend=extend,
        bsgenome=bsgenome
    )
    return(params)
}
