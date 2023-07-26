#' @importFrom crisprBase pamSide nucleaseName pams
.formatOnTargets <- function(data, # break up function
                             nuclease
){
    ## only work off of expected input (using examples)
    
    ## remove unnecessary columns
    data[["spacer"]] <- NULL
    data[["start"]] <- NULL # keep?
    data[["end"]] <- NULL
    # data[["score_conservation"]] <- NULL
    
    ## sequence columns
    if (crisprBase::pamSide(nuclease) == "3prime"){
        spacerInfo <- c("protospacer", "pam")
    } else {
        spacerInfo <- c("pam", "protospacer")
    }
    
    data <- .formatOnTargetsData(
        data=data,
        nuclease=nuclease
    )
    
    ## coordinate columns
    coordinateInfo <- c("chr", "pam_site", "strand")
    
    ## gene target columns
    geneInfo <- c("gene_symbol", "gene_id")
    
    ## score columns (handle off-target scores for noncanonical PAMs)
    scoreInfo <- grep("^score_", colnames(data), value=TRUE)
    if (crisprBase::nucleaseName(nuclease) == "SpCas9"){
        canonicalPams <- crisprBase::pams(nuclease, as.character=TRUE)
        noncanonical <- !data[["pam"]] %in% canonicalPams
        if ("score_cfd" %in% scoreInfo){
            ## moves CFD score to end
            scoreInfo <- c(setdiff(scoreInfo, "score_cfd"), "score_cfd")
            data[["score_cfd"]][noncanonical] <- NA
        }
        if ("score_mit" %in% scoreInfo){
            ## moves MIT score to end
            scoreInfo <- c(setdiff(scoreInfo, "score_mit"), "score_mit")
            data[["score_mit"]][noncanonical] <- NA
        }
    }
    for (i in scoreInfo){
        data[[i]] <- format(round(data[[i]], 3), nsmall=3)
    }
    
    
    ## configure alignment summary columns
    alnSummaryInfo <- grep('^n[0-3](_[cp])?$', colnames(data), value=TRUE)
    
    
    ## configure other annotation columns
    otherAnnotationInfo <- c(
        "inRepeats", "percentGC", "polyA", "polyC", "polyG", "polyT",
        "startingGGGGG", "hasSNP", "NNGG", "barcode", "cut_site", "comment"
    )
    
    
    ## order columns; add remaining columns
    colOrder <- c("ID", "group", "rank", spacerInfo, coordinateInfo, geneInfo,
                  scoreInfo, alnSummaryInfo, otherAnnotationInfo)
    colOrder <- intersect(colOrder, colnames(data))
    colOrder <- c(colOrder, setdiff(colnames(data), colOrder))
    data <- data[, colOrder]
    if ("rank" %in% colnames(data)){
        data[order(data[["rank"]]),]
    }
    return(data)
}




#' @importFrom crisprBase pams
.formatOnTargetsData <- function(data,
                                 nuclease
){
    canonicalPams <- crisprBase::pams(nuclease, as.character=TRUE)
    data[["pam"]] <- vapply(data[["pam"]], function(x){
        if (!x %in% canonicalPams){
            x <- as.character(
                shiny::strong(style="color: goldenrod;", x) # or apply attr
            )
        }
        x
    }, FUN.VALUE=character(1))
    return(data)
}
