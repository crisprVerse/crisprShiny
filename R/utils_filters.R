#' @importFrom S4Vectors mcols
.getFilterDefaults <- function(guideSet,
                               useFilterPresets
){
    alnCols <- grep("^n[0-9](_[cp])?$",
                    colnames(S4Vectors::mcols(guideSet)),
                    value=TRUE)
    scoreCols <- grep("^score_",
                      colnames(S4Vectors::mcols(guideSet)),
                      value=TRUE)
    scoreCols <- setdiff(scoreCols,
                         grep("score_conservation", scoreCols, value=TRUE))
    ## defaults should be most permissive
    defaults <- list(
        polyT=FALSE,
        percentGC=c(0, 100),
        canonicalPam=TRUE,
        conservation=FALSE,
        excludeNaScore=FALSE,
        inRepeats=FALSE,
        hasSNP=FALSE,
        pfam=FALSE,
        isoforms="",
        percentCDS=c(0, 100),
        percentCodingIsoforms=0,
        promoter="",
        distRange=c(-500, 500),
        distToTss=c(-500, 500)
    )
    for (i in alnCols){
        defaults[[i]] <- NA
    }
    for (i in scoreCols){
        defaults[[i]] <- 0
    }
    if (useFilterPresets){
        filterPresets <- list(
            excludeNaScore=TRUE,
            score_deepspcas9=0.5,
            score_deephf=0.5,
            polyT=TRUE,
            percentGC=c(20, 80),
            inRepeats=TRUE,
            hasSNP=TRUE,
            percentCDS=c(0, 85)
            # distRange=switch(          # should modality be incorporated?
            #     query$parameters$modality,
            #     "crispra"=c(-500, 0),
            #     "crispri"=c(0, 500),
            #     NULL)
        )
        for (i in seq_along(filterPresets)){
            name <- names(filterPresets)[i]
            value <- filterPresets[[i]]
            defaults[[name]] <- value
        }
    }
    
    return(defaults)
}







.setFilterValues <- function(defaults,
                             input
){
    filterValues <- defaults
    for (i in seq_along(filterValues)){
        filterName <- names(filterValues)[i]
        inputName <- paste0("filter_", filterName)
        # if (exists(inputName, where=input)){
        if (inputName %in% names(input)){
            filterValues[[filterName]] <- input[[inputName]]
        }
    }
    return(filterValues)
}







## look into breaking up function by section and applying filters iteratively

#' @importFrom crisprDesign crisprNuclease geneAnnotation tssAnnotation
#' @importFrom S4Vectors mcols
#' @importFrom crisprBase pams
.applyGuideSetFilters <- function(guideSet,
                                  filterValues
){
    nuclease <- crisprDesign::crisprNuclease(guideSet)
    mcolData <- S4Vectors::mcols(guideSet)
    
    names(mcolData$score_cfd) <- NULL # temp
    names(mcolData$score_mit) <- NULL # temp
    
    ## filter nucleotide content
    filter <- TRUE
    if (filterValues$polyT && "polyT" %in% colnames(mcolData)){
        filter <- filter & !mcolData$polyT
    }
    if ("percentGC" %in% colnames(mcolData)){
        filter <- filter & (mcolData$percentGC >= filterValues$percentGC[1]) &
            (mcolData$percentGC <= filterValues$percentGC[2])
    }
    if (isFALSE(filterValues$canonicalPam)){ # pams always present in GuideSet
        canonicalPams <- crisprBase::pams(nuclease, as.character=TRUE)
        filter <- filter & (as.character(mcolData$pam) %in% canonicalPams)
    }
    ## filter alignments
    alnCols <- grep("^n[0-9](_[cp])?$", colnames(mcolData), value=TRUE)
    for (i in alnCols){
        if (!is.na(filterValues[[i]])){
            filter <- filter &
                !is.na(mcolData[[i]]) &
                mcolData[[i]] <= filterValues[[i]]
        }
    }
    ## filter scores
    scoreCols <- grep("^score_", colnames(mcolData), value=TRUE)
    scoringMethods <- crisprScore::scoringMethodsInfo$method
    scoringMethods <- paste0("score_", scoringMethods)
    scoreCols <- intersect(scoreCols, scoringMethods)
    if ("score_conservation" %in% colnames(mcolData) &&
        !"score_conservation_binary" %in% colnames(mcolData)){ # does this ever run?
        mcolData[["score_conservation_binary"]] <-
            mcolData[["score_conservation"]] >= 1
    }
    ## TEMPORARY FIX ===========================================================
    ignoredScores <- "score_ruleset3" # has scores outside of [0,1]
    scoreCols <- setdiff(scoreCols, ignoredScores)
    ## END TEMPORARY FIX =======================================================
    if ("score_conservation_binary" %in% colnames(mcolData) &&
        filterValues$conservation){
        filter <- filter & mcolData$score_conservation_binary
    }
    for (i in scoreCols){
        if (!filterValues$excludeNaScore){
            scoreFilter <- is.na(mcolData[[i]]) |
                mcolData[[i]] >= filterValues[[i]]
        } else {
            scoreFilter <- !is.na(mcolData[[i]]) &
                mcolData[[i]] >= filterValues[[i]]
        }
        ## minor issue with score_cfd and score_mit, which are named vectors
        filter <- filter & scoreFilter
    }
    ## filter genomic features
    if (filterValues$inRepeats && "inRepeats" %in% colnames(mcolData)){
        filter <- filter & !mcolData$inRepeats
    }
    if (filterValues$hasSNP && "hasSNP" %in% colnames(mcolData)){
        filter <- filter & !mcolData$hasSNP
    }
    geneAnn <- crisprDesign::geneAnnotation(guideSet)
    if (!is.null(geneAnn) &&
        "pfam" %in% colnames(geneAnn) &&
        filterValues$pfam){
        geneAnnFilter <- !is.na(geneAnn$pfam)
        ids <- rownames(geneAnn)[geneAnnFilter]
        filter <- filter & names(guideSet) %in% ids
    }
    ## filter isoform
    geneAnn <- crisprDesign::geneAnnotation(guideSet)
    if (length(filterValues$isoforms) > 0 &&
        nzchar(filterValues$isoforms) &&
        !is.null(geneAnn)){
        geneAnnFilter <- geneAnn$tx_id == filterValues$isoforms
        if ("percentCDS" %in% colnames(geneAnn)){ # contingent on isoforms
            geneAnnFilter <- geneAnnFilter &
                !is.na(geneAnn$percentCDS) &
                geneAnn$percentCDS >= filterValues$percentCDS[1] &
                geneAnn$percentCDS <= filterValues$percentCDS[2]
        }
        ids <- rownames(geneAnn)[geneAnnFilter]
        filter <- filter & names(guideSet) %in% ids
    }
    if (length(filterValues$isoforms) > 0 &&
        nzchar(filterValues$isoforms) &&
        !is.null(geneAnn) &&
        "percentCodingIsoforms" %in% colnames(geneAnn)){
        geneAnnFilter <- geneAnn$percentCodingIsoforms >=
            filterValues$percentCodingIsoforms
        ids <- rownames(geneAnn)[geneAnnFilter]
        filter <- filter & names(guideSet) %in% ids
    }
    ## filter promoter
    tssAnn <- crisprDesign::tssAnnotation(guideSet)
    if (length(filterValues$promoter) > 0 &&
        nzchar(filterValues$promoter) &&
        !is.null(tssAnn)){
        tssAnnFilter <- tssAnn$tss_id == filterValues$promoter
        if ("dist_to_tss" %in% colnames(tssAnn)){
            tssAnnFilter <- tssAnnFilter &
                !is.na(tssAnn$dist_to_tss) & # is this possible?
                tssAnn$dist_to_tss >= filterValues$distToTss[1] &
                tssAnn$dist_to_tss <= filterValues$distToTss[2]
        }
        ids <- rownames(tssAnn)[tssAnnFilter]
        filter <- filter & names(guideSet) %in% ids
    }
    ## filter out guides with unresolved NAs
    filter[is.na(filter)] <- FALSE
    guideSet <- guideSet[filter]
    return(guideSet)
}
