.guideSetFilters <- function(session,
                             guideSet,
                             savedValues
){
    ns <- session$ns
    
    .filterTemplate <- function(title,
                                ui
    ){
        shiny::wellPanel(
            class="filter-well",
            shiny::h4(title, class="filter-well-title"),
            ui
        )
    }
    
    shiny::modalDialog(
        title=shiny::h3("Filter on-targets"),
        size="l",
        shiny::div(
            class="filter-tabpanel",
            shiny::tabsetPanel(
                .guideSetFilters_nucleotideContent(
                    session=session,
                    guideSet=guideSet,
                    savedValues=savedValues
                ),
                .guideSetFilters_alignments(
                    session=session,
                    guideSet=guideSet,
                    savedValues=savedValues
                ),
                .guideSetFilters_scores(
                    session=session,
                    guideSet=guideSet,
                    savedValues=savedValues
                ),
                .guideSetFilters_genomicFeatures(
                    session=session,
                    guideSet=guideSet,
                    savedValues=savedValues
                ),
                .guideSetFilters_isoforms(
                    session=session,
                    guideSet=guideSet,
                    savedValues=savedValues
                ),
                .guideSetFilters_promoter(
                    session=session,
                    guideSet=guideSet,
                    savedValues=savedValues
                )
            )
        ),
        footer=shiny::fluidRow(
            shiny::column(
                width=4,
                align="left",
                shiny::actionButton(
                    ns("filterButton_confirm"),
                    label="Apply",
                    width="80%"
                )
            ),
            shiny::column(
                width=4,
                align="center",
                shiny::actionButton(
                    ns("filterButton_reset"),
                    label="Reset",
                    width="80%"
                )
            ),
            shiny::column(
                width=4,
                align="right",
                shiny::actionButton(
                    ns("filterButton_cancel"),
                    label="Cancel",
                    width="80%"
                )
            )
        )
    )
}





#' @importFrom S4Vectors mcols
#' @importFrom crisprDesign crisprNuclease pams
.guideSetFilters_nucleotideContent <- function(session,
                                               guideSet,
                                               savedValues
){
    ns <- session$ns
    savedValues     # need to observe outside of renderUI
    
    requiredCols <- c("polyT", "percentGC")
    hasRequiredCols <- any(requiredCols %in% colnames(S4Vectors::mcols(guideSet)))
    nuclease <- crisprDesign::crisprNuclease(guideSet)
    noncanonical <- .getNoncanonicalPams(nuclease)
    hasNoncanonicalPams <- any(crisprDesign::pams(guideSet, as.character=TRUE) %in% noncanonical)
    if (!hasRequiredCols && !hasNoncanonicalPams){
        return(NULL)
    }
    
    shiny::tabPanel(
        title="Nucleotide content",
        shiny::fluidRow(
            shiny::renderUI({
                if ("polyT" %in% colnames(S4Vectors::mcols(guideSet))){
                    shiny::column(
                        width=4,
                        .widgetWrapper(
                            "checkboxInput",
                            ns=ns,
                            inputId="filter_polyT",
                            label=shiny::strong("Exclude spacers with poly-T",
                                                "(termination signal)?"),
                            value=savedValues$polyT
                        )
                    )
                }
            }),
            shiny::renderUI({
                if ("percentGC" %in% colnames(S4Vectors::mcols(guideSet))){
                    shiny::column(
                        width=4,
                        .widgetWrapper(
                            "sliderInput",
                            ns=ns,
                            inputId="filter_percentGC",
                            label="Spacer percent GC content",
                            min=0,
                            max=100,
                            value=savedValues$percentGC,
                            step=1,
                            post="%"
                        )
                    )
                }
            }),
            shiny::renderUI({
                if (hasNoncanonicalPams){
                    shiny::column(
                        width=4,
                        .widgetWrapper(
                            "checkboxInput",
                            ns=ns,
                            inputId="filter_canonicalPam",
                            label="Include spacers with non-canonical PAMs?",
                            value=savedValues$canonicalPam
                        )
                    )
                }
            })
        )
    )
}




#' @importFrom S4Vectors mcols
.guideSetFilters_alignments <- function(session,
                                        guideSet,
                                        savedValues
){
    ns <- session$ns
    savedValues # needs to be observed outside of renderUI
    
    alnCols <- grep("^n[0-9](_[cp])?$",
                    colnames(S4Vectors::mcols(guideSet)),
                    value=TRUE)
    alnTotal <- grep("^n[0-9]$", alnCols, value=TRUE)
    alnCds <- grep("^n[0-9]_c$", alnCols, value=TRUE)
    alnTss <- grep("^n[0-9]_p$", alnCols, value=TRUE)
    
    .renderAlignmentFilterInputs <- function(alnCols,
                                             pattern,
                                             labelPrefix
    ){
        lapply(alnCols, function(x){
            id <- paste0("filter_", x)
            count <- as.numeric(gsub(pattern, "", x))
            label <- paste(labelPrefix, count, .plural("mismatch", count))
            .widgetWrapper(
                "numericInput",
                ns=ns,
                inputId=id,
                label=label,
                min=0,
                value=savedValues[[x]],
                step=1
            )
        })
    }
    
    if (length(alnCols) > 0){
        shiny::tabPanel(
            title="Off-target count",
            shiny::fluidRow(
                shiny::column(
                    width=4,
                    .renderAlignmentFilterInputs(
                        alnCols=alnTotal,
                        pattern="n",
                        labelPrefix="Max total alignments with"
                    )
                ),
                shiny::renderUI({
                    if (length(alnCds) > 0){
                        shiny::column(
                            width=4,
                            .renderAlignmentFilterInputs(
                                alnCols=alnCds,
                                pattern="n|_c",
                                labelPrefix="Max alignments in CDS with"
                            )
                        )
                    }
                }),
                shiny::renderUI({
                    if (length(alnTss) > 0){
                        shiny::column(
                            width=4,
                            .renderAlignmentFilterInputs(
                                alnCols=alnTss,
                                pattern="n|_p",
                                labelPrefix="Max alignments in promoters with"
                            )
                        )
                    }
                })
            )
        )
    }
}




#' @importFrom S4Vectors mcols
.guideSetFilters_scores <- function(session,
                                    guideSet,
                                    savedValues
){
    ns <- session$ns
    
    scoringMethods <- crisprScore::scoringMethodsInfo
    scoringMethods$method <- paste0("score_", scoringMethods$method)
    guideSetScores <- grep("^score_",
                           colnames(S4Vectors::mcols(guideSet)),
                           value=TRUE)
    ##### TEMPORARY ============================================================
    ignoredScores <- "score_ruleset3"
    guideSetScores <- setdiff(guideSetScores, ignoredScores)
    ##### TEMPORARY ============================================================
    overlapScores <- scoringMethods$method %in% guideSetScores
    scoringMethods <- scoringMethods[overlapScores, , drop=FALSE]
    
    hasConservationScores <- any(grepl("score_conservation", guideSetScores))
    hasScores <- any(overlapScores) || hasConservationScores
    
    .renderScoreFilterInputs <- function(scoreType,
                                         sectionTitle
                                         
    ){
        indices <- scoringMethods$type == scoreType
        scoreMethods <- scoringMethods$method[indices]
        scoreLabels <- scoringMethods$label[indices]
        ui <- lapply(seq_along(scoreMethods), function(x){
            method <- scoreMethods[x]
            id <- paste0("filter_", method)
            label <- scoreLabels[x]
            .widgetWrapper(
                "sliderInput",
                ns=ns,
                inputId=id,
                label=paste("Minimum", label, "score"),
                width="100%",
                min=0,
                max=1,
                step=0.01,
                value=savedValues[[method]]
            )
        })
        colStyle <- "padding-left: 20px; padding-right: 20px;"
        if (length(ui) > 0){
            shiny::column(
                width=6,
                style=colStyle, # make class
                shiny::h5(sectionTitle,
                          style=paste("text-align: center;", # make class
                                      "font-weight: bold;",
                                      "font-size: 17px;")),
                ui
            )
        }
    }
    
    if (hasScores){ # if scores are NA, if scores are negative...
        shiny::tabPanel(
            title="Scores",
            shiny::renderUI({
                shiny::fluidRow(
                    shiny::column(
                        width=4,
                        .widgetWrapper(
                            "checkboxInput",
                            ns=ns,
                            inputId="filter_excludeNaScore",
                            label=shiny::strong(
                                "Exclude gRNAs missing values for any score method?",
                                "(i.e. has NA values)"
                            ),
                            value=savedValues$excludeNaScore
                        )
                    ),
                    shiny::renderUI({
                        if (hasConservationScores){
                            shiny::column(
                                width=4,
                                .widgetWrapper(
                                    "checkboxInput",
                                    ns=ns,
                                    inputId="filter_conservation",
                                    label=shiny::strong(
                                        "Require gRNA spacers to target conserved regions?"
                                    ),
                                    value=savedValues$conservation
                                )
                            )
                        }
                    })
                )
            }),
            shiny::fluidRow(
                shiny::renderUI({
                    .renderScoreFilterInputs(
                        scoreType="On-target",
                        sectionTitle="On-target activity scores"
                    )
                }),
                shiny::renderUI({
                    .renderScoreFilterInputs(
                        scoreType="Off-target",
                        sectionTitle="Off-target specificity scores"
                    )
                })
            )
        )
    }
}





#' @importFrom S4Vectors mcols
#' @importFrom crisprDesign geneAnnotation
.guideSetFilters_genomicFeatures <- function(session,
                                             guideSet,
                                             savedValues
){
    ns <- session$ns
    savedValues  # need to observe outside of renderUI
    
    requiredCols <- c("inRepeats", "hasSNP")
    hasRequiredCols <- any(
        requiredCols %in% colnames(S4Vectors::mcols(guideSet))
    )
    requiredGeneAnnCols <- c("pfam")
    hasRequiredGeneAnnCols <- any(
        requiredCols %in% colnames(crisprDesign::geneAnnotation(guideSet))
    )
    
    if (hasRequiredCols || hasRequiredGeneAnnCols){
        shiny::tabPanel(
            title="Genomic features",
            shiny::fluidRow(
                shiny::renderUI({
                    if ("inRepeats" %in% colnames(S4Vectors::mcols(guideSet))){
                        shiny::column(
                            width=4,
                            .widgetWrapper(
                                "checkboxInput",
                                ns=ns,
                                inputId="filter_inRepeats",
                                label=shiny::strong(
                                    paste("Exclude gRNAs targeting repeat elements",
                                          "(recommended)")
                                ),
                                value=savedValues$inRepeats
                            )
                        )
                    }
                }),
                shiny::renderUI({
                    if ("hasSNP" %in% colnames(S4Vectors::mcols(guideSet))){
                        shiny::column(
                            width=4,
                            .widgetWrapper(
                                "checkboxInput",
                                ns=ns,
                                inputId="filter_hasSNP",
                                label=shiny::strong(
                                    paste("Exclude gRNAs overlapping common SNPs",
                                          "(recommended)")
                                ),
                                value=savedValues$hasSNP
                            )
                        )
                    }
                }),
                shiny::renderUI({
                    geneAnn <- crisprDesign::geneAnnotation(guideSet)
                    if ("pfam" %in% colnames(geneAnn)){
                        shiny::column(
                            width=4,
                            .widgetWrapper(
                                "checkboxInput",
                                ns=ns,
                                inputId="filter_pfam",
                                label=shiny::strong(
                                    "Require gRNA spacers to target Pfam domains?"
                                ),
                                value=savedValues$pfam
                            )
                        )
                    }
                })
            )
        )
    }
}











#' @importFrom crisprDesign geneAnnotation bsgenome
#' @importFrom GenomeInfoDb commonName
#' @importFrom utils data
.guideSetFilters_isoforms <- function(session,
                                      guideSet,
                                      savedValues
){
    ns <- session$ns
    
    geneAnn <- crisprDesign::geneAnnotation(guideSet)
    if (is.null(geneAnn)){
        return()
    }
    isoform_df <- data.frame(tx_id=geneAnn$tx_id,
                             gene_symbol=vapply(geneAnn$gene_symbol, function(x){
                                 if (x != ""){
                                     x <- paste0("(", x, ")")
                                 }
                                 x
                             }, FUN.VALUE=character(1))
    )
    isoform_df$gene_symbol <- paste(isoform_df$tx_id, isoform_df$gene_symbol)
    isoform_df <- unique(isoform_df)
    isoform_choices <- isoform_df$tx_id
    names(isoform_choices) <- isoform_df$gene_symbol
    ## add identifiers for canonical isoforms
    bsgenome <- crisprDesign::bsgenome(guideSet)
    species <- tolower(GenomeInfoDb::commonName(bsgenome))
    ## user should provide this optional information
    # if (species %in% c("human", "mouse")){
    #     canonicalTxs <- switch(
    #         species,
    #         'human'=utils::data(canonicalHuman, package="crisprDesignData"),
    #         'mouse'=utils::data(canonicalMouse, package="crisprDesignData")
    #     )
    #     isCanonical <- which(isoform_choices %in% get(canonicalTxs)$tx_id)
    #     names(isoform_choices)[isCanonical] <- paste(
    #         names(isoform_choices)[isCanonical], "(canonical)"
    #     )
    # }
    isoform_choices <- sort(isoform_choices)
    isoform_choices <- c("Select isoform"="", isoform_choices)
    
    shiny::tabPanel(
        title="Isoform-specific parameters",
        shiny::fluidRow(
            shiny::column(
                width=4,
                .widgetWrapper(
                    "selectizeInput",
                    ns=ns,
                    inputId="filter_isoforms",
                    label="Require spacer to target specific isoform",
                    choices=isoform_choices,
                    selected=savedValues$isoforms,
                    multiple=FALSE,
                    width="100%"
                ),
                shiny::conditionalPanel(
                    condition="input.filter_isoforms != ''",
                    ns=ns,
                    shiny::renderUI({
                        .widgetWrapper(
                            "sliderInput",
                            ns=ns,
                            inputId="filter_percentCDS",
                            label=paste0("Target position in the coding sequence ",
                                         "relative to 5", SYMBOLS$PRIME, " end"),
                            min=0,
                            max=100,
                            value=savedValues$percentCDS,
                            step=1,
                            post="%"
                        )
                    })
                )
            ),
            shiny::column(
                width=8,
                shiny::fluidRow(
                    shiny::column(
                        width=6,
                        .widgetWrapper(
                            "sliderInput",
                            ns=ns,
                            inputId="filter_percentCodingIsoforms",
                            label="Minimum percent coding isoforms targeted",
                            min=0,
                            max=100,
                            value=savedValues$percentCodingIsoforms,
                            step=1,
                            post="%"
                        )
                    )
                )
            )
        )
    )
}





#' @importFrom crisprDesign tssAnnotation
.guideSetFilters_promoter <- function(session,
                                      guideSet,
                                      savedValues
){
    ns <- session$ns
    
    tssAnn <- crisprDesign::tssAnnotation(guideSet)
    if (is.null(tssAnn)){
        return()
    }
    
    promoter <- sort(unique(tssAnn$tss_id))
    promoter <- c("Select promoter"="", promoter)
    
    shiny::tabPanel(
        title="Promoter targeting parameters",
        shiny::fluidRow(
            shiny::column(
                width=4,
                .widgetWrapper(
                    "selectizeInput",
                    ns=ns,
                    inputId="filter_promoter",
                    label="Require spacer to target specific promoter",
                    choices=promoter,
                    selected=savedValues$promoter,
                    multiple=FALSE,
                    width="100%"
                )
            ),
            shiny::column(
                width=4,
                .widgetWrapper(
                    "sliderInput",
                    ns=ns,
                    inputId="filter_distToTss",
                    label="Distance to TSS",
                    min=min(savedValues$distRange),
                    max=max(savedValues$distRange),
                    value=savedValues$distToTss,
                    step=1
                )
            )
        )
    )
}
