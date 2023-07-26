#' @importFrom DT renderDataTable dataTableOutput
.renderAlignmentsTab <- function(session,
                                 alignments
){
    shiny::req(alignments)
    ns <- session$ns
    
    shiny::renderUI({
        shiny::tagList(
            shiny::uiOutput(ns("alignmentsParameters")),
            shiny::uiOutput(ns("alignmentsSummary")),
            DT::dataTableOutput(ns("alignmentsDataTable")),
            shiny::br(),
            shiny::uiOutput(ns("alignmentsBrowser"))
        )
    })
}







.renderAlignmentsParameters <- function(session,
                                        primaryData,
                                        alignmentsData,
                                        nuclease,
                                        savedParameters
){
    shiny::req(savedParameters)
    ns <- session$ns
    
    shiny::renderUI({
        shiny::tagList(
            shiny::wellPanel(
                class="input inner",
                shiny::fluidRow(
                    shiny::renderUI({
                        mmCount <- grep("^n[0-9]$", colnames(primaryData),
                                        value=TRUE)
                        mmCount <- gsub("n", "", mmCount)
                        if (length(mmCount) == 0){
                            mmCount <- 0
                        } else {
                            mmCount <- max(as.numeric(mmCount))
                        }
                        if (mmCount > 0){
                            shiny::column(
                                width=3,
                                .widgetWrapper(
                                    widget="sliderInput",
                                    ns=ns,
                                    inputId="alignmentsMismatchCount",
                                    label="Max number of off-target mismatches",
                                    min=0,
                                    max=mmCount,
                                    step=1,
                                    value=savedParameters$mmCount,
                                    width="80%"
                                )
                            ) 
                        }
                    }),
                    shiny::renderUI({
                        choices <- c("Anything"="all")
                        if (any(grepl("^n[0-9]_c$", colnames(primaryData)))){
                            choices <- c(choices,
                                         "Coding sequences (CDS) only"="cds")
                        }
                        if (any(grepl("^n[0-9]_p$", colnames(primaryData)))){
                            choices <- c(choices,
                                         "Promoter regions only"="tss")
                        }
                        shiny::column(
                            width=3,
                            .widgetWrapper(
                                widget="radioButtons",
                                ns=ns,
                                inputId="alignmentsTargetRegion",
                                label="Include off-targets that overlap",
                                choices=choices,
                                selected=savedParameters$targetRegion
                            )
                        ) 
                    }),
                    shiny::renderUI({
                        pams <- alignmentsData$pam
                        noncanonicalPams <- .getNoncanonicalPams(nuclease)
                        if (any(pams %in% noncanonicalPams)){
                            shiny::column(
                                width=3,
                                shiny::tagList(
                                    shiny::strong("Include off-targets with"),
                                    .widgetWrapper(
                                        widget="checkboxInput",
                                        ns=ns,
                                        inputId="alignmentsCanonical",
                                        label="Non-canonical PAM sequences",
                                        value=savedParameters$canonicalPam
                                    )
                                )
                            )
                        }
                    }),
                    shiny::renderUI({
                        alignmentScores <- c(
                            "CFD Score"="score_cfd",
                            "MIT Score"="score_mit"
                        )
                        scores <- alignmentScores %in% colnames(alignmentsData)
                        if (any(scores)){
                            alignmentScores <- alignmentScores[scores]
                            shiny::column(
                                width=3,
                                .widgetWrapper(
                                    widget="radioButtons",
                                    ns=ns,
                                    inputId="alignmentsScoreSorting",
                                    label="Sort off-targets by",
                                    choices=alignmentScores,
                                    selected=savedParameters$scoreSorting
                                )
                            )
                        }
                    })
                ),
                ## button to apply settings
                shiny::fluidRow(
                    shiny::column(
                        width=3,
                        .actionButton(
                            inputId=ns("alignmentsOptionsButton"),
                            label="Apply",
                            width="100%"
                        )
                    )
                )
            )
        )
    })
}





.renderAlignmentsSummary <- function(alignments,
                                     primaryData,
                                     mmCount
){
    shiny::req(alignments)
    
    id <- unique(alignments$ID)
    primaryData <- primaryData[primaryData$ID == id, , drop=FALSE]
    
    alignmentType <- alignments$alignmentType
    summaryValues <- rep(NA, 4)
    summaryValues[1] <- sum(grepl("On-target", alignmentType))
    for (i in seq_len(mmCount)){
        primarySummary <- primaryData[[paste0("n", i)]]
        if (length(primarySummary) > 0 && !is.na(primarySummary)){
            pattern <- paste0("(", i, "mm)")
            summaryValues[i+1] <- sum(grepl(pattern, alignmentType))
        }
    }
    mmPanels <- lapply(seq_along(summaryValues), function(x){
        count <- summaryValues[x]
        backgroundColor <- ifelse(is.na(count), "#ddd", "white")
        header <- ifelse(x-1 == 0, "On-target", paste0(x-1, "-mismatch"))
        shiny::column(
            width=3,
            shiny::wellPanel(
                class="mm-panel",
                style=paste("background-color:", backgroundColor),
                shiny::div(class="mm-header", header),
                shiny::div(class="mm-value", count)
            )
        )
    })
    shiny::renderUI({
        shiny::fluidRow(
            mmPanels
        )
    })
}







#' @importFrom crisprBase pamSide
#' @importFrom DT datatable formatStyle styleEqual styleColorBar
.renderAlignmentsDataTable <- function(alnData,
                                       nuclease
){
    shiny::req(alnData)
    
    scoreCols <- grep("score", colnames(alnData), ignore.case=TRUE, value=TRUE)
    
    pamSide <- crisprBase::pamSide(nuclease)
    mismatchCol <- ifelse(pamSide == "5prime",
                          "PAM + Mismatches",
                          "Mismatches + PAM")
    
    if (length(scoreCols) > 0){ # should be dynamic on crisprScore methods
        dtColnames <- c("ID", "Alignment type", mismatchCol,
                        "CFD score", "MIT score",
                        "Genomic context", "PAM site")
    } else {
        dtColnames <- c("ID", "Alignment type", mismatchCol,
                        "Genomic context", "PAM site")
    }
    cleanColnames <- colnames(alnData)
    # colnames(alnData) <- dtColnames
    
    .initiateDatatableTooltips(alnData)
    headerCallback <- .getHeaderCallback(ncol(alnData))
    
    dt <- DT::datatable(
        alnData,
        colnames=dtColnames,
        class="compact cell-border nowrap",
        rownames=NULL,
        filter="none",
        selection="single",
        escape=FALSE,
        extensions="Buttons",
        options=list(
            paging=FALSE,
            scrollX=TRUE,
            scrollY=500,
            scrollCollapse=TRUE,
            searching=FALSE,
            ordering=FALSE,
            dom="lrtBip",
            headerCallback=headerCallback,
            columnDefs=list(
                list(
                    targets=colnames(alnData),
                    className="dt-head-left"
                ),
                list(
                    targets=scoreCols,
                    className="dt-body-right"
                )
            ),
            buttons=list(
                list(
                    extend="csv",
                    text=as.character(
                        shiny::span(
                            shiny::icon("download"), "Download table"
                        )
                    ),
                    exportOptions=list(columns=":visible")
                )
            )
        )
    )
    dt <- DT::formatStyle(
        dt,
        columns=colnames(alnData),
        fontFamily="Courier"
    )
    dt <- DT::formatStyle(
        dt,
        columns=which(cleanColnames == "alignmentType"),
        target="row",
        backgroundColor=DT::styleEqual(
            levels="On-target", values="#d6f3ff", default="white"
        )
    )
    dt <- DT::formatStyle(
        table=dt,
        columns=scoreCols,
        background=styleColorBar(c(0,1), "#3c8dbc33"), # define in utils?
        backgroundSize="100% 90%",
        backgroundRepeat="no-repeat",
        backgroundPosition="center"
    )
    
    # DT::renderDataTable(dt)
    return(dt)
}








## =============================================================================
## Gene browser
## =============================================================================


#' @importFrom waiter waiter_hide
#' @importFrom crisprViz plotGuideSet
.renderAlignmentsBrowser <- function(alignments,
                                     selectedRow,
                                     guideSet,
                                     geneModel
){
    if (nrow(alignments) == 0){
        return()
    }
    if (is.null(selectedRow)){
        ui <- shiny::h4(
            "Select a putative protospacer target in the table above to view",
            "its location in the genome."
        )
        return(ui)
    }
    
    target <- alignments[selectedRow, , drop=FALSE]
    context <- target[["genomicContext"]]
    context <- gsub("<[^>]+>", "", context) # remove HTML artifacts
    if (context == "intergenic (no nearby genes)"){
        ## give message, no plot
        ui <- shiny::wellPanel(
            class="genomic-locus-wellPanel",
            shiny::fluidRow(
                shiny::h4(
                    "The selected target is in an intergenic region with no",
                    "nearby genes (within", format(10000, big.mark=",",), # 10000 should be set somewhere
                    "bases)."
                )
            )
        )
        return(ui)
    }
    ## generate plot:
    shiny::tagList(
        shiny::fluidRow(
            shiny::column(
                width=12,
                shiny::renderPlot({
                    .showWaiter("Generating plot...")
                    params <- .getAlignmentsBrowserPlotParameters(
                        context=context,
                        target=target,
                        guideSet=guideSet,
                        geneModel=geneModel
                    )
                    p <- crisprViz::plotGuideSet(
                        params$gs,
                        geneModel=params$geneModel,
                        targetGene=params$targetGene,
                        from=params$from,
                        to=params$to,
                        extend.left=params$extend,
                        extend.right=params$extend,
                        bsgenome=params$bsgenome
                    )
                    waiter::waiter_hide()
                    return(p)
                })
            )
        )
    )
}
