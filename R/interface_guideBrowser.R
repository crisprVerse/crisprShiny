#' @importFrom S4Vectors mcols
.guideBrowserUI <- function(session
){
    ns <- session$ns
    
    shiny::tagList(
        shiny::wellPanel(
            class="empty",
            shiny::wellPanel(
                class="input",
                shiny::fluidRow(
                    shiny::column(
                        width=12,
                        shiny::h4(
                            "Select up to 20 gRNAs in the on-targets table to",
                            "visualize. All gRNAs must target the same chromosome."
                        )
                    )
                ),
                shiny::br(),
                shiny::fluidRow(
                    shiny::column(
                        width=3,
                        .widgetWrapper(
                            widget="selectizeInput",
                            ns=ns,
                            inputId="guideBrowserGene",
                            label="Gene to view on-targets",
                            choices=character(0),
                            multiple=FALSE,
                            width="100%"
                        )
                    ),
                    shiny::column(
                        width=3,
                        .widgetWrapper(
                            widget="radioButtons",
                            ns=ns,
                            inputId="guideBrowserWindow",
                            label="Viewing range",
                            choiceNames=c("Show full gene",
                                          "Zoom in on gRNAs"),
                            choiceValues=c("full", "zoom"),
                            selected="full"
                        )
                    ),
                    shiny::column(
                        width=3,
                        .widgetWrapper(
                            widget="radioButtons",
                            ns=ns,
                            inputId="guideBrowserStacking",
                            label="On-targets track display",
                            choiceNames=c("Each gRNA on separate line (full)",
                                          "gRNAs share line where they fit (squish)",
                                          "All gRNAs overlap on single line (dense)"),
                            choiceValues=c("full", "squish", "dense"),
                            selected="full"
                        )
                    ),
                    shiny::column(
                        width=3,
                        align="center",
                        shiny::actionButton(
                            inputId=ns("guideBrowserButton"),
                            label="Vizualize on-targets",
                            width="75%"
                        )
                    )
                )
            ),
            shiny::uiOutput(ns("guideBrowserPlot"))
        )
    )
}








#' @importFrom waiter waiter_hide
#' @importFrom crisprDesign queryTxObject
#' @importFrom crisprViz plotGuideSet
.renderGuideBrowserPlotAndCoords <- function(guideSet,
                                             gene,
                                             geneModel,
                                             selectedRows,
                                             plotWindow,
                                             guideStacking
){
    .showWaiter("Generating plot...")
    params <- .getGuideBrowserParameters(
        guideSet=guideSet,
        gene=gene,
        geneModel=geneModel,
        selectedRows=selectedRows,
        plotWindow=plotWindow,
        guideStacking=guideStacking
    )
    
    if ("error" %in% names(params)){
        errorUI <- .renderBrowserErrorMessage(params)
        waiter::waiter_hide()
        return(errorUI)
    }
    
    # get length of guideSet + number of gene tracks in genemodel
    if (length(params$geneModel) > 0){
        nTracks <- crisprDesign::queryTxObject(
            params$geneModel,
            featureType="transcripts",
            queryColumn="gene_symbol",
            queryValue=params$gene
        )
        nTracks <- length(nTracks)
    } else {
        nTracks <- 0
    }
    if (isTRUE(params$guideStacking == "dense")){
        nTracks <- nTracks + 1
    } else {
        nTracks <- nTracks + length(params$guideSet)
    }
    nTracks <- 50 * nTracks
    
    shiny::tagList(
        shiny::h4(
            shiny::strong("Genomic coordinates:"),
            shiny::HTML("&emsp;"),
            shiny::span(style="font-family: monospace;", params$coordinates)
        ),
        shiny::div(
            shiny::renderPlot({
                p <- crisprViz::plotGuideSet(
                    params$guideSet,
                    geneModel=params$geneModel,
                    targetGene=params$gene,
                    guideStacking=params$guideStacking,
                    from=params$start,
                    to=params$end
                )
                waiter::waiter_hide()
                return(p)
            },
            height=nTracks
            )
        )
    )
}







.renderBrowserErrorMessage <- function(params
){
    errorMessage <- switch(
        params[["error"]],
        "index"=paste("Select gRNAs targeting your chosen gene in the",
                      "table of on-targets above to view."),
        "chr"=paste("All selected gRNAs must target the same chromosome."),
        "render"=paste("Too many gRNAs to render in the plot.",
                       "Please include fewer gRNAs in your selection.")
    )
    errorUI <- shiny::tagList(
        shiny::br(),
        shiny::br(),
        shiny::fluidRow(
            shiny::column(
                width=12,
                align="center",
                shiny::h4(class="error-text", errorMessage)
            )
        )
    )
    return(errorUI)
}
