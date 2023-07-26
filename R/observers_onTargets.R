#' @importFrom DT datatable formatStyle styleEqual styleInterval 
.onTargetsDataTable <- function(data,
                                nuclease
){
    ## clean, have subfunctions where necessary
    
    shiny::req(data)
    
    scoreMethods <- crisprScore::scoringMethodsInfo$method
    
    data <- .formatOnTargets(
        data=data,
        nuclease=nuclease
    )
    
    ## get original, unmodified column names
    cleanColnames <- colnames(data)
    ## temporary
    if ("score_cfd" %in% colnames(data)){
        colnames(data)[colnames(data) == "score_cfd"] <- "score_cfd_aggregate"
    }
    if ("score_mit" %in% colnames(data)){
        colnames(data)[colnames(data) == "score_mit"] <- "score_mit_aggregate"
    }
    ## column UI formatting
    logicalCols <- c("polyA", "polyC", "polyG", "polyT", "startingGGGGG",
                     "inRepeats", "hasSNP")
    logicalCols <- which(colnames(data) %in% logicalCols)
    alnSummaryCols <- grep('^n[0-3](_[cp])?$', colnames(data))
    scoreCols <- c(grep("^score_", colnames(data), value=TRUE),
                   intersect(colnames(data), scoreMethods))
    
    .initiateDatatableTooltips(data)
    headerCallback <- .getHeaderCallback(ncol(data))
    
    dt <- DT::datatable(
        data,
        colnames=cleanColnames,
        class="compact cell-border nowrap",
        rownames=NULL,
        filter="none", # revisit
        selection="multiple",
        escape=FALSE,
        extensions=c("Buttons", "FixedColumns"),
        options=list(
            paging=TRUE,
            scrollX=TRUE, 
            searching=FALSE,
            ordering=TRUE,
            dom="lrtBip",
            pageLength=25,
            headerCallback=headerCallback,
            fixedColumns=list(
                leftColumns=1
            ),
            columnDefs=list(
                list(
                    targets=c("ID", "protospacer", "pam"),
                    orderable=FALSE
                ),
                list(
                    targets=scoreCols,
                    className="dt-right"
                ),
                list(
                    targets="ID",
                    searchable=FALSE
                )
            ),
            buttons=list(
                list(
                    extend="csv",
                    text=as.character(
                        shiny::span(shiny::icon("download"), "Download table")
                    ),
                    exportOptions=list(
                        columns=":visible",
                        modifier=list(
                            selected=NULL
                        )
                    )
                ),
                list(
                    extend="colvis",
                    text="Show/hide columns",
                    columns="th:nth-child(n+2)",
                    collectionLayout="fixed four-column",
                    className="colvis"
                )
            )
        )
    )
    ## higlight ID column
    dt <- DT::formatStyle(
        table=dt,
        column=1,
        "border-left"="1px solid black",
        "border-right"="1px solid black"
    )
    ## logical columns
    dt <- DT::formatStyle(
        table=dt,
        columns=logicalCols,
        color=DT::styleEqual(
            level=c(TRUE, FALSE),
            values=c("black", "transparent")
        ),
        backgroundColor=DT::styleEqual(
            levels=c(TRUE, FALSE),
            values=c("tomato", "white")
        )
    )
    ## percent GC column
    dt <- DT::formatStyle(
        table=dt,
        columns=grep("percentGC", colnames(data)),
        backgroundColor=DT::styleInterval(
            cuts=c(20, 80),
            values=c("tomato", "white", "tomato")
        )
    )
    ## alignment summary columns
    for (i in alnSummaryCols){
        dt <- DT::formatStyle(
            table=dt,
            columns=i,
            color=DT::styleInterval(
                cuts=c(0),
                values=c("transparent", "black")
            ),
            backgroundColor=DT::styleEqual(
                levels=NA,
                values="#E6EAED",
                default="white"
            )
        )
    }
    ## score columns
    scoreCols_noBar <- c("score_composite", "score_cds", "score_exon",
                         "score_conservation_binary")
    scoreCols <- setdiff(scoreCols, scoreCols_noBar)
    dt <- DT::formatStyle(
        table=dt,
        columns=scoreCols,
        background=styleColorBar(c(0,1), '#3c8dbc33'),
        backgroundSize="100% 90%",
        backgroundRepeat="no-repeat",
        backgroundPosition="center"
    )
    
    return(dt)
}
