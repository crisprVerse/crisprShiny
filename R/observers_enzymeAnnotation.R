#' @importFrom DT datatable formatStyle
.enzymeAnnotationDataTable <- function(data,
                                       id
){
    shiny::req(data, id)
    data <- data[data$ID == id, , drop=FALSE]
    
    dt <- DT::datatable(
        data,
        class="compact cell-border nowrap",
        rownames=FALSE,
        filter="none",
        selection="none",
        escape=FALSE,
        # extensions=c("Buttons", "ColReorder", "Select"),
        options=list(
            paging=FALSE,
            scrollX=TRUE,
            scrollY=500,
            scrollCollapse=TRUE,
            searching=FALSE,
            ordering=TRUE,
            dom="lrtBip",
            # fixedColumns=list(leftColumns=1),
            # buttons=list(
            #     list(
            #         extend="csv",
            #         text=as.character(
            #             shiny::span(shiny::icon("download"), "Download table")
            #         ),
            #         exportOptions=list(
            #             columns=":visible",
            #             modifier=list(
            #                 selected=NULL
            #             )
            #         )
            #     ),
            #     list(
            #         extend="colvis",
            #         text="Show/hide columns",
            #         collectionLayout="fixed four-column",
            #         className="colvis"
            #     )
            # ),
            select=TRUE
        )
    )
    
    dt <- DT::formatStyle(
        table=dt,
        columns=colnames(data),
        fontFamily="Courier"
    )
    # dt <- DT::formatStyle(
    #     table=dt,
    #     columns=logicalCols,
    #     color=DT::styleEqual(
    #         level=c(TRUE, FALSE),
    #         values=c("black", "transparent")
    #     ),
    #     backgroundColor=DT::styleEqual(
    #         levels=c(TRUE, FALSE),
    #         values=c("red", "white") # change red (css?)
    #     )
    # )
    # dt <- DT::formatStyle(
    #     table=dt,
    #     columns=grep("percentGC", colnames(data)),
    #     backgroundColor=DT::styleInterval(
    #         cuts=c(20, 80),
    #         values=c("red", "white", "red")
    #     )
    # )
    return(dt)
}