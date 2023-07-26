#' @importFrom DT datatable formatStyle
.geneAnnotationDataTable <- function(data,
                                     id
){
    shiny::req(data, id)
    data <- data[data$ID == id, , drop=FALSE]
    
    .initiateDatatableTooltips(data)
    headerCallback <- .getHeaderCallback(ncol(data))
    
    dt <- DT::datatable(
        data,
        class="compact cell-border nowrap",
        rownames=FALSE,
        filter="none",
        selection="none",
        escape=FALSE,
        extensions=c("FixedColumns"),
        options=list(
            paging=FALSE,
            scrollX=TRUE,
            scrollY=500,
            scrollCollapse=TRUE,
            searching=FALSE,
            ordering=TRUE,
            dom="lrtBip",
            headerCallback=headerCallback,
            fixedColumns=list(
                leftColumns=1
            ),
            columnDefs=list(
                list(
                    targets="ID",
                    orderable=FALSE
                )
            ),
            select=TRUE
        )
    )
    
    dt <- DT::formatStyle(
        table=dt,
        columns=colnames(data),
        fontFamily="Courier"
    )
    
    return(dt)
}
