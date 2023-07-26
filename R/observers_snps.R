#' @importFrom DT datatable formatStyle
.snpsDataTable <- function(data,
                           id
){
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
        options=list(
            paging=FALSE,
            scrollX=TRUE,
            scrollY=500,
            scrollCollapse=TRUE,
            searching=FALSE,
            ordering=TRUE,
            dom="lrtBip",
            select=TRUE,
            headerCallback=headerCallback
        )
    )
    dt <- DT::formatStyle(
        table=dt,
        columns=colnames(data),
        fontFamily="Courier"
    )
    return(dt)
}
