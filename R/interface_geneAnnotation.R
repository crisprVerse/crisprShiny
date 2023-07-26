#' @importFrom DT dataTableOutput
.renderGeneAnnotationTab <- function(session,
                                     data
){
    shiny::req(data)
    ns <- session$ns
    shiny::renderUI({
        shiny::tagList(
            shiny::br(),
            DT::dataTableOutput(ns("geneAnnotationDataTable"))
            ## TODO: add plot
        )
    })
}
