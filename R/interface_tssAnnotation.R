#' @importFrom DT dataTableOutput
.renderTssAnnotationTab <- function(session,
                                    data
){
    shiny::req(data)
    ns <- session$ns
    shiny::renderUI({
        shiny::tagList(
            shiny::br(),
            DT::dataTableOutput(ns("tssAnnotationDataTable"))
        )
    })
}
