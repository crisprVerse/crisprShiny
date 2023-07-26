#' @importFrom DT dataTableOutput
.renderEnzymeAnnotationTab <- function(session,
                                       data
){
    shiny::req(data)
    ns <- session$ns
    ## add visualization
    shiny::renderUI({
        shiny::tagList(
            shiny::br(),
            DT::dataTableOutput(ns("enzymeAnnotationDataTable"))
        )
    })
}
