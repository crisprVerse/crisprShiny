#' @importFrom DT dataTableOutput
.renderSnpsTab <- function(session,
                           data
){
    shiny::req(data)
    ns <- session$ns
    ## add visualization
    shiny::renderUI({
        shiny::tagList(
            shiny::br(),
            DT::dataTableOutput(ns("snpsDataTable"))
        )
    })
}
