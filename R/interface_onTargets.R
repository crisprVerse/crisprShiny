#' @importFrom DT dataTableOutput
.onTargetsModule <- function(session
){
    ns <- session$ns
    
    shiny::renderUI({
        shiny::tagList(
            shiny::h4("Table of on-targets"),
            shiny::fluidRow(
                shiny::column(
                    width=3,
                    shiny::actionButton(
                        inputId=ns("onTargetsFilters"),
                        label="Filter on-targets",
                        icon=shiny::icon("filter"),
                        width="80%"
                    )
                )
            ),
            shiny::br(),
            DT::dataTableOutput(ns("primaryDataTable"))
        )
    })
}
