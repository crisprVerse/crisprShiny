#' @importFrom waiter waiter_show spin_1
.showWaiter <- function(loadingMessage
){
    loadingMessage <- switch( # for sgrnaViewer only
        loadingMessage,
        "crisprTechnology"="Retrieving pre-annotated gRNAs",
        "targetGenes"="Selecting target genes",
        "grnaFilters"="Filtering gRNAs",
        "screeningPlatform"="Preparing library for screening platform",
        "controls"="Adding controls",
        "download"="Exporting library",
        loadingMessage
    )
    
    waiter::waiter_show(
        html=shiny::div(
            class="loading",
            waiter::spin_1(),
            shiny::br(),
            shiny::br(),
            shiny::h3(loadingMessage)
        ),
        color="transparent")
}
