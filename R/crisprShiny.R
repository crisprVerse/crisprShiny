#' @title Interactive visualization of GuideSets via Shiny applications
#' @description Means to interactively visualize gRNAs within a `GuideSet`
#'     object via a Shiny application. Contents of the Shiny app reflect
#'     annotations contained within the `GuideSet` object, and provide
#'     intuitive controls to examine, filter, and export `.csv` tables of gRNAs.
#' 
#' @param guideSet A GuideSet object.
#' @param geneModel A GRangesList object obtained using
#'     `crisprDesign::TxDb2GRangesList`.
#  @param modality Not currently used.
#' @param useFilterPresets Whether to use preset filter values on app launch.
#'     See details.
# @param canonicalIsoforms Not currently used; optional, takes data.frame of
#      "tx_id" and "gene_id". See crisprDesignData for examples.
#' 
#' @details Preset filter values
#'     
#' Using sensible, preset filters can conveniently remove many poorer-quality
#' gRNAs from view upon app launch. This can be done by setting
#' `useFilterPresets=TRUE`, while passing `FALSE` will retain all gRNAs in
#' `guideSet`. Of course, filters can still be adjusted within the app to
#' either further refine or broaden the list of gRNAs to view. Setting
#' `useFilterPresets=TRUE` (default) will impose the following filter
#' criteria, as appropriate to the `guideSet`, upon app launch:
#' * spacers with polyT are excluded
#' * permissible spacer percent GC range set to \[20, 80\]
#' * spacers missing values for any score method (`NA`) are excluded
#' * (SpCas9 nuclease only) minimum permissible DeepHF and DeepSpCas9 scores set
#'     to 0.5
#' * spacers targeting repeat elements are excluded
#' * spacers overlapping SNPs are excluded
#' * (for GuideSets having gene annotation) spacers targeting the final 15%
#'     of the gene CDS (i.e., 3' end) are excluded
#'
#' Filters will not be applied if the `guideSet` lacks the necessary annotation.
#' For example, a `guideSet` lacking SNP annotation will not be filtered on the
#' SNP criterium.
#' 
#' @return A Shiny app object.
#' 
#' @examples
#'     library(crisprShiny)
#'     data("guideSetExample_kras", package="crisprShiny")
#'     app <- crisprShiny(guideSetExample_kras)
#'     
#'     if (interactive()) {
#'         shiny::runApp(app)
#'     }
#'     
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @importFrom methods is
#' @importFrom S4Vectors isTRUEorFALSE
#' @export
crisprShiny <- function(guideSet,
                        geneModel=NULL,
                        # modality=c("crisprko", "crispra", "crispri"),
                        useFilterPresets=TRUE
                        # canonicalIsoforms=NULL
                        
){
    ## check inputs
    stopifnot("guideSet must be a GuideSet object." = {
        methods::is(guideSet, "GuideSet")
    })
    # modality <- match.arg(modality)
    .validateGeneModel(geneModel)
    stopifnot("useFilterPresets must be TRUE or FALSE" = {
        S4Vectors::isTRUEorFALSE(useFilterPresets)
    })
    
    app <- shiny::shinyApp(
        ui=shiny::fluidPage(
            crisprUI("crisprShinyApp")
        ),
        server=function(input, output, session){
            observeEvent(guideSet, {
                crisprServer(
                    id="crisprShinyApp",
                    guideSet=guideSet,
                    geneModel=geneModel,
                    title="crisprShiny: interactive visualization of gRNAs",
                    useFilterPresets=useFilterPresets
                )
            })
        }
    )
}
