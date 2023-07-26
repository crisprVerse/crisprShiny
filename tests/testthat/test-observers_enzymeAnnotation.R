server <- function(gs){
    function(input, output, session){
        df <- GuideSet2DataFrames(gs)[["enzymeAnnotation"]] ## remove indices
        enzymeAnnotationDT <- shiny::reactive(
            .enzymeAnnotationDataTable(
                data=df,
                id=names(gs)[1]
            )
        )
        output$enzymeAnnotationDataTable <- DT::renderDataTable(enzymeAnnotationDT())
    }
}



test_that("Enzyme Annotation DT is not available for minimal GuideSet", {
    gs <- guideSetExample_kras
    testServer(server(gs), {
        expect_error(enzymeAnnotationDT(), regexp=NA)
        expect_error(output$enzymeAnnotationDataTable, regexp=NA)
    })
})


test_that("Enzyme Annotation DT is expected for annotated CRISPRko GuideSet", {
    testServer(server(guideSetExample_kras), {
        x <- enzymeAnnotationDT()$x
        ## data
        enzymeAnn <- crisprDesign::enzymeAnnotation(guideSetExample_kras)
        cols <- c("ID", colnames(enzymeAnn))
        expect_setequal(colnames(x$data), cols)
        expect_equal(nrow(x$data), 1)
        ## DT settings
        expect_equal(x$filter, "none")
        expect_equal(x$selection$mode, "none")
        output$enzymeAnnotationDataTable
    })
})

## additional tests for other GuideSet examples (be, ai)