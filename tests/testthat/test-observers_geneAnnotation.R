server <- function(gs){
    function(input, output, session){
        df <- GuideSet2DataFrames(gs)[["geneAnnotation"]] ## remove indices
        geneAnnotationDT <- shiny::reactive(
            .geneAnnotationDataTable(
                data=df,
                id=names(gs)[1]
            )
        )
        output$geneAnnotationDataTable <- DT::renderDataTable(geneAnnotationDT())
    }
}



test_that("Gene Annotation DT is not available for minimal GuideSet", {
    gs <- guideSetExample_kras
    testServer(server(gs), {
        expect_error(geneAnnotationDT(), regexp=NA)
        expect_error(output$geneAnnotationDataTable, regexp=NA)
    })
})


test_that("Gene Annotation DT is expected for annotated CRISPRko GuideSet", {
    gs <- guideSetExample_kras
    testServer(server(guideSetExample_kras), {
        x <- geneAnnotationDT()$x
        ## data
        cols <- c("ID", colnames(geneAnnotation(gs)))
        expect_setequal(colnames(x$data), cols)
        expect_equal(nrow(x$data), 4)
        ## DT settings
        expect_equal(x$filter, "none")
        expect_true("FixedColumns" %in% x$extensions)
        expect_equal(x$selection$mode, "none")
        output$geneAnnotationDataTable
    })
})

## additional tests for other GuideSet examples (be, ai)