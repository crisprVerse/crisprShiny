data("tooltipAnnotation")

server <- function(gs){
    function(input, output, session){
        df <- GuideSet2DataFrames(gs)[["snps"]] ## remove indices
        snpsDT <- shiny::reactive(
            .snpsDataTable(
                data=df,
                id="spacer_1" # has SNP
            )
        )
        output$snpsDataTable <- DT::renderDataTable(snpsDT())
    }
}



test_that("SNPs DT is not available for minimal GuideSet", {
    gs <- guideSetExample_kras
    testServer(server(gs), {
        expect_error(snpsDT(), regexp=NA)
        expect_error(output$snpsDataTable, regexp=NA)
    })
})


test_that("SNPs DT is expected for annotated CRISPRko GuideSet", {
    testServer(server(guideSetExample_kras), {
        x <- snpsDT()$x
        ## data
        cols <- c("ID", colnames(crisprDesign::snps(guideSetExample_kras)))
        expect_setequal(colnames(x$data), cols)
        expect_equal(nrow(x$data), 1)
        ## DT settings
        expect_equal(x$filter, "none")
        expect_equal(x$selection$mode, "none")
        output$snpsDataTable
    })
})

## additional tests for other GuideSet examples (be, ai)