test_that(".getGuideBrowserParameters gives error when no rows selected", {
    expect_equal(.getGuideBrowserParameters(guideSetExample_kras,
                                            gene="KRAS",
                                            geneModel=txdb_kras,
                                            selectedRows=numeric(0),
                                            plotWindow="full",
                                            guideStacking="full"),
                 list("error"="index")
    )
})


test_that(".getGuideBrowserParameters gives error when >20 rows selected", {
    expect_equal(.getGuideBrowserParameters(guideSetExample_kras,
                                            gene="KRAS",
                                            geneModel=txdb_kras,
                                            selectedRows=1:25,
                                            plotWindow="full",
                                            guideStacking="full"),
                 list("error"="render")
    )
})


test_that(".getGuideBrowserParameters gives error when chr is not unique", {
    expect_equal(.getGuideBrowserParameters(ntc_kras,
                                            gene="KRAS",
                                            geneModel=txdb_kras,
                                            selectedRows=c(1, length(ntc_kras)),
                                            plotWindow="full",
                                            guideStacking="full"),
                 list("error"="chr")
    )
})


test_that(".getGuideBrowserParameters returns NULL for gene as empty string", {
    out <- .getGuideBrowserParameters(guideSetExample_kras,
                                      gene="",
                                      geneModel=txdb_kras,
                                      selectedRows=1,
                                      plotWindow="full",
                                      guideStacking="full")
    expect_null(out$gene)
})


test_that(".getGuideBrowserParameters returns gene symbol when provided", {
    gene <- "KRAS"
    out <- .getGuideBrowserParameters(guideSetExample_kras,
                                      gene=gene,
                                      geneModel=txdb_kras,
                                      selectedRows=1,
                                      plotWindow="full",
                                      guideStacking="full")
    expect_equal(out$gene, gene)
})


test_that(".getGuideBrowserParameters returns geneModel in list", {
    out <- .getGuideBrowserParameters(guideSetExample_kras,
                                      gene="KRAS",
                                      geneModel=txdb_kras,
                                      selectedRows=1,
                                      plotWindow="full",
                                      guideStacking="full")
    expect_equal(out$geneModel, txdb_kras)
    out_null <- .getGuideBrowserParameters(guideSetExample_kras,
                                           gene="KRAS",
                                           geneModel=NULL,
                                           selectedRows=1,
                                           plotWindow="full",
                                           guideStacking="full")
    expect_null(out_null$geneModel)
})


test_that(".getGuideBrowserParameters coords depend on geneModel when plotWindow='full'", {
    ## plotWindow="full"
    out <- .getGuideBrowserParameters(guideSetExample_kras,
                                      gene="KRAS",
                                      geneModel=txdb_kras,
                                      selectedRows=1,
                                      plotWindow="full",
                                      guideStacking="full")
    out_null <- .getGuideBrowserParameters(guideSetExample_kras,
                                           gene="KRAS",
                                           geneModel=NULL,
                                           selectedRows=1,
                                           plotWindow="full",
                                           guideStacking="full")
    expect_true(out$start != out_null$start)
    expect_true(out$end != out_null$end)
    expect_true(out$coordinates != out_null$coordinates)
    ## plotWindow="zoom"
    out <- .getGuideBrowserParameters(guideSetExample_kras,
                                      gene="KRAS",
                                      geneModel=txdb_kras,
                                      selectedRows=1,
                                      plotWindow="zoom",
                                      guideStacking="full")
    out_null <- .getGuideBrowserParameters(guideSetExample_kras,
                                           gene="KRAS",
                                           geneModel=NULL,
                                           selectedRows=1,
                                           plotWindow="zoom",
                                           guideStacking="full")
    expect_true(out$start == out_null$start)
    expect_true(out$end == out_null$end)
    expect_true(out$coordinates == out_null$coordinates)
})


test_that(".getGuideBrowserParameters converts guideStacking='full' to NA", {
    x <- c("full", "squish", "dense")
    for (i in 1:3){
        results <- .getGuideBrowserParameters(guideSetExample_kras,
                                                gene="KRAS",
                                                geneModel=txdb_kras,
                                                selectedRows=1,
                                                plotWindow="full",
                                                guideStacking=x[i])
        if (x[i] == "full"){
            expect_true(is.na(results$guideStacking))
        } else {
            expect_equal(results$guideStacking, x[i])
        }
    }
})
