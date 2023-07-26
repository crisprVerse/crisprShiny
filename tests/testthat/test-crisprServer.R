test_that("crisprServer generates all reactives for fully-annotated GuideSet", {
    testServer(crisprServer,
               args=list(
                   id="test_module",
                   guideSet=guideSetExample_kras,
                   geneModel=txdb_human,
                   useFilterPresets=FALSE
               ), {
                   expect_equal(nuclease, crisprDesign::crisprNuclease(guideSet))
                   expect_true(is.list(filter_defaults()))
                   expect_equal(filter_defaults(), filter_saved())
                   expect_true(methods::is(guideSet_filtered(), "GuideSet"))
                   # expect_equal(guideSet_filtered(), guideSet) # some gRNAs still filtered out
                   expect_true(is.list(dataframes()))
                   expect_setequal(names(dataframes()),
                                   c("primary", "alignments", "geneAnnotation",
                                     "tssAnnotation", "enzymeAnnotation", "snps"))
                   expect_true(methods::is(onTargetsDataTable(), "datatables"))
                   session$setInputs(annotationsGuideSelect = "spacer_1")
                   expect_true(is.list(alignments_parameters()))
                   expect_setequal(names(alignments_parameters()),
                                   c("mmCount", "targetRegion", "canonicalPam", "scoreSorting"))
                   expect_true(is.data.frame(alignments_processed()))
                   expect_setequal(colnames(alignments_processed()),
                                   c("ID", "alignmentType", "mismatchesAndPam",
                                     "score_cfd", "score_mit", "genomicContext",
                                     "pam_site"))
                   expect_true(methods::is(geneAnnotationDT(), "datatables"))
                   expect_true(methods::is(tssAnnotationDT(), "datatables"))
                   expect_true(methods::is(enzymeAnnotationDT(), "datatables"))
                   expect_true(methods::is(snpsDT(), "datatables"))
                   expect_null(session$returned())
               }
    )
})


test_that("crisprServer generates minimal reactives for minimal GuideSet", {
    ## throws test error if any annotations are missing
    
    # testServer(crisprServer,
    #            args=list(
    #                id="test_module",
    #                guideSet=guideSetExample_basic,
    #                geneModel=txdb_human,
    #                useFilterPresets=FALSE
    #            ), {
    #                
    #                # expect_equal(nuclease, crisprDesign::crisprNuclease(guideSet))
    #                # expect_true(is.list(filter_defaults()))
    #                # expect_equal(filter_defaults(), filter_saved())
    #                # expect_true(methods::is(guideSet_filtered(), "GuideSet"))
    #                # expect_equal(guideSet_filtered(), guideSet) # some gRNAs still filtered out
    #                # expect_true(is.list(dataframes()))
    #                # expect_equal(names(dataframes()), "primary")
    #                # expect_true(methods::is(onTargetsDataTable(), "datatables"))
    #                # alignments_parameters
    #                # alignments_processed
    #                # session$setInputs(annotationsGuideSelect = "spacer_1")
    #                # expect_true(methods::is(geneAnnotationDT(), "datatables"))
    #                # expect_true(methods::is(tssAnnotationDT(), "datatables"))
    #                # expect_true(methods::is(enzymeAnnotationDT(), "datatables"))
    #                # expect_true(methods::is(snpsDT(), "datatables"))
    #                # expect_null(session$returned())
    #            }
    # )
})



test_that("crisprServer handles NULL geneModel", {
    testServer(crisprServer,
               args=list(
                   id="test_module",
                   guideSet=guideSetExample_kras,
                   geneModel=NULL,
                   useFilterPresets=FALSE
               ), {
                   expect_equal(nuclease, crisprDesign::crisprNuclease(guideSet))
                   expect_true(is.list(filter_defaults()))
                   expect_equal(filter_defaults(), filter_saved())
                   expect_true(methods::is(guideSet_filtered(), "GuideSet"))
                   # expect_equal(guideSet_filtered(), guideSet) # some gRNAs still filtered out
                   expect_true(is.list(dataframes()))
                   expect_setequal(names(dataframes()),
                                   c("primary", "alignments", "geneAnnotation",
                                     "tssAnnotation", "enzymeAnnotation", "snps"))
                   expect_true(methods::is(onTargetsDataTable(), "datatables"))
                   session$setInputs(annotationsGuideSelect = "spacer_1")
                   expect_true(is.list(alignments_parameters()))
                   expect_setequal(names(alignments_parameters()),
                                   c("mmCount", "targetRegion", "canonicalPam", "scoreSorting"))
                   expect_true(is.data.frame(alignments_processed()))
                   expect_setequal(colnames(alignments_processed()),
                                   c("ID", "alignmentType", "mismatchesAndPam",
                                     "score_cfd", "score_mit", "genomicContext",
                                     "pam_site"))
                   expect_true(methods::is(geneAnnotationDT(), "datatables"))
                   expect_true(methods::is(tssAnnotationDT(), "datatables"))
                   expect_true(methods::is(enzymeAnnotationDT(), "datatables"))
                   expect_true(methods::is(snpsDT(), "datatables"))
                   expect_null(session$returned())
               }
    )
})



test_that("crisprServer adds preset filters when useFilterPresets=TRUE", {
    testServer(crisprServer,
               args=list(
                   id="test_module",
                   guideSet=guideSetExample_kras,
                   geneModel=txdb_human,
                   useFilterPresets=TRUE
               ), {
                   expect_equal(nuclease, crisprDesign::crisprNuclease(guideSet))
                   expect_true(is.list(filter_defaults()))
                   expect_equal(filter_defaults(), filter_saved())
                   expect_true(methods::is(guideSet_filtered(), "GuideSet"))
                   ## tests that compare guideSet to guideSet_filtered()
                   expect_true(is.list(dataframes()))
                   expect_null(session$returned())
               }
    )
})
