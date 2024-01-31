test_that(".getAlignmentParameters_default returns expected parameters", {
    gs <- guideSetExample_kras
    data <- crisprDesign::GuideSet2DataFrames(gs)[["primary"]]
    nuclease <- crisprDesign::crisprNuclease(gs)
    out <- .getAlignmentParameters_default(data, nuclease)
    expect_equal(out$mmCount, 2)
    expect_equal(out$targetRegion, "all")
    expect_equal(out$canonicalPam, TRUE)
    expect_equal(out$scoreSorting, "score_cfd")
    
    gs <- guideSetExample_basic
    data <- crisprDesign::GuideSet2DataFrames(gs)[["primary"]]
    out <- .getAlignmentParameters_default(data, nuclease)
    expect_equal(out$mmCount, 0)
    nuclease <- AsCas12a
    out <- .getAlignmentParameters_default(data, nuclease)
    expect_null(out$scoreSorting)
})



test_that(".getAlignmentParameters returns default values for missing parameters", {
    gs <- guideSetExample_kras
    data <- crisprDesign::GuideSet2DataFrames(gs)[["primary"]]
    nuclease <- crisprDesign::crisprNuclease(gs)
    ## default values
    out <- .getAlignmentParameters(data,
                                   nuclease,
                                   mmCount=NULL,
                                   targetRegion=NULL,
                                   canonicalPam=NULL,
                                   scoreSorting=NULL)
    expect_equal(out$mmCount, 2)
    expect_equal(out$targetRegion, "all")
    expect_equal(out$canonicalPam, TRUE)
    expect_equal(out$scoreSorting, "score_cfd")
    ## set values
    out <- .getAlignmentParameters(data,
                                   nuclease,
                                   mmCount=1,
                                   targetRegion="cds",
                                   canonicalPam=FALSE,
                                   scoreSorting="score_mit")
    expect_equal(out$mmCount, 1)
    expect_equal(out$targetRegion, "cds")
    expect_equal(out$canonicalPam, FALSE)
    expect_equal(out$scoreSorting, "score_mit")
})


test_that(".formatAlignmentsData returns correctly formatted data.frame", {
    gs <- guideSetExample_kras
    alns <- GuideSet2DataFrames(gs)[["alignments"]]
    data <- .formatAlignmentsData(
        alignments=alns,
        id="spacer_1",
        mmCount=3,
        targetRegion="all",
        canonicalPam=TRUE,
        scoreSorting="score_cfd",
        nuclease=crisprDesign::crisprNuclease(gs)
    )
    expect_equal(unique(data$ID), "spacer_1")
    expect_equal(colnames(data),
                 c("ID", "alignmentType", "mismatchesAndPam", "score_cfd",
                   "score_mit", "genomicContext", "pam_site"))
    expect_true(is.numeric(data$score_cfd))
    expect_true(is.numeric(data$score_mit))
    expect_true(all(grepl("^chr[0-9XYM]{1,2}([^:]+)?:[0-9]+\\([-+]\\)$", data$pam_site)))
    dotCount <- crisprBase::spacerLength(crisprDesign::crisprNuclease(gs))
    pattern <- paste0("^\\.{", dotCount, "} [ACGT]GG$")
    expect_true(grepl(pattern, data$mismatchesAndPam[1]))
})


test_that(".formatAlignmentsData filters by specified mismatch counts", {
    gs <- guideSetExample_kras
    alns <- GuideSet2DataFrames(gs)[["alignments"]]
    data <- .formatAlignmentsData(
        alignments=alns,
        id="spacer_1",
        mmCount=3,
        targetRegion="all",
        canonicalPam=TRUE,
        scoreSorting="score_cfd",
        nuclease=crisprDesign::crisprNuclease(gs)
    )
    expect_true("On-target" %in% data$alignmentType)
    expect_true("Off-target (1mm)" %in% data$alignmentType)
    expect_true("Off-target (2mm)" %in% data$alignmentType)
    expect_true("Off-target (3mm)" %in% data$alignmentType)
    
    data <- .formatAlignmentsData(
        alignments=alns,
        id="spacer_1",
        mmCount=2,
        targetRegion="all",
        canonicalPam=TRUE,
        scoreSorting="score_cfd",
        nuclease=crisprDesign::crisprNuclease(gs)
    )
    expect_true("On-target" %in% data$alignmentType)
    expect_true("Off-target (1mm)" %in% data$alignmentType)
    expect_true("Off-target (2mm)" %in% data$alignmentType)
    expect_false("Off-target (3mm)" %in% data$alignmentType)
    
    data <- .formatAlignmentsData(
        alignments=alns,
        id="spacer_1",
        mmCount=1,
        targetRegion="all",
        canonicalPam=TRUE,
        scoreSorting="score_cfd",
        nuclease=crisprDesign::crisprNuclease(gs)
    )
    expect_true("On-target" %in% data$alignmentType)
    expect_true("Off-target (1mm)" %in% data$alignmentType)
    expect_false("Off-target (2mm)" %in% data$alignmentType)
    expect_false("Off-target (3mm)" %in% data$alignmentType)
    
    data <- .formatAlignmentsData(
        alignments=alns,
        id="spacer_1",
        mmCount=0,
        targetRegion="all",
        canonicalPam=TRUE,
        scoreSorting="score_cfd",
        nuclease=crisprDesign::crisprNuclease(gs)
    )
    expect_true("On-target" %in% data$alignmentType)
    expect_false("Off-target (1mm)" %in% data$alignmentType)
    expect_false("Off-target (2mm)" %in% data$alignmentType)
    expect_false("Off-target (3mm)" %in% data$alignmentType)
})


test_that(".formatAlignmentsData filters by target region", {
    gs <- guideSetExample_kras
    alns <- GuideSet2DataFrames(gs)[["alignments"]]
    data <- .formatAlignmentsData(
        alignments=alns,
        id="spacer_30",
        mmCount=3,
        targetRegion="all",
        canonicalPam=TRUE,
        scoreSorting="score_cfd",
        nuclease=crisprDesign::crisprNuclease(gs)
    )
    expect_true(any(grepl("cds", data$genomicContext)))
    expect_true(any(grepl("promoter", data$genomicContext)))
    expect_true(any(grepl("intergenic", data$genomicContext)))
    
    data <- .formatAlignmentsData(
        alignments=alns,
        id="spacer_30",
        mmCount=3,
        targetRegion="cds",
        canonicalPam=TRUE,
        scoreSorting="score_cfd",
        nuclease=crisprDesign::crisprNuclease(gs)
    )
    expect_true(any(grepl("cds", data$genomicContext)))
    expect_false(any(grepl("promoter", data$genomicContext)))
    expect_false(any(grepl("intergenic", data$genomicContext)))
    
    data <- .formatAlignmentsData(
        alignments=alns,
        id="spacer_30",
        mmCount=3,
        targetRegion="tss",
        canonicalPam=TRUE,
        scoreSorting="score_cfd",
        nuclease=crisprDesign::crisprNuclease(gs)
    )
    expect_false(any(grepl("cds", data$genomicContext)))
    expect_true(any(grepl("promoter", data$genomicContext)))
    expect_false(any(grepl("intergenic", data$genomicContext)))
})


test_that(".formatAlignmentsData filters by canonicalPam", {
    ## skip: not testable with guideSetExample_kras
})


test_that(".formatAlignmentsData sorts by scoreSorting", {
    gs <- guideSetExample_kras
    alns <- crisprDesign::GuideSet2DataFrames(gs)[["alignments"]]
    data <- .formatAlignmentsData(
        alignments=alns,
        id="spacer_1",
        mmCount=3,
        targetRegion="all",
        canonicalPam=TRUE,
        scoreSorting="score_cfd",
        nuclease=crisprDesign::crisprNuclease(gs)
    )
    expect_equal(sort(data$score_cfd, decreasing=TRUE), data$score_cfd)
    
    data <- .formatAlignmentsData(
        alignments=alns,
        id="spacer_1",
        mmCount=3,
        targetRegion="all",
        canonicalPam=TRUE,
        scoreSorting="score_mit",
        nuclease=crisprDesign::crisprNuclease(gs)
    )
    expect_equal(sort(data$score_mit, decreasing=TRUE), data$score_mit)
})


test_that(".getAlignmentsBrowserPlotParameters returns correct parameters", {
    gs <- guideSetExample_kras
    alns <- crisprDesign::GuideSet2DataFrames(gs)[["alignments"]]
    data <- .formatAlignmentsData(
        alignments=alns,
        id="spacer_1",
        mmCount=3,
        targetRegion="all",
        canonicalPam=TRUE,
        scoreSorting="score_cfd",
        nuclease=crisprDesign::crisprNuclease(gs)
    )
    ind <- 10
    target <- data[ind,]
    context <- target$genomicContext
    geneModel <- txdb_kras
    params <- .getAlignmentsBrowserPlotParameters( # gives warning
        context=context,
        target=target,
        guideSet=gs,
        geneModel=geneModel
    )
    expect_true(methods::is(params$gs, "GuideSet"))
    expect_equal(geneModel, params$geneModel)
    expect_true(grepl(params$targetGene, context))
    expect_true(is.numeric(params$from))
    expect_true(is.numeric(params$to))
    expect_true(params$from < params$to)
    expect_true(is.numeric(params$extend))
    expect_equal(bsgenome(gs), params$bsgenome)
})


test_that(".getAlignmentsBrowserPlotParameters accepts NULL geneModel", {
    gs <- guideSetExample_kras
    alns <- crisprDesign::GuideSet2DataFrames(gs)[["alignments"]]
    data <- .formatAlignmentsData(
        alignments=alns,
        id="spacer_1",
        mmCount=3,
        targetRegion="all",
        canonicalPam=TRUE,
        scoreSorting="score_cfd",
        nuclease=crisprDesign::crisprNuclease(gs)
    )
    ind <- 10
    target <- data[ind,]
    context <- target$genomicContext
    params <- .getAlignmentsBrowserPlotParameters(
        context=context,
        target=target,
        guideSet=gs,
        geneModel=NULL
    )
    expect_null(params$geneModel)
})
