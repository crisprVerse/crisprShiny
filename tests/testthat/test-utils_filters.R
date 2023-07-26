test_that(".getFilterDefaults returns permissive values when usePresetFilters=FALSE", {
    results <- .getFilterDefaults(guideSet=guideSetExample_kras,
                                  useFilterPresets=FALSE)
    expect_true(is.list(results))
    scores <- grep("^score_", names(results), value=TRUE)
    for (i in scores){
        expect_equal(results[[i]], 0)
    }
    alns <- grep("^n[0-9](_[cp])?$", names(results), value=TRUE)
    for (i in alns){
        expect_true(is.na(results[[i]]))
    }
    expect_equal(results[["percentGC"]], c(0, 100))
})


test_that(".getFilterDefaults returns restrictive values when usePresetFilters=TRUE", {
    results <- .getFilterDefaults(guideSet=guideSetExample_kras,
                                  useFilterPresets=TRUE)
    expect_true(is.list(results))
    scores <- grep("^score_", names(results), value=TRUE)
    for (i in scores){
        value <- ifelse(i %in% c("score_deepspcas9", "score_deephf"), 0.5, 0)
        expect_equal(results[[i]], value)
    }
    alns <- grep("^n[0-9](_[cp])?$", names(results), value=TRUE)
    for (i in alns){
        expect_true(is.na(results[[i]]))
    }
    expect_equal(results[["percentGC"]], c(20, 80))
})


test_that("Dynamic .getFilterDefaults fields depend on passed GuideSet", {
    results <- .getFilterDefaults(guideSet=guideSetExample_basic,
                                  useFilterPresets=FALSE)
    expect_true(is.list(results))
    expect_false(any(grepl("^score_", names(results))))
    expect_false(any(grepl("^n[0-9](_[cp])?$", names(results))))
})




server <- function(gs){
    function(input, output, session){
        defaults <- shiny::reactive(
            .getFilterDefaults(
                guideSet=guideSetExample_kras,
                useFilterPresets=FALSE
            )
        )
        filterValues <- shiny::reactive(.setFilterValues(defaults(), input))
        output$out <- renderText(filterValues()$percentGC)
        gs_filtered <- shiny::reactive(
            .applyGuideSetFilters(
                guideSet=gs,
                filterValues=filterValues()
            )
        )
    }
}


test_that(".setFilterValues updates filter values upon input change", {
    testServer(server(guideSetExample_kras), {
        expect_equal(output$out, "0 100")
        session$setInputs(filter_percentGC=c(40,60))
        expect_equal(output$out, "40 60")
    })
})


test_that(".applyGuideSetFilters returns same GuideSet for default filters", {
    gs <- guideSetExample_kras_be # remove _be, need to permit NTCs
    testServer(server(gs), {
        expect_equal(gs, gs_filtered())
    })
})


test_that(".applyGuideSetFilters apply nucleotide content filters", {
    testServer(server(guideSetExample_kras), {
        session$setInputs(filter_polyT=TRUE,
                          filter_percentGC=c(20, 80),
                          filter_canonicalPam=FALSE)
        
        polyT <- gs_filtered()$polyT
        expect_true(!any(polyT))
        percentGC <- gs_filtered()$percentGC
        expect_true(min(percentGC) >= 20 && max(percentGC) <= 80)
        canonicalPam <- gs_filtered()$canonicalPam
        pams <- paste0(c("A", "C", "G", "T"), "GG")
        expect_true(all(canonicalPam %in% pams))
        
        # session$setInputs(filter_percentGC=c(0, 0)) # bad test
        # expect_true(length(gs_filtered()) == 0)
    })
})


test_that(".applyGuideSetFilters apply alignment filters", {
    alns <- c(1, 1, 5, 10)
    testServer(server(guideSetExample_kras[1:50]), {
        start <- gs_filtered()
        session$setInputs(filter_n0=alns[1],
                          filter_n1=alns[2],
                          filter_n2=alns[3],
                          filter_n3=alns[4])
        expect_true(all(gs_filtered()$n0 <= alns[1]))
        expect_true(all(gs_filtered()$n1 <= alns[2]))
        expect_true(all(gs_filtered()$n2 <= alns[3]))
        expect_true(all(gs_filtered()$n3 <= alns[4]))
        
        session$setInputs(filter_n0=NA,
                          filter_n1=NA,
                          filter_n2=NA,
                          filter_n3=NA,
                          filter_n0_c=alns[1],
                          filter_n1_c=alns[2],
                          filter_n2_c=alns[3],
                          filter_n3_c=alns[4])
        expect_true(all(gs_filtered()$n0_c <= alns[1]))
        expect_true(all(gs_filtered()$n1_c <= alns[2]))
        expect_true(all(gs_filtered()$n2_c <= alns[3]))
        expect_true(all(gs_filtered()$n3_c <= alns[4]))
        
        session$setInputs(filter_n0_c=NA,
                          filter_n1_c=NA,
                          filter_n2_c=NA,
                          filter_n3_c=NA,
                          filter_n0_p=alns[1],
                          filter_n1_p=alns[2],
                          filter_n2_p=alns[3],
                          filter_n3_p=alns[4])
        expect_true(all(gs_filtered()$n0_p <= alns[1]))
        expect_true(all(gs_filtered()$n1_p <= alns[2]))
        expect_true(all(gs_filtered()$n2_p <= alns[3]))
        expect_true(all(gs_filtered()$n3_p <= alns[4]))
        
        session$setInputs(filter_n0_p=NA,
                          filter_n1_p=NA,
                          filter_n2_p=NA,
                          filter_n3_p=NA)
        end <- gs_filtered()
        expect_equal(start, end)
    })
})


test_that(".applyGuideSetFilters apply score filters", {
    testServer(server(guideSetExample_kras[1:50]), {
        start <- gs_filtered()
        session$setInputs(filter_score_azimuth=0.5)
        expect_true(all(gs_filtered()$score_azimuth >= 0.5))
        session$setInputs(filter_score_azimuth=1)
        expect_true(length(gs_filtered()) == 0)
        session$setInputs(filter_score_azimuth=0)
        expect_equal(gs_filtered(), start)
    })
    ## score_conservation_binary?
    ## ignore ruleset3
})


test_that(".applyGuideSetFilters apply genomic features filters", {
    # inRepeats, hasSNP, pfam
    testServer(server(guideSetExample_kras[1:50]), {
        start <- gs_filtered()
        session$setInputs(filter_inRepeats=TRUE,
                          filter_hasSNP=TRUE,
                          filter_pfam=TRUE)
        expect_true(!any(gs_filtered()$inRepeats))
        expect_true(!any(gs_filtered()$hasSNP))
        # expect_true(!any(is.na(gs_filtered()$pfam))) # adjust evaluation method
        session$setInputs(filter_inRepeats=FALSE,
                          filter_hasSNP=FALSE,
                          filter_pfam=FALSE)
        expect_equal(gs_filtered(), start)
    })
})


test_that(".applyGuideSetFilters apply isoform filters", {
    # isoforms=""
    # percentCDS=c(0, 100)
    # percentCodingIsoforms=0
})


test_that(".applyGuideSetFilters apply promoter filters", {
    # promoter=""
    # distToTss=c(-500, 500)
})
