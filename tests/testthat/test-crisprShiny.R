test_that("crisprShiny requires a GuideSet passed to guideSet", {
    bad_input <- list(NULL,
                      NA,
                      "guideSet",
                      as(guideSetExample_basic, "GenomicRanges"))
    for (i in bad_input){
        expect_error(crisprShiny(guideSet=i))
    }
    expect_error(crisprShiny(guideSetExample_basic), regexp=NA)
})


test_that("crisprShiny requires a GRangesList object or NULL passed to geneModel", {
    bad_input <- list(NA,
                      "geneModel",
                      txdb_human[[1]],
                      txdb_human[1],
                      txdb_human[-1])
    for (i in bad_input){
        expect_error(crisprShiny(guideSet=guideSetExample_basic,
                                 geneModel=i))
    }
    good_input <- list(NULL,
                       txdb_human)
    for (i in good_input){
        expect_error(crisprShiny(guideSet=guideSetExample_basic,
                                 geneModel=i),
                     regexp=NA)
    }
})


test_that("crisprShiny requires boolean value passed to useFilterPreset", {
    bad_input <- c(NULL,
                   NA,
                   "TRUE")
    for (i in bad_input){
        expect_error(crisprShiny(guideSet=guideSetExample_basic,
                                 useFilterPresets=i))
    }
    good_input <- list(TRUE,
                       FALSE)
    for (i in good_input){
        expect_error(crisprShiny(guideSet=guideSetExample_basic,
                                 useFilterPresets=i),
                     regexp=NA)
    }
})


test_that("crisprShiny returns a shiny.appobj object", {
    ## test several input value combinations
    sets <- list(guideSetExample_basic,
                 guideSetExample_kras,
                 guideSetExample_kras_be,
                 guideSetExample_ntcs)
    geneModels <- list(NULL, txdb_human)
    booleans <- list(TRUE, FALSE)
    for (i in sets){
        for (ii in geneModels){
            for (iii in booleans){
                app <- crisprShiny(guideSet=i, geneModel=ii, useFilterPresets=iii)
                expect_true(methods::is(app, "shiny.appobj"))
            }
        }
    }
})
