## can this be moved to setup?
df <- GuideSet2DataFrames(guideSetExample_basic)[["primary"]]
results_min <- .formatOnTargets(df, nuclease=SpCas9)
df_ko <- GuideSet2DataFrames(head(guideSetExample_kras))[["primary"]] # temp: head
results_ko <- .formatOnTargets(df_ko, nuclease=SpCas9)
df_noncanonical <- df_ko
df_noncanonical$pam[1] <- "AAA"
results_noncanonical <- .formatOnTargets(df_noncanonical, nuclease=SpCas9)


test_that(".formatOnTargets returns data.frame with minimal columns", {
    minimalCols <- c("ID", "protospacer", "pam", "chr", "pam_site", "strand")
    expect_true(all(minimalCols %in% colnames(results_min)))
})

test_that(".formatOnTargets applies highlight attr to noncanonical PAMs", {
    highlight_color <- "goldenrod"
    expect_true(grepl(highlight_color, results_noncanonical$pam[1]))
    expect_false(any(grepl(highlight_color, results_noncanonical$pam[-1])))
})

test_that(".formatOnTargets sets off-target scores for noncanonical PAMs to NA", {
    # expect_true(is.na(results_noncanonical$score_cfd[1]))
    # expect_true(is.na(results_noncanonical$score_mit[1]))
    expect_true(grepl("NA", results_noncanonical$score_cfd[1])) # is character for now
    expect_true(grepl("NA", results_noncanonical$score_mit[1])) # is character for now
    expect_false(any(is.na(results_noncanonical$score_cfd[-1])))
    expect_false(any(is.na(results_noncanonical$score_mit[-1])))
})

test_that(".formatOnTargets rounds scores to 3 decimal places", {
    scores <- as.numeric(results_ko$score_azimuth)
    expect_identical(scores, round(scores, 3))
})

test_that(".formatOnTargets sorts rows by rank column, when present", {
    expect_true(isSorted(results_ko$rank))
})

test_that(".formatOnTargets retains all extra columns", {
    extraCol <- "EXTRA_COLUMN"
    df_ko[[extraCol]] <- 0
    results_extra <- .formatOnTargets(df_ko, nuclease=SpCas9)
    expect_true(extraCol %in% colnames(results_extra))
})
