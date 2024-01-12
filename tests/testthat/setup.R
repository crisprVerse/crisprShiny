library(crisprDesign)
data("SpCas9", package="crisprBase")
data("AsCas12a", package="crisprBase")

data("guideSetExample_basic", package="crisprShiny")
data("guideSetExample_kras", package="crisprShiny")
data("guideSetExample_ntcs", package="crisprShiny")
data("guideSetExample_kras_be", package="crisprShiny")
data("txdb_kras", package="crisprShiny")
data("tss_kras", package="crisprShiny")

## example GuideSets containing NTCs (and NA for most mcols)
ntcs <- c("ntc_a"=paste0(rep("A", spacerLength(SpCas9)), collapse=""),
          "ntc_t"=paste0(rep("t", spacerLength(SpCas9)), collapse=""))

ntc_simple <- crisprDesign::addNtcs(guideSetExample_basic,
                                    ntcs=ntcs)
ntc_kras <- crisprDesign::addNtcs(guideSetExample_kras,
                                  ntcs=ntcs)
ntc_kras_be <- crisprDesign::addNtcs(guideSetExample_kras_be,
                                     ntcs=ntcs)

## example GuideSet with noncanonical PAM
gs_noncanonical <- guideSetExample_basic
mcols(gs_noncanonical)[["pam"]][1] <- "AAA"
