## generate full-annotated GuideSet object for human KRAS
library(crisprDesign)
library(crisprDesignData)
library(BSgenome.Hsapiens.UCSC.hg38)
library(usethis)
data("SpCas9", package="crisprBase")
data("BE4max", package="crisprBase")
data("txdb_human", package="crisprDesignData")
data("tss_human", package="crisprDesignData")
data("gr.repeats.hg38", package="crisprDesignData")
bsgenome <- BSgenome.Hsapiens.UCSC.hg38
canonicalIsoform <- "ENST00000311936"
## local files
aligner_index <- "~/crisprIndices/bowtie/hg38/hg38"
conservationFile <- "~/crisprIndices/conservation/hg38/hg38.phyloP30way.bw"
vcf <- "~/crisprIndices/snps/dbsnp151.grch38/00-common_all_snps_only.vcf.gz"

kras_cds <- queryTxObject(txdb_human,
                          featureType="cds",
                          queryColumn="gene_symbol",
                          queryValue="KRAS")
kras_tss <- queryTss(tss_human,
                     queryColumn="gene_symbol",
                     queryValue="KRAS",
                     tss_window=c(-500, 500))
kras <- c(kras_cds, kras_tss)
kras <- reduce(kras)


## generate GuideSet for CRISPRko
gs <- findSpacers(kras,
                  crisprNuclease=SpCas9,
                  bsgenome=bsgenome)
## minimal example
guideSetExample_basic <- gs
usethis::use_data(guideSetExample_basic,
                  compress="xz",
                  overwrite=TRUE)


addCrisprKoAnnotations <- function(gs){
    gs <- addSequenceFeatures(gs,
                              addHairpin=TRUE)
    gs <- addRepeats(gs,
                     gr.repeats=gr.repeats.hg38)
    gs <- addRestrictionEnzymes(gs)
    gs <- addSpacerAlignmentsIterative(gs,
                                       aligner="bowtie",
                                       txObject=txdb_human,
                                       tssObject=tss_human,
                                       aligner_index=aligner_index,
                                       bsgenome=bsgenome,
                                       n_mismatches=3) # annotationType = (gene_symbol | gene_id)?
    gs <- addOnTargetScores(gs)
    gs <- addOffTargetScores(gs)
    gs <- addPamScores(gs)
    gs <- addConservationScores(gs,
                                conservationFile=conservationFile)
    gs <- addCompositeScores(gs,
                             methods=c("azimuth", "deephf", "deepspcas9",
                                       "lindel", "ruleset1", "ruleset3",
                                       "crisprater", "crisprscan"))
    gs <- addGeneAnnotation(gs,
                            txObject=txdb_human,
                            addPfam=TRUE,
                            mart_dataset="hsapiens_gene_ensembl")
    gs <- addIsoformAnnotation(gs, "ENST00000256078")
    gs <- addTssAnnotation(gs,
                           tssObject=tss_human,
                           tss_window=c(-500, 500))
    gs <- addDistanceToTss(gs,
                           tss_id=canonicalIsoform)
    # chrDir <- "~/crisprIndices/chromatin/hg38"
    # chromatinFiles <- c(
    #     mnase=file.path(chrDir, "crispria_mnase_human_K562_hg38.bigWig"),
    #     dnase=file.path(chrDir, "crispria_dnase_human_K562_hg38.bigWig"),
    #     faire=file.path(chrDir, "crispria_faire_human_K562_hg38.bigWig")
    # )
    # fastaFile <- "~/crisprIndices/genomes/hg38/hg38.fa.gz"
    # gs_ko <- addCrispraiScores(
    #     gs_ko,
    #     gr=kras,
    #     tssObject=tss_human,
    #     chromatinFiles=chromatinFiles,
    #     fastaFile=fastaFile
    # )
    gs <- addSNPAnnotation(gs,
                           vcf=vcf)
    gs <- addOpsBarcodes(gs)
    gs <- rankSpacers(gs,
                      modality="CRISPRko")
    mcols(gs)[["gene_symbol"]] <- "KRAS"
    mcols(gs)[["gene_id"]] <- "ENSG00000133703"
    return(gs)
}

guideSetExample_kras <- addCrisprKoAnnotations(gs)
usethis::use_data(guideSetExample_kras,
                  compress="xz",
                  overwrite=TRUE)

## generate GuideSet with NTCs
ntcs <- c("ntc_a"=paste0(rep("A", spacerLength(SpCas9)), collapse=""),
          "ntc_t"=paste0(rep("t", spacerLength(SpCas9)), collapse=""))
guideSetExample_ntcs <- addNtcs(gs, ntcs)
guideSetExample_ntcs <- addCrisprKoAnnotations(guideSetExample_ntcs)
usethis::use_data(guideSetExample_ntcs,
                  compress="xz",
                  overwrite=TRUE)


## generate GuideSet for CRISPRbe
addBaseEditorAnnotations <- function(gs){
    gs <- addEditingSites(gs,
                          substitution="C2T")
    gs <- addGeneAnnotation(gs,
                            txObject=txdb_human)
    gs <- addTxTable(gs,
                     gene_id="ENSG00000133703",
                     txObject=txdb_human)
    gs <- addExonTable(gs,
                       gene_id="ENSG00000133703",
                       txObject=txdb_human)
    gs <- addEditedAlleles(gs,
                           baseEditor=BE4max,
                           txTable=getTxInfoDataFrame(canonicalIsoform,
                                                      txObject=txdb_human,
                                                      bsgenome=bsgenome),
                           editingWindow=c(-20, -8))
    mcols(gs)[["gene_symbol"]] <- "KRAS"
    mcols(gs)[["gene_id"]] <- "ENSG00000133703"
    return(gs)
}

guideSetExample_kras_be <- findSpacers(kras_cds,
                                       crisprNuclease=BE4max,
                                       bsgenome=bsgenome)
guideSetExample_kras_be <- addBaseEditorAnnotations(guideSetExample_kras_be)
usethis::use_data(guideSetExample_kras_be,
                  compress="xz",
                  overwrite=TRUE)
