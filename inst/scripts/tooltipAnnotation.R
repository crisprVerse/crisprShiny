## list of tooltip annotations

tooltipAnnotation <- list(
    ## on-targets table
    ID="Spacer ID",
    protospacer="Protospacer sequence in the host genome targeted by the CRISPR nuclease",
    pam="Protospacer adjacent motif recognized by the CRISPR nuclease",
    chr="Chromosome",
    pam_site="Coordinate of the first (5\u2032-terminal) nucleotide of the PAM sequence",
    strand="DNA strand on which the protospacer is located",
    gene_symbol="Gene symbol of target gene",
    gene_id="Ensembl ID of target gene",
    n0="Total alignments with 0 mismatches",
    n1="Total alignments with 1 mismatch",
    n2="Total alignments with 2 mismatches",
    n3="Total alignments with 3 mismatches",
    n0_c="Alignments in CDS regions with 0 mismatches",
    n1_c="Alignments in CDS regions with 1 mismatch",
    n2_c="Alignments in CDS regions with 2 mismatches",
    n3_c="Alignments in CDS regions with 3 mismatches",
    n0_p="Alignments in promoter regions with 0 mismatches",
    n1_p="Alignments in promoter regions with 1 mismatch",
    n2_p="Alignments in promoter regions with 2 mismatches",
    n3_p="Alignments in promoter regions with 3 mismatches",
    percentGC="Percent GC content of spacer sequence",
    polyA="Whether gRNA spacer sequence contains a stretch of at least four consecutive A bases",
    polyC="Whether gRNA spacer sequence contains a stretch of at least four consecutive C bases",
    polyG="Whether gRNA spacer sequence contains a stretch of at least four consecutive G bases",
    polyT="Whether gRNA spacer sequence contains a stretch of at least four consecutive T bases (termination signal for U6 promoter)",
    startingGGGGG="Whether the protospacer has 5 consecutive G bases at its 5\u2032 end",
    NNGG="Extended PAM sequence for SpCas9: one nucleotide upstream of PAM sequence followed by PAM sequence",
    selfHairpin="Predicted hairpin formation within the spacer sequence via self-complementarity",
    backboneHairpin="Predicted hairpin formation with the backbone sequence via sequence complementarity",
    inRepeats="Whether protospacer overlaps a repeat element",
    hasSNP="Whether protospacer overlaps a SNP",
    cut_site="Coordinate of the nucleotide immediately upstream of the double-stranded break",
    region="Custom-defined genomic region",
    ## alignments table
    alignmentType="Whether alignment is on-target or off-target (with number of mismatches)",
    mismatchesAndPam="DNA:gRNA mismatches are given by genomic nucleotide symbols, with red symbols overlapping the gRNA seed sequence; the PAM sequence is always shown",
    genomicContext="Gene regions that overlap the putative off-target protospacer",
    ## gene annotation
    anchor_site="Protospacer coordinate used to determine whether it overlaps genomic regions",
    tx_id="Ensembl ID of target gene isoform",
    protein_id="Ensembl ID of protein product of the target gene isoform",
    exon_id="Ensembl ID of target exon",
    cut_cds="Whether the protospacer occurs in the CDS for the given transcript",
    cut_fiveUTRs="Whether the protospacer occurs in the 5\u2032 UTR for the given transcript",
    cut_threeUTRs="Whether the protospacer occurs in the 3\u2032 UTR for the given transcript",
    cut_introns="Whether the protospacer occurs in an intron for the given transcript",
    aminoAcidIndex="Numeric position of the amino acid in the protein product where the gRNA spacer targets",
    downstreamATG="Number of downstream, in-frame ATGs within 250bp of the gRNA target site",
    percentCDS="Location of the gRNA target site with respect to the CDS of the given transcript, where 0 is at the 5\u2032 terminus and 100 is at the 3\u2032 terminus",
    percentTx="Location of the gRNA target site with respect to the given transcript, where 0 is at the 5\u2032 terminus and 100 is at the 3\u2032 terminus",
    nIsoforms="Number of isoforms for the given gene that are targeted by the gRNA spacer",
    totalIsoforms="Number of isoforms for the given gene",
    percentIsoforms="Percent of isoforms targeted for the given gene",
    isCommonExon="Whether gRNA spacer targets a common exon for the given gene",
    nCodingIsoforms="Number of coding isoforms for the given gene that are targeted by the gRNA spacer",
    totalCodingIsoforms="Number of isoforms that give a protein product for the given gene",
    percentCodingIsoforms="Percent of coding isoforms targeted for the given gene",
    isCommonCodingExon="Whether gRNA spacer targets a common coding exon for the given gene",
    pfam="Pfam domain overlapping the protospacer sequence",
    ## tss annotation
    # score="...",
    # peak_start="...",
    # peak_end="...",
    # source="...",
    # promoter="...", P1...
    tss_id="ID for the TSS",
    tss_strand="Strand the TSS is located on",
    tss_pos="Genomic coordinate of the TSS",
    dist_to_tss="Distance in nucleotides between protospacer anchor_site and tss_pos",
    ## snps
    rs="Reference SNP cluster ID",
    rs_site="Genomic coordinate of the SNP",
    rs_site_rel="Position of SNP relative to the PAM site",
    allele_ref="DNA sequence of the SNP reference allele",
    allele_minor="DNA sequence of the SNP minor allele",
    MAF_1000G="Minor allele frequency in the 1000 Genomes project",
    MAF_TOPMED="Minor allele frequence in the TOPMed project",
    type="Type of SNP (ins:insertion, del:deletion)",
    length="Length of SNP in nucleotides"
)


usethis::use_data(tooltipAnnotation,
                  overwrite=TRUE)
