library("knitr")
library("BiocStyle")

.cran_packages <- c("ggplot2", "gridExtra")
.bioc_packages <- c("dada2", "phyloseq", "DECIPHER", "phangorn")
.inst <- .cran_packages %in% installed.packages()
if(any(!.inst)) {
   install.packages(.cran_packages[!.inst])
}
.inst <- .bioc_packages %in% installed.packages()
if(any(!.inst)) {
   source("http://bioconductor.org/biocLite.R")
   biocLite(.bioc_packages[!.inst], ask = F)
}

# Load packages into session, and print package version
sapply(c(.cran_packages, .bioc_packages), require, character.only = TRUE)

miseq_path <- "~/MinSeqAmp/MinSeqAmpfastq" # CHANGE to the directory containing 

# Sort ensures forward/reverse reads are in same order
fnFs <- sort(list.files(miseq_path, pattern="_R1_001.fastq"))
fnRs <- sort(list.files(miseq_path, pattern="_R2_001.fastq"))

# Extract sample names, assuming filenames have format: SAMPLENAME_XXX.fastq
sampleNames <- sapply(strsplit(fnFs, "_"), `[`, 1)

# Specify the full path to the fnFs and fnRs
fnFs <- file.path(miseq_path, fnFs)
fnRs <- file.path(miseq_path, fnRs)

filt_path <- file.path(miseq_path, "filtered") # Place filtered files in filtered/ subdirectory
if(!file_test("-d", filt_path)) dir.create(filt_path)
filtFs <- file.path(filt_path, paste0(sampleNames, "_F_filt.fastq.gz"))
filtRs <- file.path(filt_path, paste0(sampleNames, "_R_filt.fastq.gz"))

#change numbers in truncLen based on the quality of my dataset. shorter reads get discarded and base pairs after that point are also discarded.
#Might need to leave out truncLen 
out1 <- filterAndTrim(fnFs, filtFs, fnRs, filtRs, trimLeft= c(20,20),truncLen =c(150,150),
              maxN=0, maxEE=c(5,5), truncQ=2, rm.phix=TRUE,
              compress=TRUE, multithread=TRUE) 
print(head(out1))
derepFs <- derepFastq(filtFs, verbose=TRUE)
derepRs <- derepFastq(filtRs, verbose=TRUE)
names(derepFs) <- sampleNames
names(derepRs) <- sampleNames
errF <- learnErrors(filtFs, multithread=TRUE)
errR <- learnErrors(filtRs, multithread=TRUE)
dadaFs <- dada(derepFs, err=errF, multithread=TRUE)
dadaRs <- dada(derepRs, err=errR, multithread=TRUE)
mergers <- mergePairs(dadaFs, derepFs, dadaRs, derepRs,verbose=TRUE, minOverlap = 5)
seqtabAll <- makeSequenceTable(mergers[!grepl("Mock", names(mergers))])
table(nchar(getSequences(seqtabAll)))
seqtabNoC <- removeBimeraDenovo(seqtabAll)

df.dada2 <- data.frame(sequence=colnames(seqtabNoC), abundance=colSums(seqtabNoC))
df.dada2$id <- paste0("sq", df.dada2$sequence)
uniquesToFasta(df.dada2, "dada2_out3v2.fasta", id=df.dada2$id)

capture.output(seqtabAll, file= "seqtaball3v2.txt")
capture.output(seqtabNoC, file= "seqtabNochim3v2.txt")
capture.output(mergers, file= "mergers3v2.txt")

seqs <- colnames(seqtabNoC)
otab <- otu_table(seqtabNoC, taxa_are_rows=FALSE)
colnames(otab) <- paste0("seq", seq(ncol(otab)))
otab = t(otab)
write.table(seqs, "dada_seqs.txt",quote=FALSE)
write.table(otab, "dada_table.txt",quote=FALSE,sep="\t")

getN <- function(x) sum(getUniques(x))
track <- cbind(out1, sapply(dadaFs, getN), sapply(dadaRs, getN), 
sapply(mergers, getN), rowSums(seqtabNoC))
colnames(track) <- c("input", "filtered", "denoisedF", "denoisedR", "merged", "nonchim")
rownames(track) <- sampleNames
capture.output(track, file= "dada2track.txt")
