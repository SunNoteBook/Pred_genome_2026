library(ggplot2)
library(macrosyntR)
library(tidyverse)


orthologs_2sp <- load_orthologs(orthologs_table = "~/Desktop/Ongoing/Pred/Nigon/newInputfromgithub/elegans_NigonOnly_reformed.genelist2",
                               bedfiles = c("~/Desktop/Ongoing/Pred/gfffiles/PREV1.genes.v1.1.gene.bed",
                                            "~/Desktop/Ongoing/Pred/gfffiles/caenorhabditis_elegans.WBPS19.gene.bed"
                               ))
Nigonindex = read.table("~/Desktop/Ongoing/Pred/Nigon/newInputfromgithub/elegans_NigonOnly_reformed.genelist")
orthologs_2sp_unique <- orthologs_2sp %>%distinct(sp2.ID, .keep_all = TRUE)
orthologs_2sp_extend <- orthologs_2sp_unique %>% left_join(Nigonindex[,c(1,3)], by = c("sp1.ID" = "V1"))
colnames(orthologs_2sp_extend)[ncol(orthologs_2sp_extend)] <- "Nigon"
chr <- read.table("~/Desktop/Ongoing/Pred/fastafiles/PREV1.genome.v1.0.chr", header=FALSE)
colnames(chr) <- c("chr","chr_length")

bed <- data.frame(
  chr   = orthologs_2sp_extend$sp1.Chr,
  start = orthologs_2sp_extend$sp1.Start,
  end   = orthologs_2sp_extend$sp1.End,
  gene  = orthologs_2sp_extend$sp1.ID,
  color = orthologs_2sp_extend$Nigon
)
colnames(bed) <- c("chr","start","end","gene","color")

# make sure the order of chromosome color
chr$chr <- factor(chr$chr, levels=chr$chr)
bed$chr <- factor(bed$chr, levels=chr$chr)

ggplot() +
  
  # 1. chromsome backbone
  geom_segment(
    data=chr,
    aes(x=0, xend=chr_length, y=chr, yend=chr),
    linewidth=6,
    color="grey80"
  ) +
  
  # 2. genes
  geom_segment(
    data=bed,
    aes(x=start, xend=end+20000, 
        y=as.numeric(chr), 
        yend=as.numeric(chr),, color=color),
    linewidth=6
  ) +

  theme_classic() +
  labs(x="Genomic position", y="Chromosome")

