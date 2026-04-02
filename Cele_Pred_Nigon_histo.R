library(dplyr)
library(ggplot2)

chr <- read.table("/Users/sunsimo/Desktop/Ongoing/Pred/fastafiles/PREV1.genome.v1.0.chr", header=FALSE)
colnames(chr) <- c("chr","chr_length")

colorsNigon <- c(
  "A"="#E41A1C",
  "B"="#377EB8",
  "C"="#4DAF4A",
  "D"="#984EA3",
  "E"="#FF7F00",
  "N"="#FFFF33",
  "X"="#A65628"
)
bed <- data.frame(
  chr   = orthologs_2sp_extend$sp1.Chr,
  start = orthologs_2sp_extend$sp1.Start,
  end   = orthologs_2sp_extend$sp1.End,
  gene  = orthologs_2sp_extend$sp1.ID,
  color = orthologs_2sp_extend$Nigon
)
colnames(bed) <- c("chr","start","end","gene","color")

window_size <- 1e6   # 1 Mbp
bed$window <- floor(bed$start / window_size) * window_size
bed_win <- as.data.frame(
  table(bed$chr, bed$window, bed$color)
)
colnames(bed_win) <- c("chr","window","color","count")
bed_win_withoutMt <- bed_win[(bed_win$color != "MtDNA" & bed_win$chr != "MtDNA"), ]
ggplot(bed_win_withoutMt,
       aes(x=as.numeric(as.character(window))/1e6,
           y=count,
           fill=color)) +
  
  geom_col(width=window_size/1e6) +
  
  facet_wrap(~chr, scales="free_y", nrow=1) +
  
  scale_fill_manual(values=colorsNigon) +
  
  labs(
    x="Genomic position",
    y="Gene count / 1 Mbp",
    fill="color"
  ) +
  
  theme_classic()

