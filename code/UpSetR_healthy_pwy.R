# install library, packages
library(UpSetR)

# example of list input (list of named vectors)
listInput <- list(
  RA = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs06/UpSetR/RA_healthy_sig.tsv', sep='\t',header=TRUE)$Pathway)[[1]],
  SS = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs06/UpSetR/SS_healthy_sig.tsv', sep='\t',header=TRUE)$Pathway)[[1]],
  NSS = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs06/UpSetR/NSS_healthy_sig.tsv', sep='\t',header=TRUE)$Pathway)[[1]],
  SLE = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs06/UpSetR/SLE_healthy_sig.tsv', sep='\t',header=TRUE)$Pathway)[[1]],
  PsA = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs06/UpSetR/PsA_healthy_sig.tsv', sep='\t',header=TRUE)$Pathway)[[1]],
  PsO = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs06/UpSetR/PsO_healthy_sig.tsv', sep='\t',header=TRUE)$Pathway)[[1]]
)


pdf(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs06/UpSetR/UpSetR_healthy.pdf',
    width=6,
    height=6)
upset(fromList(listInput),#fromExpression(input), 
      order.by = "degree",
      nintersects= NA,
      nsets=20, # 5 is default here
      #text.scale = c(2.5,2.5,1.25,1.25,2,1), 
      #sets=c('RA','A','CD','PS'),
      keep.order=F,
      set_size.show=T,
      show.numbers=T)
dev.off()

