# install library, packages
library(UpSetR)

# example of list input (list of named vectors)
listInput <- list(
  RA = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/RA_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  #PS = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/PS_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  #A = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/A_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  #RAPsA = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/RAPsA_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  SS = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/SS_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  #SSSLE = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/SSSLE_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  SLE = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/SLE_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  PsA = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/PsA_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  PsO = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/PsO_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  CD = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/CD_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]]
)

pdf(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/LEfSe_UpSetR_disease.pdf',
    width=6,
    height=6)
upset(fromList(listInput),#fromExpression(input), 
      order.by = "degree",
      nintersects= NA,
      nsets=20, # 5 is default here
      #text.scale = c(2.5,2.5,1.25,1.25,2,1), 
      #sets=c('RA','A','CD','PS'),
      keep.order=T,
      set_size.show=T,
      show.numbers=T)
dev.off()

