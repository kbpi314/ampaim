# install library, packages
library(UpSetR)

# example of list input (list of named vectors)
listInput <- list(
  RA = list(read_delim(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/RA_lefse_sig.tsv', delim='\t')$Taxa)[[1]],
  PS = list(read_delim(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/PS_lefse_sig.tsv', delim='\t')$Taxa)[[1]],
  A = list(read_delim(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/A_lefse_sig.tsv', delim='\t')$Taxa)[[1]],
  RAPsA = list(read_delim(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/RAPsA_lefse_sig.tsv', delim='\t')$Taxa)[[1]],
  SS = list(read_delim(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/SLE_lefse_sig.tsv', delim='\t')$Taxa)[[1]],
  SSSLE = list(read_delim(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/SSSLE_lefse_sig.tsv', delim='\t')$Taxa)[[1]],
  SLE = list(read_delim(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/SS_lefse_sig.tsv', delim='\t')$Taxa)[[1]],
  PsA = list(read_delim(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/PsA_lefse_sig.tsv', delim='\t')$Taxa)[[1]],
  PsO = list(read_delim(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/PsO_lefse_sig.tsv', delim='\t')$Taxa)[[1]],
  CD = list(read_delim(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/CD_lefse_sig.tsv', delim='\t')$Taxa)[[1]]
)

# example of expression input
expressionInput <- c(one = 2, 
                     two = 1, 
                     three = 2,
                     `one&two` = 1, 
                     `one&three` = 4, 
                     `two&three` = 1, 
                     `one&two&three` = 2)

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

