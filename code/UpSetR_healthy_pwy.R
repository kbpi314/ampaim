# install library, packages
library(UpSetR)

ncol = 7
col2 <- colorRampPalette(brewer.pal(9, "Set2"))(ncol)[2:7]
# dx.order = c("Healthy", "RA", "PsA", "PsO", "SLE", "SS", "NSS")
# col1 <- c("#E99073" "#AB98C8" "#C6B18B" "#E1D83B" "#E9C783" "#B3B3B3")
# col1 <- c("#B3B3B3", "#E1D83B", "#E9C783", "#C6B18B", "#AB98C8", "#E99073")
#bottom up PsA NSS PsO SS RA SLE
col1 <- c(col2[2], col2[6], col2[3], col2[5], col2[1], col2[4])



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
      text.scale = c(3,2.5,1,# c(intersection size title, intersection size tick labels, set size title, 
                     1.25,2,1),  #set size tick labels, set names, numbers above bars)main.bar.color = "gray23", 
      sets.bar.color = col1,
      keep.order=T,
      set_size.show=T,
      mainbar.y.max=15,
      show.numbers=T)
dev.off()

