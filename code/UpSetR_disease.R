################################# 
## R script                    ##
## Project: AMP AIM            ##
## 16S                         ##
## UpSetR disease overlap      ##
## Author: Kevin Bu            ##
## Last Updated: 7/16/24       ##
#################################

# install library, packages
library(UpSetR)
library(RColorBrewer)

# choose colors
ncol = 7
col2 <- colorRampPalette(brewer.pal(9, "Set2"))(ncol)[2:7]
col2 <- colorRampPalette(brewer.pal(9, "Paired"))(ncol)
# dx.order = c("Healthy", "RA", "PsA", "PsO", "SLE", "SS", "NSS")
#col1 <- c("#E99073" "#AB98C8" "#C6B18B" "#E1D83B" "#E9C783" "#B3B3B3")
#col1 <- c("#B3B3B3", "#E1D83B", "#E9C783", "#C6B18B", "#AB98C8", "#E99073")
col2 <- colorRampPalette(brewer.pal(9, "Paired"))(ncol)

# the order is 
# dx.order = c("Healthy", "RA", "PsA", "PsO", "SLE", "SS", "NSS")

# the order we want is bottom up NSS SLE SS PsO PsA RA 

col1 <- c(col2[7], col2[5], col2[6], col2[4], col2[3], col2[2])


# example of list input (list of named vectors)
listInput <- list(
  RA = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/RA_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  #PS = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/PS_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  #A = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/A_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  #RAPsA = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/RAPsA_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  SjD = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/SS_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  #SSSLE = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/SSSLE_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  NSS = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/NSS_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  SLE = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/SLE_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  PsA = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/PsA_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]],
  PsO = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/PsO_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]]
  # CD = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs04/CD_lefse_sig.tsv', sep='\t', header=TRUE)$Taxa)[[1]]
)

# set_config(upset(fromList(listInput)), yticks = c(0, 1, 2))

pdf(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/LEfSe_UpSetR_disease.pdf',
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
      # set_size.show=T,
      mainbar.y.max=15,
      show.numbers=T)
dev.off()

