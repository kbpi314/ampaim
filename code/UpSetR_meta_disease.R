################################# 
## R script                    ##
## Project: AMP AIM            ##
## 16S                         ##
## UpSetR healthy overlap      ##
## Author: Kevin Bu            ##
## Last Updated: 7/16/24       ##
#################################

# install library, packages
library(UpSetR)
library(dplyr)
library(RColorBrewer)

# choose colors
ncol = 7
# col1 <- c("#AB98C8", "#C6B18B", "#E99073", "#E9C783", "#B3B3B3", "#E1D83B")
col2 <- colorRampPalette(brewer.pal(9, "Paired"))(ncol)
# dx.order = c("Healthy", "RA", "PsA", "PsO", "SLE", "SS", "NSS")


# FIRST SET

# the order we want is bottom up RA RA PsA PsA SLE SLE
col1 <- c(#col2[2], col2[2],
          col2[3], col2[3], 
          col2[5], col2[5], 
          col2[4], col2[4], 
          col2[6], col2[6])

# example of list input (list of named vectors)
listInput <- list(
  #AMPAIM.PsA_AIMD = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/AMPAIM_PsA_PsA.tsv', sep='\t',header=TRUE)$Taxa)[[1]],
  #Gill2022.AxSpA_AIMD = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/Gill2022_AxSpA_axial spondyloarthritis.tsv', sep='\t',header=TRUE)$Taxa)[[1]],

  AMPAIM.SLE_AIMD = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/AMPAIM_SLE_sle.tsv', sep='\t',header=TRUE)$Taxa)[[1]],
  Su2020.SLE_AIMD = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/Su2020_SLE_SLE-G.tsv', sep='\t',header=TRUE)$Taxa)[[1]],
    
  AMPAIM.RA_AIMD = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/AMPAIM_RA_RA.tsv', sep='\t',header=TRUE)$Taxa)[[1]],
  Yu2022.RA_AIMD = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/Yu2022_RA_RA.tsv', sep='\t',header=TRUE)$Taxa)[[1]],

  AMPAIM.PsO_AIMD = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/AMPAIM_PsO_PsO.tsv', sep='\t',header=TRUE)$Taxa)[[1]],
  Luca2024.PsO_AIMD = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/Luca2024_PsO_PsO.tsv', sep='\t',header=TRUE)$Taxa)[[1]],
  
  AMPAIM.SjD_AIMD = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/AMPAIM_SjD_ss.tsv', sep='\t',header=TRUE)$Taxa)[[1]],
  Wang2022.SjD_AIMD = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/Wang2022_SjD_SjD.tsv', sep='\t',header=TRUE)$Taxa)[[1]]
  
)

# thank you kind person
# https://stackoverflow.com/questions/65027133/extract-intersection-list-from-upset-object
df2 <- data.frame(gene=unique(unlist(listInput)))

df1 <- lapply(listInput,function(x){
  data.frame(gene = x)
}) %>% 
  bind_rows(.id = "path")

df_int <- lapply(df2$gene,function(x){
  # pull the name of the intersections
  intersection <- df1 %>% 
    dplyr::filter(gene==x) %>% 
    arrange(path) %>% 
    pull("path") %>% 
    paste0(collapse = "|")
  
  # build the dataframe
  data.frame(gene = x,int = intersection)
}) %>% 
  bind_rows()


df_int %>% 
  group_by(int) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

pdf(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/LEfSe_UpSetR_AIMD.pdf',
    width=8,
    height=8)
upset(fromList(listInput),#fromExpression(input), 
      order.by = "degree",
      nsets=20, # 5 is default here
      text.scale = c(3,2.5,1,# c(intersection size title, intersection size tick labels, set size title, 
                     1.25,2,1),  #set size tick labels, set names, numbers above bars)
      # main.bar.color = "gray23", 
      sets.bar.color = "gray23",
      #sets.bar.color =col1,
      keep.order=F,
      nintersects=10,
      # set_size.show=T,
      #mainbar.y.max=15,
      show.numbers=T)
dev.off()


# query indiv sets
df <- df_int[df_int$int == 'AMPAIM.RA_AIMD|Yu2022.RA_AIMD',]
df <- df_int[df_int$int == 'AMPAIM.SjD_AIMD|Su2020.SLE_AIMD|Wang2022.SjD_AIMD|Yu2022.RA_AIMD',]

df <- df_int[df_int$int == 'Luca2024.PsO_AIMD|Su2020.SLE_AIMD|Wang2022.SjD_AIMD|Yu2022.RA_AIMD', ]
df$gene


