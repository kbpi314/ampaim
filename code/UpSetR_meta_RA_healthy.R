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

# the order we want is bottom up RA RA PsA PsA SLE SLE SjD SjD
col1 <- c(#col2[2], col2[2],
          col2[3], col2[3], 
          col2[5], col2[5], col2[5], 
          col2[4], col2[4],  col2[4], 
          col2[6], col2[6])

# example of list input (list of named vectors)
listInput <- list(
  #AMPAIM.PsA_HC = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/AMPAIM_PsA_healthy.tsv', sep='\t',header=TRUE)$Taxa)[[1]],
  #Gill2022.AxSpA_HC = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/Gill2022_AxSpA_healthy control.tsv', sep='\t',header=TRUE)$Taxa)[[1]],

  AMPAIM.RA_HC = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/AMPAIM_RA_healthy.tsv', sep='\t',header=TRUE)$Taxa)[[1]],
  Valid4RA.RA_HC = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/Valid4RA_RA_HC.tsv', sep='\t',header=TRUE)$Taxa)[[1]],
  Valid7RA.RA_HC = list(read.table(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/Valid7RA_RA_HLT.tsv', sep='\t',header=TRUE)$Taxa)[[1]]
  

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

pdf(file='/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs100/LEfSe_UpSetR_RA_healthy.pdf',
    width=8,
    height=8)
upset(fromList(listInput),#fromExpression(input), 
      order.by = "degree",
      # nintersects= NA,
      nsets=20, # 5 is default here
      text.scale = c(3,2.5,1,# c(intersection size title, intersection size tick labels, set size title, 
                     1.25,2,3),  #set size tick labels, set names, numbers above bars)
      # main.bar.color = "gray23", 
      # sets.bar.color =col1,
      keep.order=F,
      sets.bar.color = "gray23",
      #sets.bar.color =col1,
      nintersects=26,
      show.numbers="yes")
dev.off()

# query indiv sets
df<-df_int[df_int$int == 'AMPAIM.RA_HC|Valid4RA.RA_HC',]

df<-df_int[df_int$int == 'AMPAIM.RA_HC|Valid7RA.RA_HC',]
df$gene
