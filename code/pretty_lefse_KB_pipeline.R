library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(stringr)

#######
bkg <-
  theme(axis.text.x = element_text(size = 24, color = "black")) +
  theme(axis.text.x = element_text(angle=0, vjust = 0.5, hjust = 1)) +
  theme(axis.title.x = element_text(margin = unit(c(0,0,4,0), "mm"))) +
  theme(axis.title.x = element_text(size = 24, color = "black")) +
  theme(axis.text.y = element_text(size = 24, color = "black")) +
  theme(axis.title.y = element_text(size = 24, color = "black")) +
  theme(axis.title.y = element_text(margin = unit(c(0,4,0,0), "mm"))) +
  theme(legend.position = "right") +
  theme(legend.title = element_text(size = 24)) +
  theme(legend.text = element_text(size = 24)) +
  theme(legend.key.size = unit(0.6, 'cm')) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(size=, color= "black", face ="bold")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(size=24, color= "black"))



### all data
aimds = c('RA','PsO','PsA','SjD','SLE','NSS','Affected')
jobs = c('jobs09','jobs10','jobs11','jobs12','jobs13','jobs14','jobs16')
levels = list(c('healthy', 'RA'),
              c('healthy', 'PsO'),
              c('healthy', 'PsA'),
              c('healthy', 'SjD'),
              c('healthy', 'SLE'),
              c('healthy', 'NSS'),
              c('healthy', 'Affected')
              )

library(RColorBrewer)

col1 <- colorRampPalette(brewer.pal(9, "Paired"))(7)
# "black"   "#4F9AA6" "#5DB54B" "#FB9A99" "#EB5037" "#FE9425" "#CAB2D6"
col1[1] <- "#5A5A5A"

# order factors for legend
# dx.order = c("Healthy", "RA", "PsA", "PsO", "SLE", "SjD", "NSS")


gc = list(c(healthy = col1[1], RA = col1[2]), # c(healthy = "#B3A98C", RA = "#E69F00"),
          c(healthy = col1[1], PsO = col1[4]),
          c(healthy = col1[1], PsA = col1[3]),
          c(healthy = col1[1], ss = col1[6]),
          c(healthy = col1[1], sle = col1[5]),
          c(healthy = col1[1], nss = col1[7]),
          c(healthy = col1[1], affected = "#E69F00")
)
slices = list(c(),
              c(),
              c(),
              c(),
              c(),#3,7),
              c(),#4,9),
              c())#NA,NA))

# make list of lists for UpSetR
healthy <- vector("list", length=7) 
disease <- vector("list", length=7)


### statistics ###
for (k in 1:length(aimds)){
  aimd = aimds[k]
  jobn = jobs[k]
  level = levels[[k]]
  group.colors = gc[[k]]

  
  ### MSQ138+141
  data <-read.table(paste0("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/",jobn,"/lefse_results.res"), header = FALSE, sep = "\t")
  
  # set names
  names(data) <- c("RawTaxa", "X", "Group", "LDA", "pval")
  plot_data <- subset(data, !is.na(data$LDA))
  taxa_strs <- list()
  for (raw in plot_data$RawTaxa) {
    split <- as.character(unlist(str_split(raw, "\\.")))
    i <- length(split)
    blanks <- 0
    while (i > 0) {
      if (split[i] == "__") {
        blanks <- blanks + 1
        print(split)
        split <- split[1:i-1]
        print(split)
      }        
      else {
        break
      }
      i <- i - 1
    }
    
    if (length(split) == 1) {
      taxa_str <- split[1]
    }
    else {
      taxa_str <- paste(split[length(split)-1], split[length(split)])
    }
    if (blanks > 0) {
      for (i in 1:blanks) {
        taxa_str <- paste(taxa_str, "__uncl.", sep="")
      }
    }
    taxa_strs <- append(taxa_strs, taxa_str)
  }
  plot_data$Taxa <- as.character(taxa_strs)
  
  # exclude non family results
  plot_data <- plot_data[grep("f__", plot_data$Taxa),]
  
  # exclude uncl
  plot_data <- plot_data[grep("uncl", plot_data$Taxa, invert=T),]
  
  # take everything before trailing __
  # plot_data$Taxa <- sub('__*.', '', plot_data$Taxa)
  # plot_data$Taxa <- lapply(str_split(plot_data$Taxa, '__')[-1])
  # plot_data$Taxa <- sub("__.*", "", plot_data$Taxa)
  # plot_data$Taxa <- sub("__[^__]*$", '', plot_data$Taxa)
  
  # take everything after the last __
  plot_data$Taxa <- sub('.*__', '', plot_data$Taxa)
  
  # drop empty taxa strings
  plot_data <- plot_data[!(is.na(plot_data$Taxa) | plot_data$Taxa==""), ]
  
  # string specific substitutions
  plot_data$Taxa <- str_replace_all(plot_data$Taxa, 'Phascolarctobacterium_A', 'Phascolarctobacterium')
  plot_data$Taxa <- str_replace_all(plot_data$Taxa, 'Clostridium_AQ', 'Clostridium')
  #plot_data$Taxa <- str_replace_all(plot_data$Taxa, 'Clostridium_Q_134516', 'Clostridium')
  plot_data$Taxa <- str_replace_all(plot_data$Taxa, 'Coprococcus_A_121497', 'Coprococcus')
  plot_data$Taxa <- str_replace_all(plot_data$Taxa, 'Eubacterium_R', 'Eubacterium')
  plot_data$Taxa <- str_replace_all(plot_data$Taxa, 'Pseudoruminococcus_A', 'Psuedoruminoccocus')
  plot_data$Taxa <- str_replace_all(plot_data$Taxa, 'Adlercreutzia_404257', 'Adlercreutzia')
  plot_data$Taxa <- str_replace_all(plot_data$Taxa, 'Anaerotignum_189125', 'Anaerotignum')
  plot_data$Taxa <- str_replace_all(plot_data$Taxa, 'Alistipes_A_871404', 'Alistipes')
  
  
  # remove duplicates in Taxa
  plot_data <- plot_data[!duplicated(plot_data$Taxa),]
  
  # remove taxa on this specific list
  # butyrate producers; eubacterium, agathobaculum, acidaminococacceae, anaerostipes, phasco, coprococcus
  plot_data <- subset(plot_data, !(Taxa %in% c(
    'QALR01', 'Frisingicoccus', 'SFMI01', 'CAG_914', 'CAG_272', 'CAG_41', 'CAG_74', 'CAG_83', 'UBA660',
    'CAG_914', 'PeH17', 'UBA2658', 'COE1', 'WQUU01', 'UBA7173', 'CAG_475', 'CAG_917',
    'UBA2658', 'WQUU01', 'COE1', 'G11', 'UBA1381', 'CAG_353', 'UBA5905','Clostridium_Q_135822',
    'Onthenecus','Butyricicoccaceae','Paramuribaculum','Muribaculaceae', 'Clostridium_Q_134516',
    'Mediterraneibacter_A_155507', 'Bariatricus','Desulfovibrio_R_446353',
    'Tidjanibacter', # IBS apparently
    'Desulfovibrionaceae')
  ))
  # grab healthy and disease
  healthy_data = subset(plot_data, (Group %in% c('healthy')))
  healthy_taxa = healthy_data$Taxa
  healthy[[i]] <- healthy_taxa
  
  disease_data = subset(plot_data, (Group %in% c(aimd)))
  disease_taxa = disease_data$Taxa
  if (aimd == 'RA'){
    print(aimd)
    print(disease_data)
    print(disease_taxa)
  }
  disease[[i]] <- disease_taxa
  
  # sort by LDA magnitude 
  plot_data <- plot_data[order(plot_data$LDA,decreasing=TRUE),]
  
  # keep first 5 duplicates (top values) for each group
  plot_data <- plot_data %>%
               group_by(Group) %>%
               filter(row_number() <= 5)
  
  # flip directionality for healthy
  #plot_data$Taxa <- sub(".*_s__", '', plot_data$RawTaxa)
  plot_data[plot_data$Group == "healthy",]$LDA <- -1 * plot_data[plot_data$Group == "healthy",]$LDA
  
  # save results
  write.csv(plot_data,paste0("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/lefse_csv_", aimd, '.csv'), row.names = FALSE)
  
  # save pdf
  pdf(paste0("/Users/KevinBu/Desktop/clemente_lab/Projects/ampaim/outputs/jobs27/lefse_barplot_pretty_",aimd,'.pdf'), width=10, height=6)

  p <- ggbarplot(plot_data, x="Taxa", y="LDA", fill="Group", width= 1, color = "white", sort.val = "asc", sort.by.groups=TRUE) +  
    labs(x = "", y = "LDA score", fill="Group") + coord_flip() + 
    scale_fill_manual(values=group.colors) + bkg + theme(legend.position = "none")
  plot(p)
  dev.off()
}